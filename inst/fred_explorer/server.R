# EXPLORER ----------------------------------------------------------------------

server <- function(input, output, session) {

  session$onSessionEnded(function() {
    if (Sys.getenv("SHINY_FRED_AUTOCLOSE") == "TRUE") {
      message("App has ended because the session was ended.")
      stopApp()
    }
  })



  # Disclaimer --------------------------------------------------------------

  showModal(modalDialog(
    title = welcome_title,
    welcome_text,
    size = "l",
    easyClose = TRUE
  ))

  # Overview Table ----------------------------------------------------------

  df_temp <- reactiveVal()

  min_power_debounced <- reactive({input$minpower}) %>%
     debounce(500)

  # Update df_temp based on filters
  observe({
    df_temp <- df[rev(row.names(df)), ]

    # source
    if (input$source == "All studies") {
      df_temp <- df_temp
    } else {
      df_temp <- df_temp[df_temp$source == input$source, ]
    }

    # minpower
    if (min_power_debounced() == .05) {
      df_temp <- df_temp
    } else {
      df_temp <- df_temp[df_temp$power_r >= min_power_debounced(), ]
    }

    # validated
    if (input$validated == TRUE) {
      df_temp <- df_temp[!is.na(df_temp$validated), ]
    }

    # only show show coded entries?
    if (input$codedentries == TRUE) {
      df_temp <- df_temp[!is.na(df_temp$result), ]
    }
    df_temp(df_temp)
  })

  outcome_colors <- reactive({
    criterion_colors <- success_criteria_colors %>%
      dplyr::filter(criterion == input$success_criterion)

    outcome_colors <- setNames(criterion_colors$color, criterion_colors$label %>% cap_first_letter())
    c(outcome_colors, "Not coded" = "#C8C8C8")
  })

  # Recalculate replication success based on criterion
  assess_success <- function(result, success_criterion) {
    assess_replication_outcome(result$es_original, result$n_original, result$es_replication, result$n_replication,
                               criterion = success_criterion)$outcome_report
  }

  observeEvent({input$success_criterion; df_temp()}, {
    if (nrow(df_temp()) > 1) {
      updated_df <- df_temp() %>%
        arrange(ref_original) %>%
        filter(if (input$validated == "TRUE") validated == 1 else TRUE) %>%
        mutate(
          result = assess_success(., input$success_criterion) %>% cap_first_letter(),
          result = factor(
            result,
            levels = rev(c(
              "Not coded",
              unique(result[grepl("Success", result, ignore.case = TRUE)]),
              unique(result[grepl("Failure", result, ignore.case = TRUE)]),
              unique(result[!(result %in% c("Not coded", "OS not significant")) &
                              !grepl("Success|Failure", result, ignore.case = TRUE)]),
              "OS not significant"
            ))
          )
        )

      df_temp(updated_df)
    }
  }, ignoreNULL = TRUE)


  df_temp_DT <- reactive({
    df_temp()[input$table_rows_all, ] # rows on all pages (after being filtered)
  })

  debounced_search <- reactive({
    input$sidebar_search
  }) %>% debounce(500)


  output$table <- DT::renderDT(server = FALSE, {
    ## apply filters
    df_temp <- df_temp()

    df_temp[is.na(df_temp$result), "result"] <- "Not coded"

    df_temp <- df_temp %>% dplyr::rename("LeBel result" = result2)

    df_temp_filtered <- df_temp[, c(
      "description", "tags", "contributors"
      # , "es_original", "es_replication"
      , "result", "ref_original", "ref_replication"
    )]

    DT::datatable(
      df_temp_filtered,
      extensions = "Buttons",
      selection = "multiple",
      options = list(
        scrollX = TRUE,
        dom = "Brtip",
        buttons = c("copy", "csv", "excel"),
        pageLength = 5,
      search = list(
        search = debounced_search() # Link to the debounced sidebar search
      )
      ),
      rownames = FALSE
      # , options = list(pageLength = 5)
    )
  })


  # Overview Plot -----------------------------------------------------------

  output$success_note <- renderUI({
    tags$div(
      HTML(markdown::markdownToHTML(text = paste0("*Note:* ", success_criterion_note[input$success_criterion]), fragment.only = TRUE))
    )
  })

  output$overviewplot <- plotly::renderPlotly({
    ## apply filters
    df_temp <- df_temp_DT()

    validate(need(nrow(df_temp) > 0, "Plot cannot be created if no studies are selected"))

    df_temp$scatterplotdescription <- paste(df_temp$description, "\nr(original) = ",
      round(df_temp$es_original, 3),
      ", r(replication) = ",
      round(df_temp$es_replication, 3),
      sep = ""
    )


    pointsize <- ifelse(nrow(df_temp) < 10, 5, ifelse(nrow(df_temp) < 100, 4, 3))

    s3 <- input$table_rows_selected

    df_temp$significant_original <- c("Not significant", "Significant")[(df_temp$p_value_original < .05) + 1] %>%
      factor( levels = c("Not significant", "Significant"))
    df_temp$significant_replication <- c("Not significant", "Significant")[(df_temp$p_value_replication < .05) + 1] %>%
      factor(levels = c("Not significant", "Significant"))

    df_temp$result <- factor(df_temp$result, levels = names(outcome_colors()))

    scatterplot <-
      ggplot(df_temp, aes(x = es_original, y = es_replication, text = scatterplotdescription)) +
      geom_hline(aes(yintercept = 0), linetype = 2) +
      geom_abline(intercept = 0, slope = 1, color = "Grey60") +
      geom_point(aes(fill = result), size = pointsize, color = "Grey30", shape = 21, alpha = .8) +
      scale_fill_manual(values = outcome_colors(), drop = FALSE) +
      # highlighted studies
      # geom_point(data = df_temp[s3, ], mapping = aes(size = power), fill= "Grey30",color="Grey30",shape=4) +
      geom_point(data = df_temp[s3, ], fill = "#0077d9", color = "#f2ef1b", shape = 4) +
      # Rugs colored manually as plotly legend turns messy otherwise
      geom_rug(data = subset(df_temp, significant_original == "Significant"),
               color = "#4DCCD0", linewidth = 1, sides = "b", alpha = .6) +
      geom_rug(data = subset(df_temp, significant_original == "Not significant"),
               color = "#FA948C", linewidth = 1, sides = "b", alpha = .6) +
      geom_rug(data = subset(df_temp, significant_replication == "Significant"),
               color = "#4DCCD0", linewidth = 1, sides = "l", alpha = .6) +
      geom_rug(data = subset(df_temp, significant_replication == "Not significant"),
               color = "#FA948C", linewidth = 1, sides = "l", alpha = .6) +
      scale_x_continuous(name = "Original Effect Size", limits = c(0, 1), breaks = c(0, .25, .5, .75, 1)) +
      scale_y_continuous(name = "Replication Effect Size", limits = c(-.5, 1), breaks = c(-.5, -.25, 0, .25, .5, .75, 1)) +
      # ggtitle("") + #xlab("") + ylab("") +
      # scale_size_continuous(name="Power",range=c(.5,3.5)) +
      theme_bw() +
      # theme(legend.position=c(.9,.6), plot.margin = unit(c(-2,-1.5,2,2), "lines")) +
      theme(legend.position = "top") +
      labs(fill = "Outcome")


    plotly::ggplotly(scatterplot, tooltip = "text") %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  }) # , height = 800





  # Forest Plot -------------------------------------------------------------


  plotHeight <- reactive({

    df_temp <- df_temp_DT()

    return(length(unique(df_temp$ref_original)) * 100)
  })

  output$forestplot <- plotly::renderPlotly({

    df_temp <- df_temp_DT()
    df_temp <- df_temp[rev(row.names(df_temp)), ]

    # use only studies with a replication effect size
    df_temp <- df_temp[!is.na(df_temp$es_replication), ]

    # use only studies with a reference for the original finding
    df_temp <- df_temp[!is.na(df_temp$ref_original), ]

    # make descriptions shorter
    df_temp$description <- gsub("(.{70,}?)\\s", "\\1\n", df_temp$description) # line breaks

    # make reference shorter
    df_temp$ref_original <- gsub("(.{70,}?)\\s", "\\1\n", df_temp$ref_original) # line breaks

    red_temp_selected <- df_temp

    xlims <- seq(from = -1, 1, .25)

    df_temp$description <- factor(df_temp$description, levels = unique(df_temp$description[order(df_temp$es_replication)]))

    forest <- ggplot(data = df_temp, aes(x = es_replication, y = ref_original)) +
      geom_vline(xintercept = 0, col = "dark grey", lwd = 1) +
      # Replication effect sizes
      geom_point() +
      geom_errorbar(aes(xmin = ci.lower_replication, xmax = ci.upper_replication)) +

      # Original effect sizes
      geom_point(aes(x = es_original, y = ref_original), color = "dark grey", alpha = .5) +
      geom_errorbar(aes(xmin = ci.lower_original, xmax = ci.upper_original), color = "dark grey") +

      # highlighted studies
     # geom_point(data = red_temp_selected, aes(x = es_replication, y = ref_original), color = ifelse(nrow(df_temp) == nrow(red_temp_selected), "black", "df")) +

      # Theme and formatting
      theme_classic() +
      geom_vline(xintercept = xlims, col = rgb(0, 0, 0, .05), lwd = 0.5, lty = 1) +
      theme(text = element_text(size = 14)) +
      xlim(c(floor(min(df_temp$ci.lower_original)), ceiling(max(df_temp$ci.upper_original, na.rm = TRUE)))) +
      xlab("r") +
      ylab("") +
      theme(legend.position = "none") +
      theme(text = element_text(size = 10)) +
      scale_y_discrete(limits = rev) +
      ggtitle(paste(
        "Blobbogram\n",
        sum(!is.na(df_temp$es_original)),
        "Effect sizes selected.\nGrey dots represent original effect sizes. Black dots represent replication effect sizes."
        # , length(unique(df_temp$ref_original))
        # , "Original studies were examined in replication studies."
      ))

    p <- plotly::ggplotly(forest) %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE), width = 1200) # , height = plotHeight

  })


  output$forestplot_ui <- renderUI({
    plotlyOutput("forestplot", height = plotHeight())
  })


  # Bar Plot -------------------------------------------------------------

  output$barplot <- plotly::renderPlotly({

    df_temp <- df_temp_DT()

    df_temp[is.na(df_temp$result), "result"] <- "Not coded"

    validate(need(nrow(df_temp) > 0, "Plot cannot be created if no studies are selected"))

    # Calculate proportions - separately to enable tooltip text
    df_summary <- df_temp %>%
      group_by(result) %>%
      summarise(count = n()) %>%
      mutate(Proportion = count / sum(count),
             text = paste(result, ":\n", round(Proportion * 100, 1), "%"))

    barchart <- ggplot(df_summary, aes(x = "", fill = result, y = Proportion, text = text)) +
      geom_bar(stat = "identity", position = "fill") +
      theme_bw() +
      ylab("Percentage") +
      xlab("") +
      coord_flip() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_fill_manual("Result", values = outcome_colors()) +
      ggtitle(paste(nrow(df_temp), "studies selected."))

    plotly::ggplotly(barchart, tooltip = "text") %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  })

  # # Barplot Result2 ---------------------------------------------------------
  #
  # output$barplot2 <- plotly::renderPlotly({
  #   ## apply filters
  #   df_temp <- df_temp_DT()
  #
  #   ## Exclude NAs
  #   df_temp <- df_temp[!is.na(df_temp$result2), ]
  #
  #   validate(need(nrow(df_temp) > 0, "Plot cannot be created if no studies are selected"))
  #
  #   df_summary <- df_temp %>%
  #     group_by(result2) %>%
  #     summarise(count = n()) %>%
  #     mutate(Proportion = count / sum(count),
  #            text = paste(result2, ": ", round(Proportion * 100, 1), "%"))
  #
  #   # Create the barchart with tooltip text
  #   barchart <- ggplot(df_summary, aes(x = "", fill = result2, y = Proportion, text = text)) +
  #     geom_bar(stat = "identity", position = "dodge") +
  #     theme_bw() +
  #     ylab("Percentage") +
  #     xlab("") +
  #     coord_flip() +
  #     scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  #     scale_fill_manual("Result", values = c(
  #       "no signal - inconsistent" = "#9c0505",
  #       "signal - consistent" = "#05e361",
  #       "no signal - OS n.s." = "grey",
  #       "NA - OS n.s." = "grey",
  #       "signal - inconsistent, smaller" = "#a4d11b",
  #       "signal - inconsistent, larger" = "#77bd06",
  #       "signal - OS n.s." = "grey",
  #       "no signal - consistent" = "#b4d4a5",
  #       "not coded" = "grey"
  #     )) +
  #     ggtitle(paste(nrow(df_temp), "of", nrow(df_temp_DT()), "studies selected."))
  #
  #   plotly::ggplotly(barchart, tooltip = "text") %>%
  #     plotly::config(displayModeBar = FALSE) %>%
  #     plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  # })



  # Z-Curve -----------------------------------------------------------------

  output$zcurve_plot <- shiny::renderPlot({
    ## apply filters
    df_temp <- df_temp_DT()

    # use only studies with effect size data
    df_temp <- df_temp[!is.na(df_temp$z), ]

    validate(need(nrow(df_temp) > 0, "Plot cannot be created if no studies are selected"))

    # # create zcurvedata object
    # zcurvedata_clustered <- data.frame("input" = NA, "p" = (1-pnorm(abs(df_temp$z)))*2, "id" = paste(df_temp$refonly_original, df_temp$study_original, sep = "_"))
    # zcurvedata_clustered <- zcurvedata_clustered[!is.na(zcurvedata_clustered$p), ]
    # zcurvedata_clustered_list <- list("precise" = zcurvedata_clustered, "censored" = data.frame("input" = as.character()
    #                                                                                             , "p.lb" =  as.logical()
    #                                                                                             , "p.ub" =  as.logical()
    #                                                                                             , "p.rep" = as.logical()
    #                                                                                             , "id" =    as.logical()))
    # class(zcurvedata_clustered_list) <- "zcurve_data"
    # # run z-curve analysis (clustered)
    # zc <- zcurve::zcurve_clustered(data = zcurvedata_clustered_list, method = "w", bootstrap = FALSE) # clustered (new version), requires bootstrap

    # run z-curve analysis
    zc <- zcurve::zcurve(z = df_temp$z, method = "EM", bootstrap = 0) # non clustered (old version)

    orr <- round(mean(df_temp$result == "success", na.rm = TRUE), 2)
    err <- zc$coefficients[1]

    # create plot
    zcurve::plot.zcurve(zc, annotation = TRUE, CI = TRUE
                        , main = ""
    #                     , main = paste("Observed Replication Rate: ", orr,
    #   # "\nCorrected ERR: ", round(1.85*err-0.573, digits = 2),
    #   sep = ""
    # )
    )
  })






  # Dataset -----------------------------------------------------------------



  output$dataset <- DT::renderDT(server = FALSE,
                                 DT::datatable(df_display,
    rownames = FALSE,
    # extensions = 'Buttons',
    options = list(scrollX=TRUE, lengthMenu = c(5, 10, 15),
                   paging = TRUE, searching = TRUE,
                   fixedColumns = TRUE, autoWidth = TRUE,
                   ordering = TRUE
                   # , dom = 'Bfrtip'
                   # buttons = list(list(extend = 'copy'),
                   #                list(extend = 'excel', filename = "FReD"))
                   )
  ))

  # variables
  output$variables <- DT::renderDT(DT::datatable(dataset_variables,
    rownames = FALSE, options = list(pageLength = 20, dom = 't')
  ))




  # Correlates of R ---------------------------------------------------------

  library(dplyr)
  library(rlang)
  #LW to check: is this needed?

  aggregate_results <- function(df, ..., result = "result", mixed_text = "mixed", NA_text = "not coded yet") {

    result_sym <- ensym(result)

    # Replace NA values in the chosen result column with NA_text
    df <- df %>%
      mutate({{ result_sym }} := if_else(is.na({{ result_sym }}), NA_text, as.character({{ result_sym }})))

    # Grouping, summarizing and renaming dynamically
    df %>%
      dplyr::group_by(...) %>%
      dplyr::summarise(result_col = if (all({{ result_sym }} == first({{ result_sym }}))) {
        first({{ result_sym }})
      } else {
        mixed_text
      }, .groups = "drop") %>%
      dplyr::rename({{ result_sym }} := result_col)
  }





  output$correlate_decade <- plotly::renderPlotly({

    red_agg <- aggregate_results(df_temp_DT(), ref_original)

    red_agg$year_orig <- as.numeric(substr(gsub("\\D", "", red_agg$ref_original), 1, 4))

    # Remove implausible years
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    red_agg$year_orig <- ifelse(red_agg$year_orig > current_year+2, NA, red_agg$year_orig)
    red_agg$year_orig <- ifelse(red_agg$year_orig < 1900, NA, red_agg$year_orig)

    red_agg$decade_orig <- floor(red_agg$year_orig / 10) * 10

    ### DECADE (ORIGINAL)
    red_agg$row <- 1:nrow(red_agg)
    reprate_decade <- aggregate(row ~ decade_orig * result, data = red_agg, FUN = "length")
    names(reprate_decade) <- c("decade_orig", "Result", "k")

    p <- ggplot(reprate_decade, aes(x = decade_orig, y = k, col = Result)) +
      geom_point(position = "identity") +
      geom_line(position = "identity") +
      # geom_area(position = "stack") +
      theme_bw() +
      labs(
        x = "Decade the Original Finding was Published", y = "Number of Replication Findings",
        title = paste("Aggregated replication outcomes by decade for k = ", sum(reprate_decade$k), " replicated original studies", sep = "")
      ) +
      scale_color_manual("Result", values = outcome_colors())

    plotly::ggplotly(p) %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  })

  output$correlate_journal <- plotly::renderPlotly({
    red_agg <- aggregate_results(df_temp_DT(), ref_original, orig_journal)



    red_agg$row <- 1:nrow(red_agg)
    # red_agg$orig_journal <- tolower(red_agg$orig_journal)
    red_agg$orig_journal <- gsub("\\b([A-Za-z])", "\\U\\1", red_agg$orig_journal, perl = TRUE)
    reprate_journal <- aggregate(row ~ orig_journal * result, data = red_agg, FUN = "length")
    names(reprate_journal) <- c("journal_orig", "Result", "k")

    # remove faulty rows
    reprate_journal <- reprate_journal[reprate_journal$journal_orig != "consistent", ]
    reprate_journal <- reprate_journal[reprate_journal$journal_orig != "inconsistent", ]
    reprate_journal <- reprate_journal[reprate_journal$journal_orig != "mixed", ]
    reprate_journal <- reprate_journal[reprate_journal$journal_orig != "success", ]
    reprate_journal <- reprate_journal[reprate_journal$journal_orig != "informative failure to replicate", ]
    reprate_journal <- reprate_journal[reprate_journal$journal_orig != "practical failure to replicate", ]
    reprate_journal <- reprate_journal[reprate_journal$journal_orig != "inconclusive", ]

    p <- ggplot(reprate_journal, aes(x = journal_orig, y = k, fill = Result)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      labs(
        x = "", y = "Number of Replicated Original Studies",
        title = paste("Aggregated replication outcomes by journal for k = ", sum(reprate_journal$k), " replicated original studies.", sep = "")
      ) +
      scale_fill_manual("Result", values = outcome_colors()) +
      scale_x_discrete(limits = rev) +
      coord_flip()

    plotly::ggplotly(p) %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  })



  # MODERATORS --------------------------------------------------------------

  # Reactives for repeated elements -----------------------------------------

  preprocessed_data <- reactive({
    es <- df_temp_DT()
    es$mod <- es[, input$moderator]
    es <- es[!is.na(es$mod), ]
    es <- es[!is.na(es$ref_original), ]
    es$se <- sqrt((1 - abs(es$es_original)^2) / (es$n_original - 2))
    es
  })

  model_computation <- reactive({
    es <- preprocessed_data()
    message("Estimate metafor")
    mod <- metafor::rma.mv(
      yi = es_replication,
      V = se^2,
      random = ~ 1 | ref_original,
      tdist = TRUE,
      data = es,
      mods = ~ mod - 1,
      method = "ML",
      sparse = TRUE
    )

    message("Done estimating")
    mod
  })

  # Moderator Plot ----------------------------------------------------------

  output$flexibleplot <- plotly::renderPlotly({
    es <- preprocessed_data()
    mod <- es$mod
    p <- ggplot2::ggplot(data = es, aes(y = es_replication, color = ref_original)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      theme_bw() +
      labs(x = input$moderator, y = "Replication Effect Size (r)", color = "Reference")

    if (is.numeric(mod)) {
      p <- p + aes(x = mod) + geom_point() + geom_smooth(aes(color = NULL), formula = y ~ x)
    } else {
      p <- p + aes(x = fct_rev(mod)) + geom_violin(fill = NA) + geom_jitter(aes(color = ref_original), width = .1) + coord_flip()
    }

    plotly::ggplotly(p) %>% plotly::config(displayModeBar = FALSE)
  })

  # Moderator Model ---------------------------------------------------------
  output$flexiblemoderatormodel <- renderPrint({
    model <- model_computation()
    print(model)
  })

  # Moderator Text ----------------------------------------------------------
  output$flexiblemoderatortext <- renderText({
    model <- model_computation()

    HTML(paste("<br><br>The effect of ", "<b>", input$moderator, "</b>", " on replication effect sizes is ", ifelse(model[["QMp"]] < .05, "", "<b>not</b> "),
      "significant at the 5% level. Test of moderators: <i>F</i>(", model[["QMdf"]][1], ", ", model[["QMdf"]][2],
      ") = ", round(model[["QM"]], digits = 2), ", <i>p</i> ",
      ifelse(round(model[["QMp"]], digits = 3) == 0, "< .001", paste("=", round(model[["QMp"]], digits = 3))),
      ".",
      sep = ""
    ))
  })

  # Moderator Table ---------------------------------------------------------

  output$flexiblemodtable <- DT::renderDT({
    es <- preprocessed_data()
    model <- model_computation()
    mod <- es$mod
    # check moderator type (factor or metric)
    if (is.numeric(mod)) { # metric

      modtable <- es %>%
        summarise(n = n(), mean = mean(mod), sd = sd(mod), min = min(mod), max = max(mod))
      rownames(modtable) <- substring(input$moderator, first = 4)
      modtable[, 2:6] <- round(as.data.frame(modtable)[, 2:6], digits = 2)
      modelbeta <- metafor::rma.mv(
        yi = es_replication,
        V = se^2,
        random = ~ 1 | ref_original,
        tdist = TRUE,
        data = es,
        mods = ~mod,
        method = "ML"
      )
      modtable$beta <- round(modelbeta$b[2], digits = 2)
      modtable$vars <- NULL
      modtable$range <- NULL
      modtable$se <- NULL
    } else { # factor
      modtable <- data.frame(
        "Moderator_Levels" = substring(rownames(model$b), first = 4),
        "r" = round(model$b, digits = 3),
        "ci_lower" = round(model$ci.lb, digits = 3),
        "ci_upper" = round(model$ci.ub, digits = 3)
        # , "k" = as.numeric(paste(table(es$mod)))
      )
    }


    # print table
    DT::datatable(modtable,
      options = list(options = list(pageLength = 200, dom = "t")),
      rownames = FALSE
    )
  })


  # REFERENCE CHECKER -------------------------------------------------------

  output$references_barplot <- plotly::renderPlotly({
    if (nchar(input$refcheck) > 0) {
      entries <- as.character(input$refcheck)
    }

    if (nchar(input$refcheck) > 0) {
      entries <- as.character(input$refcheck)
    }

    entries <- unlist(base::strsplit(entries, split = "\n")) # |-
    dois <- tolower(stringr::str_extract(entries, "10.\\d{4,9}/[-._;()/:a-z0-9A-Z]+"))

    # combine coded and uncoded studies
    df[is.na(df$result), "result"] <- "Not coded yet"

    # Check which entries  exist in the df
    intersection <- dois[dois %in% df$doi_original]

    # df subset
    df_temp <- df[(tolower(df$doi_original) %in% dois), ]
    df_temp <- df_temp[!is.na(df_temp$doi_original), ]

    bardata <- as.data.frame(base::table(df_temp$result, useNA = "always") / nrow(df_temp))
    names(bardata) <- c("Result", "Proportion")
    bardata$Proportion <- round(bardata$Proportion, 4) * 100

    bardata$description <- paste(bardata$Result, ": ", bardata$Proportion, "%", sep = "")

    barchart <- ggplot(bardata, aes(x = "", fill = Result, y = Proportion, text = description)) +
      geom_bar(position = "fill", stat = "identity") +
      theme_bw() +
      ylab("Percentage") +
      xlab("") +
      coord_flip() +
      scale_fill_manual("Result", values = outcome_colors()) +
      ggtitle(paste(nrow(df_temp), "Replication findings were identified. These stem from", length(unique(df_temp$doi_original)), "different publication(s)."))

     p <- plotly::ggplotly(barchart, tooltip = "text") %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) #  %>% layout(height = 10000, width = 1200)
  })

  output$references_doi <- shiny::renderTable({
    if (nchar(input$refcheck) > 0) {
      entries <- as.character(input$refcheck)
    }

    entries <- unlist(base::strsplit(entries, split = "\n")) # |-
    dois <- tolower(stringr::str_extract(entries, "10.\\d{4,9}/[-._;()/:a-z0-9A-Z]+"))

    # combine coded and uncoded studies
    df[is.na(df$result), "result"] <- "Not coded yet"

    # Check which entries  exist in the df
    intersection <- dois[dois %in% df$doi_original]

    # df subset
    df_temp <- df[(tolower(df$doi_original) %in% dois), ]
    df_temp <- df_temp[!is.na(df_temp$doi_original), ]

    df_temp$original <- df_temp$ref_original # paste(df_temp$ref_original, df_temp$doi_original, sep = " ") # ADD DOIs if they are not already part of the reference
    df_temp$replication <- df_temp$ref_replication # paste(df_temp$ref_replication, df_temp$doi_replication, sep = " ")

    print(df_temp[, c("original", "description", "replication", "result")])
  })



  # CHECKER / SUMMARIZER -----------------------------------------------------------------


  # Checkertable ------------------------------------------------------------


  output$checkertable <- DT::renderDT(server = FALSE, {

    # exclude non-validated entries
    df_temp <- df_temp()


    # df_temp_filtered <- df_temp[, c("description", "n_original", "n_replication", "power", "result")]
    # df_temp_filtered <- df_temp[, c("description", "tags", "contributors", "result", "ref_original", "ref_replication")]

    DT::datatable(
      df_temp[, c("description", "tags", "result", "ref_original", "ref_replication")],
      extensions = "Buttons",
      options = list(
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        pageLength = 5
        # , lengthMenu = c(5, 10, 100) # XXX not working yet
      ), rownames = FALSE
    )
  })



  # # Checker Violin Plot -----------------------------------------------------
  #
  #
  #
  #   output$checker_violin <- plotly::renderPlotly({
  #
  #     # # combine coded and uncoded studies
  #     # df <- plyr::rbind.fill(df, as)
  #     # df <- plyr::rbind.fill(df, forrt)
  #     # df[is.na(df$result), "result"] <- "not coded yet"
  #
  #     # this plot is based on the filtered entries from the checkertable
  #     df_temp <- df
  #     df_temp <- df_temp[rev(row.names(df_temp)), ]
  #
  #     # exclude non-validated entries
  #     df_temp <- df_temp[!is.na(df_temp$validated), ]
  #
  #     # use only filtered studies
  #     s1 <- input$checkertable_rows_current  # rows on the current page
  #     s2 <- input$checkertable_rows_all      # rows on all pages (after being filtered)
  #     s3 <- input$checkertable_rows_selected # selected rows
  #
  #     df_temp <- df_temp[s2, ]
  #
  #     # exclude NAs
  #     df_temp <- df_temp[!is.na(df_temp$result), ]
  #
  #     # compute se
  #     df_temp$se_original <- sqrt((1-abs(as.numeric(df_temp$es_original))^2)/(as.numeric(df_temp$n_original)-2))
  #     df_temp$se_replication <- sqrt((1-abs(as.numeric(df_temp$es_replication))^2)/(as.numeric(df_temp$n_replication)-2))
  #
  #     redlong_original <- df_temp[, c("es_original", "ref_original", "n_original", "se_original")]
  #     redlong_original$type = "Original"
  #     names(redlong_original) <- c("es", "ref", "n", "se", "type")
  #     redlong_original <- redlong_original[!duplicated(redlong_original), ]
  #
  #     redlong_replication <- df_temp[ , c("es_replication", "ref_replication", "n_replication", "se_replication")]
  #     redlong_replication$type = "Replication"
  #     names(redlong_replication) <- c("es", "ref", "n", "se", "type")
  #
  #     redlong <- rbind(redlong_original, redlong_replication)
  #     checker_gg <- ggplot(redlong, aes(x = type, y = as.numeric(es), text = ref)) + # , text = ref
  #       # geom_violin(draw_quantiles =  .5) +
  #       geom_jitter(width = .1, height = 0) +
  #       xlab("Study Type") + ylab("r") +
  #       geom_abline(h = 0, slope = 0, lty = 2) +
  #       theme_bw()
  #
  #     checker_plotly <- plotly::ggplotly(checker_gg, tooltip = "text") %>% #
  #       plotly::config(displayModeBar = FALSE) %>%
  #       layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  #
  #     checker_plotly
  #
  #   })


  # # Checker MA Plot ---------------------------------------------------------
  #
  #   output$checker_maplot <- plotly::renderPlotly({
  #
  #     # this plot is based on the filtered entries from the checkertable
  #     df_temp <- df
  #     df_temp <- df_temp[rev(row.names(df_temp)), ]
  #
  #     # exclude non-validated entries
  #     df_temp <- df_temp[!is.na(df_temp$validated), ]
  #
  #     # use only filtered studies
  #     s1 <- input$checkertable_rows_current  # rows on the current page
  #     s2 <- input$checkertable_rows_all      # rows on all pages (after being filtered)
  #     s3 <- input$checkertable_rows_selected # selected rows
  #
  #     df_temp <- df_temp[s2, ]
  #
  #     # exclude NAs
  #     df_temp <- df_temp[!is.na(df_temp$result), ]
  #
  #     # compute se
  #     df_temp$se_original <- sqrt((1-abs(as.numeric(df_temp$es_original))^2)/(as.numeric(df_temp$n_original)-2))
  #     df_temp$se_replication <- sqrt((1-abs(as.numeric(df_temp$es_replication))^2)/(as.numeric(df_temp$n_replication)-2))
  #
  #     redlong_original <- df_temp[, c("es_original", "ref_original", "n_original", "se_original")]
  #     redlong_original$type = "Original"
  #     names(redlong_original) <- c("es", "ref", "n", "se", "type")
  #     redlong_original <- redlong_original[!duplicated(redlong_original), ]
  #
  #     redlong_replication <- df_temp[ , c("es_replication", "ref_replication", "n_replication", "se_replication")]
  #     redlong_replication$type = "Replication"
  #     names(redlong_replication) <- c("es", "ref", "n", "se", "type")
  #
  #     # remova missing values
  #     redlong <- rbind(redlong_original, redlong_replication)
  #
  #
  #     redlong <- redlong[!is.na(redlong$ref),]
  #
  #     model <- metafor::rma.mv(yi = as.numeric(es)
  #                              , V = se^2
  #                              , random = ~1 | ref
  #                              , tdist = TRUE
  #                              , data = redlong
  #                              , mods = ~ as.factor(type) - 1
  #                              , method = "ML")
  #
  #
  #     summary(model)
  #
  #     ma_table <- data.frame("study_type" = as.character(c("Original", "Replication"))
  #                            , "mean_r" =   round(as.numeric(model$beta), 3)
  #                            , "se" =       round(as.numeric(model$se), 3)
  #                            , "lower_ci" = round(as.numeric(model$ci.lb), 3)
  #                            , "upper_ci" = round(as.numeric(model$ci.ub), 3)
  #                            , "n" = c(sum(redlong$type == "Original")
  #                                      , sum(redlong$type == "Replication"))
  #     )
  #
  #     checker_maplot <- ggplot(ma_table, aes(x = study_type, y = mean_r, ymin = lower_ci, ymax = upper_ci)) +
  #       geom_point() +
  #       geom_errorbar() +
  #       theme_bw() +
  #       geom_abline(slope = 0, lty = 2) +
  #       ylim(c(-.1, round(max(as.numeric(model$ci.ub)), 1)+.1)) +
  #       xlab("Study Type") + ylab("Mean Effect Size Estimate (r)")
  #
  #     checker_maplotly <- plotly::ggplotly(checker_maplot) %>% # , tooltip = "text"
  #       plotly::config(displayModeBar = FALSE) %>%
  #       layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  #
  #     checker_maplotly
  #   })


  # Checker Barplot ---------------------------------------------------------

  output$checker_bar <- plotly::renderPlotly({
    # this plot is based on the filtered entries from the checkertable
    df_temp <- df
    df_temp <- df_temp[rev(row.names(df_temp)), ]

    # exclude non-validated entries
    df_temp <- df_temp[!is.na(df_temp$validated), ]

    # use only filtered studies
    s1 <- input$checkertable_rows_current # rows on the current page
    s2 <- input$checkertable_rows_all # rows on all pages (after being filtered)
    s3 <- input$checkertable_rows_selected # selected rows

    df_temp <- df_temp[s2, ]

    # exclude NAs
    df_temp <- df_temp[!is.na(df_temp$result), ]

    ## Exclude NAs
    df_temp <- df_temp[!is.na(df_temp$result), ]

    bardata <- as.data.frame(base::table(df_temp$result, useNA = "always") / nrow(df_temp))
    names(bardata) <- c("Result", "Proportion")
    bardata$Proportion <- round(bardata$Proportion, 4) * 100

    bardata$description <- paste(bardata$Result, ": ", bardata$Proportion, "%", sep = "")

    barchart <- ggplot(bardata, aes(x = "", fill = Result, y = Proportion, text = description)) +
      geom_bar(position = "fill", stat = "identity") +
      theme_bw() +
      ylab("Percentage") +
      xlab("") +
      coord_flip()  +
      scale_fill_manual("Result", values = outcome_colors()) +
      ggtitle(paste(nrow(df_temp), "findings selected."))
    p <- plotly::ggplotly(barchart, tooltip = "text") %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) #  %>% layout(height = 10000, width = 1200)

    p
  })

  # Checker MA Table --------------------------------------------------------


  output$flexiblecheckertable <- DT::renderDT({
    # this plot is based on the filtered entries from the checkertable
    df_temp <- df
    df_temp <- df_temp[rev(row.names(df_temp)), ]

    # exclude non-validated entries
    df_temp <- df_temp[!is.na(df_temp$validated), ]

    # use only filtered studies
    s1 <- input$checkertable_rows_current # rows on the current page
    s2 <- input$checkertable_rows_all # rows on all pages (after being filtered)
    s3 <- input$checkertable_rows_selected # selected rows

    df_temp <- df_temp[s2, ]

    # exclude NAs
    df_temp <- df_temp[!is.na(df_temp$result), ]

    # compute se
    df_temp$se_original <- sqrt((1 - abs(as.numeric(df_temp$es_original))^2) / (as.numeric(df_temp$n_original) - 2))
    df_temp$se_replication <- sqrt((1 - abs(as.numeric(df_temp$es_replication))^2) / (as.numeric(df_temp$n_replication) - 2))

    redlong_original <- df_temp[, c("es_original", "ref_original", "n_original", "se_original")]
    redlong_original$type <- "Original"
    names(redlong_original) <- c("es", "ref", "n", "se", "type")
    redlong_original <- redlong_original[!duplicated(redlong_original), ]

    redlong_replication <- df_temp[, c("es_replication", "ref_replication", "n_replication", "se_replication")]
    redlong_replication$type <- "Replication"
    names(redlong_replication) <- c("es", "ref", "n", "se", "type")

    # remova missing values
    redlong <- rbind(redlong_original, redlong_replication)


    redlong <- redlong[!is.na(redlong$ref), ]

    model <- metafor::rma.mv(
      yi = as.numeric(es),
      V = se^2,
      random = ~ 1 | ref,
      tdist = TRUE,
      data = redlong,
      mods = ~ as.factor(type) - 1,
      method = "ML"
    )


    summary(model)

    ma_table <- data.frame(
      "study_type" = as.character(c("Original", "Replication")),
      "mean_r" = round(as.numeric(model$beta), 3),
      "se" = round(as.numeric(model$se), 3),
      "lower_ci" = round(as.numeric(model$ci.lb), 3),
      "upper_ci" = round(as.numeric(model$ci.ub), 3),
      "n" = c(
        sum(redlong$type == "Original"),
        sum(redlong$type == "Replication")
      )
    )

    # print table
    DT::datatable(ma_table,
      options = list(options = list(pageLength = 200, dom = "t")),
      rownames = FALSE
    )
  })


  # Checker MA Text ------------------------------------------------------------

  output$flexiblesummarizertext <- shiny::renderText({
    # this plot is based on the filtered entries from the checkertable
    df_temp <- df_temp()

    # exclude NAs
    df_temp <- df_temp[!is.na(df_temp$result), ]

    # compute se
    df_temp$se_original <- sqrt((1 - abs(as.numeric(df_temp$es_original))^2) / (as.numeric(df_temp$n_original) - 2))
    df_temp$se_replication <- sqrt((1 - abs(as.numeric(df_temp$es_replication))^2) / (as.numeric(df_temp$n_replication) - 2))

    redlong_original <- df_temp[, c("es_original", "ref_original", "n_original", "se_original")]
    redlong_original$type <- "Original"
    names(redlong_original) <- c("es", "ref", "n", "se", "type")
    redlong_original <- redlong_original[!duplicated(redlong_original), ]

    redlong_replication <- df_temp[, c("es_replication", "ref_replication", "n_replication", "se_replication")]
    redlong_replication$type <- "Replication"
    names(redlong_replication) <- c("es", "ref", "n", "se", "type")

    # remova missing values
    redlong <- rbind(redlong_original, redlong_replication)


    redlong <- redlong[!is.na(redlong$ref), ]

    model <- metafor::rma.mv(
      yi = as.numeric(es),
      V = se^2,
      random = ~ 1 | ref,
      tdist = TRUE,
      data = redlong,
      mods = ~ as.factor(type) - 1,
      method = "ML"
    )

    HTML(ma_text <- paste("<br><br><h5>On average and using a random-effects meta-analysis, original effect sizes were ",
      ifelse(model$pval[1] < .05, "", "not "),
      "significant. Average replication effect sizes were ",
      ifelse(model$pval[2] < .05, "", "not "),
      "significant and ",
      ifelse(model$ci.ub[2] < model$b[1] & model$ci.lb[1] > model$b[2], "", "not "),
      "significantly smaller than original effect sizes. ",
      "<h6>",
      sep = ""
    ))
  })


  # Downloadbutton ----------------------------------------------------------


  output$download_data <- downloadHandler(
    filename = function() {
      paste("FReD-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(df_temp_DT(), file, fileEncoding = "WINDOWS-1252")
    }
  )
}

