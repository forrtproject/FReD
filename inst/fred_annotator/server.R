
# ANNOTATOR ---------------------------------------------------------------

server <- function(input, output, session) {
  doi_vector <- reactiveValues(dois = c(), selected_rows = NULL)
  retracted_dois <- reactiveVal(NULL)


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

  reactive_df <- reactiveVal(df)
  selected_refs <- reactiveVal()

  observe({
    selected_refs(
      reactive_df() %>%
        filter(doi_original %in% doi_vector$dois)
    )
  })

  # Recalculate replication success based on criterion
  assess_success <- function(result, success_criterion) {
    assess_replication_outcome(result$es_original, result$n_original, result$es_replication, result$n_replication,
                               criterion = success_criterion)$outcome_report
  }

  observeEvent({input$success_criterion; selected_refs()}, {
    if (nrow(selected_refs()) > 0) {
      updated_df <- selected_refs() %>%
        arrange(ref_original) %>%
        filter(if (input$validated == "TRUE") validated == 1 else TRUE) %>%
        mutate(
          result = assess_success(., input$success_criterion) %>% cap_first_letter(),
          result = factor(
            result,
            levels = rev(c(
              "not coded",
              unique(result[grepl("success", result, ignore.case = TRUE)]),
              unique(result[grepl("failure", result, ignore.case = TRUE)]),
              unique(result[!(result %in% c("not coded", "OS not significant")) &
                              !grepl("success|failure", result, ignore.case = TRUE)]),
              "OS not significant"
            ))
          )
        )

      selected_refs(updated_df)
    }
  }, ignoreNULL = TRUE)



  outcome_colors <- reactive({
    criterion_colors <- success_criteria_colors %>%
      dplyr::filter(criterion == input$success_criterion)

    outcome_colors <- setNames(criterion_colors$color, criterion_colors$label %>% cap_first_letter())
    c(outcome_colors, "Not coded" = "#C8C8C8")
  })

  observeEvent(input$load_retractions, {
    showModal(modalDialog(
      title = "Loading Retraction Database",
      "Please wait while the retraction database is being loaded.",
      easyClose = FALSE,
      footer = modalButton("Cancel") # so that button does not say "Dismiss"
    ))
    retracted_dois(load_retractionwatch() %>%
                     dplyr::filter(RetractionNature == "Retraction") %>%
                     dplyr::select(OriginalPaperDOI))
    updated_df <- reactive_df() %>%
      dplyr::left_join(retracted_dois() %>% mutate(retracted_replication = TRUE), retraction_data(),
                       by = c("doi_replication" = "OriginalPaperDOI"), na_matches = "never") %>%
      dplyr::left_join(retracted_dois() %>% mutate(retracted_original = TRUE), retraction_data(),
                       by = c("doi_original" = "OriginalPaperDOI"), na_matches = "never") %>%
      dplyr::mutate(retracted_replication = dplyr::coalesce(retracted_replication, FALSE),
                    retracted_original = dplyr::coalesce(retracted_original, FALSE),
                    ref_original = ifelse(
                      retracted_original & !grepl("^RETRACTED", ref_original, ignore.case = TRUE),
                      paste("RETRACTED:", ref_original),
                      ref_original
                    ),
                    ref_replication = ifelse(
                      retracted_replication & !grepl("^RETRACTED", ref_replication, ignore.case = TRUE),
                      paste("RETRACTED:", ref_replication),
                      ref_replication
                    ))
    reactive_df(updated_df)
    removeModal()
    showNotification(shiny::HTML(paste("<b>Retraction database loaded successfully.</b><br> FReD contains ", sum(updated_df$retracted_original),
                           " retracted original studies and ", sum(updated_df$retracted_replication), " retracted replication studies.")))
    shinyjs::disable("load_retractions")
  })

  reactive_distinct_fred_entries <- reactive({
    df <- reactive_df() %>%
      arrange(desc(validated == 1)) %>%
      filter(if (input$validated == "TRUE") validated == 1 else TRUE) %>%
      select(doi_original, ref_original)

    dplyr::bind_rows(
      df %>% dplyr::filter(!is.na(doi_original)) %>% dplyr::group_by(doi_original) %>% dplyr::slice(1) %>% dplyr::ungroup(),
      df %>% dplyr::filter(is.na(doi_original))
    )   %>%
      distinct()
  })

  output$button_area <- renderUI({
    if (length(doi_vector$dois) == 0) {

        actionButton("process_button", "Process References")

    } else {
      tagList(

          actionButton("add_button", "Add references"),

        br(), HTML("&nbsp;"), br(),

          actionButton("replace_button", "Replace references")
      )
    }
  })

  parse_dois <- function() {

    # Check if both text box and file are provided
    if (nchar(input$references) > 0 && is.null(input$upload) == FALSE) {
      showNotification("Both text box and file will be processed.")
    }

    dois <- c()

    # Process text box input
    if (nchar(input$references) > 0) {
      text_dois <- extract_dois_from_text(input$references, report = TRUE)
      dois <- c(dois, text_dois)
    }

    # Process uploaded file
    if (is.null(input$upload) == FALSE) {
      file_dois <- extract_dois_from_file(input$upload$datapath)
      dois <- c(dois, file_dois)
    }

    if (input$navbar == "Introduction") {
      nav_select("navbar", "Study Selection")
    }

    out <- unique(dois)

    if (length(out) == 0) {
      showModal(modalDialog(
        title = "No DOIs found",
        "Please check the format of the input",
        easyClose = TRUE
      ))
    }

    out

  }


  observe({
    if (input$navbar == "Report") {
      if (!length(doi_vector$dois > 0)) {
        showModal(modalDialog(
          title = "No DOIs selected",
          "Please select DOIs before proceeding to the report.",
          easyClose = TRUE
        ))
        nav_select("navbar", "Study Selection")
      }
    }
  })


  observeEvent(input$process_button, {
    doi_vector$dois <- parse_dois()
  })

  observeEvent(input$add_button, {
    doi_vector$dois <- union(doi_vector$dois, parse_dois())
  })

  observeEvent(input$replace_button, {
    doi_vector$dois <- parse_dois()
  })


  visible <- reactiveVal(FALSE)

  output$doi_display <- renderText({
    paste(doi_vector$dois, collapse = "\n")
  })

  output$collapsible_dois <- renderUI({
    if (length(doi_vector$dois) > 0) {
      tags$div(
        id = "doi_display_container",
        style = if (visible()) "display: block;" else "display: none;",
        verbatimTextOutput("doi_display"),
        actionButton("copy_btn", "Copy DOIs", onclick = "copyText()")
      )
    }
  })

  observeEvent(input$hidden_toggle_btn, {
    visible(!visible())
  })

  output$showToggle <- reactive({
    length(doi_vector$dois) > 0
  })

  # Need to be active in background to allow toggling of DOI display
  outputOptions(output, "showToggle", suspendWhenHidden = FALSE)
  outputOptions(output, "doi_display", suspendWhenHidden = FALSE)


  extract_dois_from_text <- function(text, report = FALSE) {

    pattern <- "10(?:\\.[0-9]{4,})?\\/[^\\s]*[^\\s\\.,]" # based on pattern used by Zotero Chrome plugin
    dois <- regmatches(text, gregexpr(pattern, text, perl = TRUE))
    dois <- unlist(dois)

    dois <- sub("\\s*([,;.])*$", "", dois)  # remove trailing punctuation

    dois <- sapply(dois, function(x) {
      if (substr(x, nchar(x), nchar(x)) == ")" && sum(strsplit(x, "")[[1]] == "(") < sum(strsplit(x, "")[[1]] == ")")) {
        substr(x, 1, nchar(x) - 1)
      } else {
        x
      }
    })


    dois <- unique(unname(dois))

    if (report) {
    # Count number of non-blank lines
    n_lines <- sum(nzchar(trimws(unlist(strsplit(text, "\n")))))

      showNotification(paste("Found and added", length(dois), ifelse(length(dois) == 1, "DOI", " distinct DOIs"),
                             "from", n_lines, ifelse(n_lines == 1, "line", "lines"), "of text input."),
                       type = "message", duration = 10)

    }

      dois

  }


  extract_dois_from_file <- function(file_path) {
    dois <- c()
    if (file_path != "") {
      if (grepl("\\.pdf$", file_path)) {
        # Extract DOIs from PDF file
        pdf_text <- pdftools::pdf_text(file_path)
        dois <- extract_dois_from_text(pdf_text)
      } else if (grepl("\\.bib$", file_path)) {
        # Extract DOIs from BibTeX file
        bib_text <- readLines(file_path)
        dois <- extract_dois_from_text(bib_text)
      }
    }

    dois <- unique(unname(dois))

    showNotification(paste("Found and added", length(dois), ifelse(length(dois) == 1, "DOI", " distinct DOIs"),
                           "from your file."),
                     type = "message", duration = 10)

    dois

  }


  # Initial rendering of the DataTable
  output$selected_references <- renderDT({
    data <- datatable(
      data.frame(doi_original = character(0), doi_replication = character(0),
                 description = character(0), es_original = numeric(0),
                 es_replication = numeric(0), result = character(0)),
      escape = FALSE, options = list(pageLength = 10), filter = "none"
    )
    formatRound(data, columns = c('es_original', 'es_replication'), digits = 2)

  }, server = TRUE)

  # Observer to update data reactively
  observe({
    proxy <- dataTableProxy('selected_references', deferUntilFlush = FALSE)

    if (length(doi_vector$dois) == 0) {
      replaceData(proxy, data.frame(doi_original = character(0), doi_replication = character(0),
                                    description = character(0), es_original = numeric(0),
                                    es_replication = numeric(0), result = character(0)), resetPaging = TRUE)
      return()
    }

    if (length(doi_vector$dois) > 0) {

    data <- data.frame(doi_original = doi_vector$dois) %>%
      left_join(reactive_df(), by = c("doi_original")) %>%
      mutate(
        in_FReD = doi_original %in% reactive_df()$doi_original,
        doi_original = purrr::map_chr(doi_original, ~sprintf('<a href="https://doi.org/%s" target="_blank">%s</a>', .x, .x)),
        doi_replication = purrr::map_chr(doi_replication, ~if (!is.na(.x)) sprintf('<a href="https://doi.org/%s" target="_blank">%s</a>', .x, .x) else NA_character_)
      ) %>%
      arrange(!in_FReD, is.na(doi_replication)) %>%
      select(doi_original, doi_replication, description, es_original, es_replication, result)

    replaceData(proxy, data, resetPaging = TRUE)
    updateFilters(proxy, data)
    }

  })


  output$database_search <- renderDT({
    reactive_distinct_fred_entries() %>%
    datatable(selection = 'multiple', options = list(pageLength = 10))
  }, server = TRUE)

  observe({
    selected_rows <- which(reactive_distinct_fred_entries()$doi_original %in% doi_vector$dois)
    proxy <- dataTableProxy('database_search')
    selectRows(proxy, selected_rows)
    doi_vector$selected_rows <- selected_rows
  })

  # Observe changes in selected rows - with debounce to avoid loops
  debounced_rows_selected <- debounce(reactive(input$database_search_rows_selected), 250)

  observeEvent(debounced_rows_selected(), ignoreNULL = FALSE, {
    current_rows <- input$database_search_rows_selected

    # Determine newly selected and deselected rows
    newly_selected_rows <- setdiff(current_rows, doi_vector$selected_rows)
    deselected_rows <- setdiff(doi_vector$selected_rows, current_rows)

    # Handle deselection
    if (length(deselected_rows) > 0) {
      remove_dois <- reactive_distinct_fred_entries()$doi_original[deselected_rows]
      duplicate_rows <- which(reactive_distinct_fred_entries()$doi_original %in% remove_dois)
      current_rows <- setdiff(current_rows, duplicate_rows)
      doi_vector$dois <- setdiff(doi_vector$dois, remove_dois)
    }

    # Handle new selections
    if (length(newly_selected_rows) > 0) {
      add_dois <- reactive_distinct_fred_entries()$doi_original[newly_selected_rows]
      duplicate_rows <- which(reactive_distinct_fred_entries()$doi_original %in% add_dois)
      current_rows <- union(current_rows, duplicate_rows)
      doi_vector$dois <- union(doi_vector$dois, add_dois)
    }
    doi_vector$selected_rows <- current_rows

    # Re-select rows in the DT to reflect any changes
    proxy <- dataTableProxy('database_search')
    selectRows(proxy, current_rows)
  })


  # Need to be active in background to stay synced with DOI list
  outputOptions(output, "selected_references", suspendWhenHidden = FALSE)
  outputOptions(output, "database_search", suspendWhenHidden = FALSE)

  output$references_barplot <- plotly::renderPlotly({

    df <- tibble(doi_original = doi_vector$dois, in_FReD = doi_original %in% reactive_df()$doi_original) %>%
      mutate(in_FReD = ifelse(in_FReD, "yes", "no"))

    # Calculate the counts and proportions
    df_summary <- df %>%
      count(in_FReD) %>%
      mutate(proportion = n / sum(n),
             label = paste(in_FReD, "\n(", scales::percent(proportion, accuracy = 1), ")"))

    color_values = c("no" = "#FF7F7F", "yes" = "#8FBC8F")



    # Create the plot
    p <- df_summary %>%
      ggplot(aes(y = "", x = proportion, fill = in_FReD)) +
      geom_bar(stat = "identity", position = "fill") +
      geom_text(aes(label = label), position = position_fill(vjust = 0.5), size = 3) +  # Adjust size as needed
      theme_minimal() +
      labs(y = "", x = "Share", title = "Are replications for references in FReD?") +
      scale_x_continuous(labels = scales::percent) +
      scale_fill_manual(values = color_values) +
      guides(fill = "none") + theme_void()

    plotly::ggplotly(p, tooltip = NULL, height = 150) %>%
      plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) %>%
      plotly::config(displayModeBar = FALSE)

  })

  output$outcomes_barplot <- plotly::renderPlotly({

    df <- selected_refs()

    message("Selected refs: ", nrow(df))

    if (any(df$retracted_replication)) {
      message("any")
      count_retracted_replications <- sum(df$retracted_replication)
      df <- df %>% dplyr::filter(!retracted_replication)
    } else {
      message("none")
      count_retracted_replications <- 0
    }

    message(count_retracted_replications)

    validate(
      need(nrow(df) > 0, "", label = "No replications found")
    )

    retraction_note <- ifelse(
      count_retracted_replications > 0,
      paste0(
        "Note: ", count_retracted_replications,
        " retracted replication stud",
        ifelse(count_retracted_replications == 1, "y was", "ies were"),
        " excluded from the plot."
      ),
      ""
    )

    p <- df %>%
      mutate(result = result %>% forcats::fct_na_value_to_level("not coded")) %>%
      ggplot(aes(y = result, fill = result)) +
      geom_bar() +
      theme_minimal() +
      labs(y = "", x = "Count", title = "Outcomes of replication attempts", caption = retraction_note, fill = "") +
      scale_fill_manual(values = outcome_colors())

    plotly::ggplotly(p, tooltip = NULL) %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))

  })

  # Functions to generate markdown


  assess_outcome <- function(replications, ..., success_criterion = c("consistency", "significance_r"), return_html = TRUE) {

    ref_original <- replications$ref_original[1]

    replications %>%
      mutate(assess_replication_outcome(.data$es_original, .data$n_original, .data$es_replication,
                                        .data$n_replication, criterion = success_criterion)) %>%
      # Remove uncoded duplicates
      arrange(desc(validated == 1), is.na(outcome_report)) %>%
      filter(!(!is.na(doi_replication) & duplicated(doi_replication) & is.na(outcome_report))) %>%
      filter(!(duplicated(ref_replication) & is.na(outcome_report))) %>%
      group_by(ref_replication) %>%
      summarise(
        outcome_report = if(length(unique(outcome_report)) == 1) unique(outcome_report) else paste(outcome_report, collapse = ", "),
        outcome = case_when(
                  all(is.na(outcome)) ~ "not coded",
                  all(outcome == "success") ~ "  success",
                  all(outcome == "failure") ~ "failure",
                  all(outcome == "OS not significant") ~ "OS not significant",
                  .default = "mixed"),
        ref_original = first(ref_original)
        ) %>%
      ungroup() %>%
      summarise(
          replications = paste0("  - **", cap_first_letter(dplyr::coalesce(outcome_report, "not coded")), ":** ", ref_replication, collapse = "\n"),
          overall_outcome = case_when(
            all(outcome == "not coded") ~ if (return_html) "&#x270F;" else "[NC]",
            all(outcome == "success") ~ if (return_html) "<span style='color: darkgreen;'>&#x2714;</span>" else "[Re]",
            all(outcome == "failure") ~ if (return_html) "<span style='color: darkred;'>&#x2716;</span>" else "[¬Re]",
            all(outcome == "OS not significant") ~ if (return_html) "&#x2754;" else "[N/A]", # Using a dash for N/A
            .default = if (return_html) "&#x2753;" else "[?Re]"
          )
      ) %>%
      mutate(ref_original = ref_original)
  }


  generate_markdown <- function(df_filtered, ...) {
    # Call the new legend function
    legend <- generate_legend_markdown(input$success_criterion)

    extra_args <- list(...)
    extra_args["success_criterion"] <- input$success_criterion

    # The main body of the report is generated here
    report_body <- if (nrow(df_filtered) > 0) {
      df_filtered %>%
        mutate(
          ref_original = stringr::str_replace_all(ref_original, "\n", " "),
          ref_original = ifelse(stringr::str_detect(ref_original, stringr::fixed("doi.org")),
                                ref_original,
                                paste0(ref_original, " [https://doi.org/", doi_original, "](https://doi.org/", doi_original, ")")),
          doi_urls = ifelse(!is.na(doi_replication),
                            paste0("https://doi.org/", doi_replication),
                            ifelse(!is.na(osf_link), osf_link, "")),
          ref_replication = stringr::str_replace_all(ref_replication, "\n", " "),
          ref_replication = ifelse(stringr::str_detect(ref_replication, stringr::fixed("doi.org")) | doi_urls == "",
                                   ref_replication,
                                   paste0(ref_replication, " [", doi_urls, "](", doi_urls, ")")),
          id_original = coalesce(doi_original, ref_original)
        ) %>%
        group_by(id_original) %>%
        group_modify(~do.call(assess_outcome, c(list(.x), extra_args))) %>%
        ungroup() %>%
        mutate(
          markdown = paste0(
            "##### ", overall_outcome, " ", ref_original, "\n\n",
            replications, "\n\n"
          )
        ) %>%
        pull(markdown) %>%
        paste(collapse = "\n")
    } else {
      "" # Return empty string if no data
    }

    # Combine the legend and the report body
    final_report <- paste0(
      legend,
      "\n## Replication Outcomes\n\n",
      report_body
    )

    return(final_report)
  }


  #' Generate Markdown for the Report Legend
  #'
  #' Creates a markdown table explaining the symbols used in the report,
  #' dynamically including the note for the selected success criterion.
  #'
  #' @param success_criterion The currently selected success criterion from the UI.
  #' @return A character string containing the markdown for the legend.
  generate_legend_markdown <- function(success_criterion) {
    criterion_explanation <- success_criterion_note[success_criterion]



    legend_md <- glue::glue(
      "**Note:** {criterion_explanation}\n\n",
      "### Legend\n\n",

      "| &nbsp; | &nbsp; |\n",

      "|:------:|:----------------------------------------------------------|\n",

      "| <span style='color: darkgreen;'>&#x2714;</span> | *Success*: All replications of the original study were successful. |\n",
      "| <span style='color: darkred;'>&#x2716;</span> | *Failure*: All replications of the original study failed. |\n",
      "| &#x2753; | *Mixed Results*: The replications of the original study had mixed outcomes (e.g., some succeeded, some failed). |\n",

      if (success_criterion_o_ns[success_criterion]) {
        "| &#x2754; | *Original Not Significant*: The original study's p-value was >= .05, so 'success' is not applicable. |\n"
      } else {
        ""
      },

      "| &#x270F; | *Not Coded*: The outcome of the replication(s) has not yet been coded. |"

    )
    return(legend_md)
  }

    markdown_output <- reactive({
    generate_markdown(selected_refs())
  })

    output$refs_annotated <- renderUI({
      # Validate that there are selected references
      validate(
        need(nrow(selected_refs()) > 0, "", label = "No replications found")
      )

      # Render the markdown from the reactive expression
      tags$div(
        class = "page-like",
        style = "white-space: pre-wrap; overflow-y: auto; max-height: 800px;",
        HTML(markdown::markdownToHTML(text = paste("# Replication Report\n", markdown_output()), fragment.only = TRUE))
      )
    })



  # observe({
  #   df <- reactive_df() %>%
  #     filter(doi_original %in% doi_vector$dois)
  #
  #   if (nrow(df) == 0) {
  #     shinyjs::disable("downloadPdf")
  #   } else {
  #     shinyjs::enable("downloadPdf")
  #   }
  # })
  #
  # output$downloadPdf <- downloadHandler(
  #   filename = function() {
  #     paste("reading_list", Sys.Date(), ".pdf", sep = "")
  #   },
  #   content = function(file) {
  #     tempReport <- tempfile(fileext = ".Rmd")
  #
  #     df <- reactive_df() %>%
  #       filter(doi_original %in% doi_vector$dois)
  #
  #     markdown_output <- generate_markdown(df, return_html = FALSE)
  #
  #     # Create the Rmd content
  #     rmd_content <- paste0(
  #       "---\n",
  #       "title: \"Annotated Reading List\"\n",
  #       "output:\n  pdf_document:\n    latex_engine: xelatex\n",
  #       "---\n\n",
  #       markdown_output
  #     )
  #
  #     writeLines(rmd_content, con = tempReport)
  #
  #     out <- rmarkdown::render(tempReport, quiet = TRUE)
  #     file.rename(out, file)
  #   }
  # )

  output$downloadWord <- downloadHandler(
    filename = function() {
      paste("reading_list", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      tempReport <- tempfile(fileext = ".Rmd")

      # Create the Rmd content
      rmd_content <- paste0(
        "---\n",
        "title: \"Annotated Reading List\"\n",
        "output:\n  word_document:\n    reference_docx: null\n",
        "---\n\n",
        "*NB: This contains bookmarks that are likely displayed as []. You can change your Word settings to hide those; we are still working to remove them when creating the file.* \n\n",
        markdown_output() %>% stringr::str_remove(".*\n")
      )


      writeLines(rmd_content, con = tempReport)

      out <- rmarkdown::render(tempReport, quiet = TRUE)

      # # Pandoc inserts unnecessary bookmarks that are displayed and look like errors - so remove them
      # remove_bookmarks <- function(doc_path, output_path) {
      #   doc <- officer::read_docx(doc_path)
      #
      #   # Get all bookmarks in the document
      #   bookmarks <- officer::docx_bookmarks(doc)
      #
      #   # Iterate over all bookmarks and remove them
      #   for (bookmark in bookmarks) {
      #     cursor <- officer::cursor_bookmark(doc, bookmark)
      #     doc <- officer::cursor_reach(cursor)
      #     doc <- officer::body_remove(cursor)
      #   }
      #
      #   # Save the modified document to the output path
      #   print(doc, target = output_path)
      # }
      #
      # remove_bookmarks(out, out)

      file.rename(out, file)
    }
  )


  output$replicability_plot <- plotly::renderPlotly({

    df <- selected_refs()

    validate(
      need(nrow(df) > 0, "", label = "No replications found")
    )

    df$significant_original <- c("Not significant", "Significant")[(df$p_value_original < .05) + 1] %>%
      factor(levels = c("Not significant", "Significant"))
    df$significant_replication <- c("Not significant", "Significant")[(df$p_value_replication < .05) + 1] %>%
      factor(levels = c("Not significant", "Significant"))


    df$scatterplotdescription <- paste(stringr::str_wrap(df$description, 50), "\nr(original) = ",
                                            round(df$es_original, 3),
                                            ", r(replication) = ",
                                            round(df$es_replication, 3),
                                            sep = ""
    )

    pointsize <- ifelse(nrow(df) < 10, 5, ifelse(nrow(df) < 100, 4, 3))

    df <- df %>% filter(!is.na(es_replication))

    scatterplot <-
      ggplot(df, aes(x = es_original, y = es_replication, text = scatterplotdescription)) +
      geom_hline(aes(yintercept = 0), linetype = 2) +
      geom_abline(intercept = 0, slope = 1, color = "Grey60") +
      geom_point(aes(fill = result), size = pointsize, color = "Grey30", shape = 21, alpha = .8) +
      # geom_point(data = df_temp[s3, ], fill = "#0077d9", color = "#f2ef1b", shape = 4) +
      geom_rug(data = df[df$significant_original == "Significant", ],
               color = "#4DCCD0", linewidth = 1, sides = "b", alpha = .6) +
      geom_rug(data = df[df$significant_original == "Not significant", ],
               color = "#FA948C", linewidth = 1, sides = "b", alpha = .6) +
      geom_rug(data = df[df$significant_replication == "Significant", ],
               color = "#4DCCD0", linewidth = 1, sides = "l", alpha = .6) +
      geom_rug(data = df[df$significant_replication == "Not significant", ],
               color = "#FA948C", linewidth = 1, sides = "l", alpha = .6) +
      scale_x_continuous(name = "Original Effect Size", limits = c(0, 1), breaks = c(0, .25, .5, .75, 1)) +
      scale_y_continuous(name = "Replication Effect Size", limits = c(-.5, 1), breaks = c(-.5, -.25, 0, .25, .5, .75, 1)) +
      # ggtitle("") + #xlab("") + ylab("") +
      # scale_size_continuous(name="Power",range=c(.5,3.5)) +
      #scale_color_discrete(guide = "none") +
      scale_fill_manual(values = outcome_colors(), drop = FALSE) +
      theme_bw() +
      labs(fill = "Replication Outcome", color = "Significance")
      #theme(legend.position = "inside", plot.margin = unit(c(-2,-1.5,2,2), "lines"))
      #theme(legend.position = "none")

    plotly::ggplotly(scatterplot, tooltip = "text") %>%
      plotly::config(displayModeBar = FALSE) %>%
      plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  })

}
