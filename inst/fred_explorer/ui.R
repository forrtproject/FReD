# EXPLORER ----------------------------------------------------------------

library(shiny)
library(bslib)
library(plotly)
library(DT)
library(shinycssloaders)

# Define input choices
large_scale_project_choices <- list(
  "All studies" = "All studies",
  "Individual submissions" = "Individual submissions",
  "Boyce et al., 2023 (student replications)" = "Boyce_etal2023",
  "CORE (Feldman JDM Replications)" = "CORE",
  "CRSP special issue",
  "Data Replicada" = "datareplicada",
  "FORRT Replications and Reversals" = "FORRT",
  "Many Labs 1" = "ML1",
  "Many Labs 2" = "ML2",
  "Many Labs 3" = "ML3",
  "OpAQ (Anchoring Effects)" = "OpAQ",
  "OpenMKT.org Replications Catalog" = "openmkt",
  "OSF Registries" = "OSF Registries",
  "Reproducibility Project Psychology (OSC, 2015)" = "OSC 2015",
  "RRR1 (verbal overshadowing)" = "RRR1",
  "RRR3 (grammar on intentionality effect)" = "RRR3",
  "RRR4 (ego depletion)" = "RRR4",
  "RRR8 (professor priming)" = "RRR8",
  "RRR9 (hostility priming)" = "RRR9",
  "RRR Inclusive Language" = "RRR Inclusive Language",
  "Soto (Big5 Correlations)" = "Soto",
  "SSRP (Camerer et al., 2018)" = "SSRP"
)

moderator_choices <- list(
  "Original Effect Size" = "es_original",
  "Journal" = "orig_journal",
  "Year of Original Publication" = "orig_year",
  "Power of Replication Study" = "power"
)

# Sidebar content
sidebar_contents <- sidebar(
  id = "sidebar",
  padding = 10,
  width = 300,
  div(id = "sidebar-note", style = "color: red; font-weight: bold; display: none;", "Filtering is disabled for this view, as the full dataset is used."),
  sliderInput("minpower",
              label = popover(
                trigger = list(
                  "Minimum Power",
                  icon(c("info-circle"))
                ),
                "Power is computed based on the original effect size and the replication sample size under the assumption that this is a correlation test. Studies are not filtered for power when this slider is at 5%."
              )
              , min = .05, max = .999, value = .05),
  selectInput("source", "Browse Large-Scale Projects:",
              choices = large_scale_project_choices,
              selected = "All studies"
  ),
  tags$style(HTML("
            #result_var {
              display: none;
            }
          ")), # Remove this to actually use the result_var input
  checkboxInput("validated", "Show validated entries only", value = TRUE),
  checkboxInput("codedentries", "Show coded entries only", value = TRUE),
  hr(),
  radioButtons("success_criterion",
               label = popover(
                 trigger = list(
                   "Success criterion",
                   icon(c("info-circle"))
                 ),
                 "Check our ",
                 a("vignette", href = "https://forrt.org/FReD/articles/success_criteria.html", target = "_blank"),
                 "for details on the different success criteria."
               ),
               choices = c("Significance of Replication" = "significance_r",
                           "Aggregated Significance" = "significance_agg",
                           "Consistency with CI" = "consistency_ci",
                           "Consistency with PI" = "consistency_pi",
                           "Homogeneity" = "homogeneity",
                           "Homogeneity & Significance" = "homogeneity_significance",
                           "Small Telescopes" = "small_telescopes"),
               selected = "significance_r"),
  div(
    HTML("<strong>NB:</strong> The success criteria (e.g., <em>p</em>-values, CIs) are calculated from raw effect and sample sizes. These may differ from original reports that used adjusted models.")
  )
)

# Define content for each panel
replicability_tracker_content <- nav_panel(
  "Replicability Tracker",
      dataset_info,
      uiOutput("success_note"),
      shinycssloaders::withSpinner(plotly::plotlyOutput("barplot", width = "100%", height = "250px")),
      shinycssloaders::withSpinner(DT::DTOutput("table")),
      scatterplot_title,
      shinycssloaders::withSpinner(plotly::plotlyOutput("overviewplot", width = "100%", height = 800)),
      scatterplot_explanation,
      #barplot2_title,
      #shinycssloaders::withSpinner(plotly::plotlyOutput("barplot2", width = "100%", height = "250px")),
      #barplot2_explanation,
      breaks,
      zcurve_title, breaks,
      shinycssloaders::withSpinner(shiny::plotOutput("zcurve_plot")),
      zcurve_explanation, breaks
    )



study_overview_content <- nav_panel(
  "Study Overview",
 forest_info, shinycssloaders::withSpinner(plotly::plotlyOutput("forestplot", width = "100%")) # , height = forestplotheight
 , column(width = 10, uiOutput("forestplot_ui")
 )
  )

dataset_content <- nav_panel(
  "Dataset",
  dataset_explanation, downloadButton("download_data", "Download processed dataset"),
  breaks,
  dataset_headline,
  shinycssloaders::withSpinner(DT::DTOutput("dataset")),
  breaks,
  variables_headline,
  shinycssloaders::withSpinner(DT::DTOutput("variables"))
)

correlates_content <- nav_panel(
  "Correlates of Replicability",
  correlates_info,
  correlates_decade,
  shinycssloaders::withSpinner(plotly::plotlyOutput("correlate_decade")),
  correlates_journal,
  shinycssloaders::withSpinner(plotly::plotlyOutput("correlate_journal", width = "100%", height = "2000px"))
)

moderators_content <- nav_panel(
  "Moderators [alpha]",
  moderators_info,
  shiny::selectInput("moderator",
                     label = "Moderators",
                     choices = moderator_choices
  ),
  fluidRow(shinycssloaders::withSpinner(plotly::plotlyOutput("flexibleplot", width = "100%", height = 600))),
  fluidRow(
    column(6, shinycssloaders::withSpinner(DT::DTOutput("flexiblemodtable"))),
    column(6, shinycssloaders::withSpinner(shiny::htmlOutput("flexiblemoderatortext")))
  )
)

references_checker_content <- nav_panel(
  "References-Checker [alpha]",
  rc_info,
  textAreaInput("refcheck", "References",
                value = "Judge, T. A., & Bono, J. E. (2000). Five-factor model of personality and transformational leadership. Journal of Applied Psychology, 85, 751-765. 10.1037/0021-9010.85.5.751",
                width = "1000px",
                height = "200px",
                placeholder = NULL
  ),
  shinycssloaders::withSpinner(plotly::plotlyOutput("references_barplot")),
  shinycssloaders::withSpinner(tableOutput("references_doi"))
)

references_content <- nav_panel(
  "References",
  column(6,
         references_headline, references_list,
         references_redpublications, references_list_redpublications,
         packages_headline, packages_list
  )
)

faq_content <- nav_panel(
  "FAQ",
  column(6, includeMarkdown("www/FAQ.md"))
)

about_content <- nav_panel(
  "About",
  class = "tab-pane-narrow",
  markdown(about_page),
  markdown(paste0("#", get_dataset_changelog())),
  h2("Package changelog"),
  includeMarkdown(system.file("NEWS.md", package = "FReD")),
  img(src = "fred.png", height = 80),
  tags$style(HTML("
    .navbar-default .navbar-brand {color:black;}
    .navbar-default .navbar-brand:hover {color:black;}
    .navbar { background-color:#EAEAEA;}
    .navbar-default .navbar-nav > li > a {color: dark grey;}
    .navbar-default .navbar-nav > .active > a,
    .navbar-default .navbar-nav > .active > a:focus,
    .navbar-default .navbar-nav > .active > a:hover {color:black;background-color:#fc2d2d;}
    .navbar-default .navbar-nav > li > a:hover {color:black;background-color:#A6A6A6;text-decoration}
  "))
)

# Construct the page_navbar UI
ui <- tagList(
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(HTML(paste0(custom_css, "
      .navbar-brand {
        display: flex;
        align-items: center;
        margin-left: 15px;
        height: 50px;
      }
      .navbar-brand img {
        margin-right: 10px;
      }
      .navbar-nav > li > a {
        line-height: 50px !important;
      }
      .dark-mode-nav {
        margin-left: auto;
      }
    ")))
  ),
  tags$script(HTML("
      $(document).on('shiny:inputchanged', function(event) {
        if (event.name === 'navbar') {
          var tabsWithoutSidebar = ['Dataset', 'References-Checker [alpha]', 'References', 'FAQ', 'About'];
          if (tabsWithoutSidebar.includes(event.value)) {
            $('#sidebar .shiny-input-container').not('#success_criterion').addClass('disabled');
            $('#sidebar-note').show();
          } else {
            $('#sidebar .shiny-input-container').removeClass('disabled');
            $('#sidebar-note').hide();
          }
        }
      });
      $(document).on('shiny:connected', function(event) {
        $('<style type=\"text/css\"> .shiny-input-container.disabled * { pointer-events: none; opacity: 0.5; } </style>').appendTo('head');
      });
  ")),
  page_navbar(
    theme = custom_theme,
    id = "navbar",
    title = tags$a(
      class = "navbar-brand",
      href = "#",
      tags$img(src = "fred.png", height = 67 / 2.5, width = 715 / 2.5)
    ),
    window_title = "FReD Explorer",
    sidebar = sidebar_contents,
    replicability_tracker_content,
    study_overview_content,
    #dataset_content, out for now
    correlates_content,
    references_content,
    faq_content,
    about_content,
    nav_item(input_dark_mode(), class = "dark-mode-nav")
  )
)

