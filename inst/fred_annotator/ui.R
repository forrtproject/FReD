# ANNOTATOR ---------------------------------------------------------------

# UI definition

sidebar_contents <- sidebar(
  id = "mySidebar",
  padding = 10,
  width = 300,
  textAreaInput("references", "Paste references with DOIs", placeholder = "Paste references here...", width = '95%', height = '200px'),
  fileInput("upload", HTML('Or upload reference list (PDF, citation or text file). Must contain DOIs; otherwise process it with <a href="https://apps.crossref.org/SimpleTextQuery" target = "_blank">Crossref</a> first. Copying references from PDF generally works better than PDF upload. Linebreaks within DOIs prevent DOI recognition - preprocessing via an LLM is recommended.'), accept = c("application/pdf", " text/plain", ".bib", ".ris")),
  uiOutput("button_area"),
  checkboxInput("validated", "Use validated database entries only", value = FALSE),
  actionButton("load_retractions", "Load Retraction Database", icon = icon("database")),
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
    HTML("<strong>NB:</strong> The success criteria (e.g., <em>p</em>-values, CIs) are calculated from raw effect and sample sizes. They may differ from original reports that used adjusted models.")
  ),
  conditionalPanel(
    condition = "output.showToggle",  # This JavaScript condition reacts to Shiny output
    tags$a(id = "toggle_link", "Show/Hide DOIs >", href = "#", class = "btn btn-link"),
    actionButton("hidden_toggle_btn", "Toggle", style = "display: none;")
  ),
  uiOutput("collapsible_dois"),
  tags$script(HTML("
    $(document).on('click', '#toggle_link', function(event) {
      event.preventDefault();
      $('#doi_display_container').toggle();  // Toggle visibility of the DOI display container
      $('#hidden_toggle_btn').click();  // Trigger the hidden button click
    });
    function copyText() {
      const element = document.getElementById('doi_display');
      navigator.clipboard.writeText(element.textContent);
      alert('Copied to clipboard!');
    }
  "))
  )


# Define content for each panel
introduction_content <- nav_panel(
  "Introduction",
  class = "tab-pane-narrow",
  includeMarkdown("www/introduction.md")

)

study_selection_content <- nav_panel(
  "Study Selection",
  h2("Selected studies"),
  HTML(paste("<a href=https://github.com/forrtproject/FReD/issues/new?template=doi-reading-issue.md>Please report potential issues with DOI processing.</a>", collapse = "")),
         DTOutput("selected_references", fill = FALSE),
  h2("Available studies in FReD"),
  HTML(paste("Select/unselect rows to add/remove studies from the report. You can search by DOI, reference or description.
       At present, only studies where the DOI is listed in FReD can be used in the annotator, rows without DOIs will
       be ignored here, but can be retrieved from the FReD dataset."), sep = ""), # <a href=https://www.osf.io/9r62x/>the FReD dataset</a>
         DTOutput("database_search", fill = FALSE)
  )


report_content <- nav_panel(
  "Report",
  div(
    style = "max-width: 1000px; margin: auto;",
    plotly::plotlyOutput("references_barplot", height = 150),
    br(),
    uiOutput("success_note"),
    plotly::plotlyOutput("outcomes_barplot"),
    br(),
    plotly::plotlyOutput("replicability_plot", height = "600px"),
    scatterplot_explanation
  ),
  div(
    style = "max-width: 450px; margin: auto; display: flex; gap: 10px; align-items: center; !important",
    downloadButton("downloadWord", "Download annotated Word file"),
    # downloadButton("downloadPdf", "Download annotated PDF")
  ),
  shinycssloaders::withSpinner(uiOutput("refs_annotated"))
)

about_content <- nav_panel("About",
                           class = "tab-pane-narrow",

                        markdown(about_page),
                        markdown(paste0("#", get_dataset_changelog())),
                        h2("Package changelog"),
                        includeMarkdown(system.file("NEWS.md", package = "FReD")),
                        img(src = "fred.png", height = 80),
                        tags$style(HTML("

                               "))
)

ui <- tagList(
  tags$head(
    tags$style(HTML("
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
    "))
  ),
  page_navbar(
    theme = custom_theme,
    id = "navbar",
    title = tags$a(
      class = "navbar-brand",
      href = "#",
      tags$img(src = "fred.png", height = "40")
    ),
    sidebar = sidebar_contents,
    introduction_content,
    study_selection_content,
    report_content,
    about_content,
    nav_item(input_dark_mode(), class = "dark-mode-nav")
  )
)
