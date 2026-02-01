# Global imports - mostly for shiny
#' @import shiny
#' @import bslib
#' @import shinycssloaders
#' @import ggplot2
#' @rawNamespace import(plotly, except = c(last_plot))
#' @import dplyr
#' @importFrom dplyr %>%
#' @import checkmate
#' @rawNamespace import(DT, except = c(dataTableOutput, renderDataTable))
#' @import markdown
#' @importFrom stats na.omit pnorm pt qnorm qt
#' @importFrom utils browseURL download.file
NULL

utils::globalVariables(c("."))

# Add cache
.cache <- rlang::new_environment()

# Dummy function calls to ensure R CMD check recognizes the usage
# these packages are used in the Shiny apps
dummy_function_calls <- function() {
  if (FALSE) {
    forcats::fct_relevel
    pdftools::pdf_text
    shinyjs::useShinyjs
    zcurve::zcurve
  }
}

.check_req_packages <- function(x, note = "") {
  res <- unlist(suppressWarnings(lapply(x, requireNamespace, quietly = TRUE)))
  if (!all(res)) {
    if (!interactive()) {
      stop(note, "Some required packages are not installed. Make sure you have
               these packages: ", paste0(x[!res], collapse = ", "),
           call. = FALSE
      )
    }
    op <- options("warn")
    on.exit(options(op))
    options(warn = 1)
    warning(note, "The following packages are required for this function but
                   cannot be loaded: ", paste0(x[!res], collapse = ", "),
            call. = FALSE)
    choice <- readline(prompt = "Should I try to install these packages? (Y/N)")
    if (choice %in% c("Y", "y")) {
      utils::install.packages(x[!res])
      res <- unlist(suppressWarnings(lapply(x, requireNamespace, quietly = TRUE)))
      if (!all(res)) {
        stop("Not all packages could be installed successfully. The following could still not be loaded: ", paste0(x[!res], collapse = ", "),
             call. = FALSE
        )
      }
      return(TRUE)
    }
    stop("Cannot proceed without these packages.", call. = FALSE)
  }
}

#' Get FReD dataset citation
#'
#' Retrieves the current citation for the FReD dataset from GitHub.
#'
#' @param citation_url URL to the citation file on GitHub
#' @param cache Should the citation be returned from cache, if already requested during this session? Defaults to TRUE.
#' @return A markdown-formatted citation for the FReD dataset.

create_citation <- function(citation_url = "https://raw.githubusercontent.com/forrtproject/FReD-data/main/output/citation.txt", cache = TRUE) {
  if (get_param("FRED_OFFLINE")) {
    return(return_inbuilt("citation"))
  }

  tryCatch(
    {
      if (cache && exists("citation", .cache, inherits = FALSE)) {
        return(.cache$citation)
      }

      temp <- tempfile(fileext = ".txt")
      download.file(citation_url, temp, quiet = TRUE)

      # Validate download succeeded
      if (!file.exists(temp) || file.size(temp) == 0) {
        stop("Failed to download citation file")
      }

      cit <- readLines(temp, warn = FALSE) %>% paste(collapse = "\n")

      # Validate citation content is not empty
      if (nchar(trimws(cit)) == 0) {
        stop("Citation file is empty")
      }

      .cache$citation <- cit

      cit
    },
    error = function(e) {
      return_inbuilt("citation")
    }
  )
}

#' Get the dataset changelog from OSF
#'
#' Downloads and reads the changelog file from OSF, and returns it as a character string.
#'
#' @param changelog_file The URL of the changelog file on OSF
#' @param cache Should the changelog be returned from cache, if already requested during this session? Defaults to TRUE.

get_dataset_changelog <- function(changelog_file = "https://osf.io/fj3xc/download", cache = TRUE) {
  if (get_param("FRED_OFFLINE")) {
    return(return_inbuilt("data_changelog"))
  }

  tryCatch(
    {
      if (cache && exists("changelog", .cache, inherits = FALSE)) {
        return(.cache$changelog)
      }
      temp <- tempfile(fileext = ".md")
      download.file(changelog_file, temp)
      changelog <- readLines(temp, warn = FALSE) %>% paste(collapse = "\n")
      .cache$changelog <- changelog
      changelog
    },
    error = function(e) {
      return_inbuilt("data_changelog")
    }
  )
}

#' Load the FReD dataset
#'
#' This function loads the FReD dataset into R, and conducts variable transformations to  prepare for analyses.
#'
#' @inheritParams read_fred
#' @return A data frame with the processed FReD dataset
#' @export

load_fred_data <- function(data = get_param("FRED_DATA_FILE"), verbose = FALSE) {

  read_fred(data, verbose = verbose) %>%
    clean_variables() %>%
    add_common_effect_sizes() %>%
    align_effect_direction() %>%
    add_uncertainty() %>%
    add_replication_power() %>%
    code_replication_outcomes() %>%
    augment_for_zcurve()
}

safe_read_xl <- function(file, url, ...) {
  # TK: make warning clearer
  tryCatch(openxlsx::read.xlsx(file, ...), error = function(e) {
    openxlsx::read.xlsx(url, ...)
  })
}

