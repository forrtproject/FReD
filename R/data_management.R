
#' Returns an inbuilt data item
#'
#' This is used then the package is set to work offline, or when the data cannot be loaded.
#'
#' @param item Which inbuilt item to return?
#' @noRd

return_inbuilt <- function(item) {
  assert_choice(item, c("data", "data_description", "data_changelog", "citation", "data_date"))
  if (item == "data_date") {
    date_only <- TRUE
    item <- "data"
  } else {
      date_only <- FALSE
    }
  data <- system.file("extdata", "snapshot", paste0(item, ".RDS"), package = "FReD")
  data <- readRDS(data)

  if (!get_param("FRED_OFFLINE") & !date_only) {
    last_updated <- format(attr(data, "last_updated"), "%d-%m-%Y %I:%M%p")
    message("Using inbuilt ", item, " last updated on ", last_updated, ". This is likely because of an issue with your internet connection, or with the online data source. If this is unexpected and persists, please report this issue on GitHub.")
  }
  if (date_only) {
    return(attr(data, "last_updated"))
  }
  data
}


#' Load variable descriptions
#'
#' This reads names and variable descriptions of key variables from FReD. This can used for subsetting and describing the dataset.
#'
#' @param sheet_name Path to the variable descriptions
#' @param data Path to the FReD dataset (defaults to current FReD data on OSF)
#' @return A data frame with variable names (`Variable`) and descriptions (`Description`)

load_variable_descriptions <- function(sheet_name = "Key Variables", data = get_param("FRED_DATA_FILE")) {
  if (get_param("FRED_OFFLINE")) return(return_inbuilt("data_description"))
  tryCatch({
    safe_read_xl(data, url = get_param("FRED_DATA_URL"), sheet = sheet_name, startRow = 2)
  }, error = function(e) {
    return_inbuilt("data_description")
  })
}

#' Bind Rows with Character Columns
#'
#' A wrapper for `dplyr::bind_rows` that ensures any columns that are character
#' in one of the data frames are converted to character in all data frames before binding.
#' This reduces the likelihood of errors, but does not give up on type-checking entirely.
#'
#' @param ... Data frames to combine. Each argument should be a data frame.
#' @param .id An optional string that will be used to create a column in the output.
#' If supplied, this will create a new column with the name given by `.id`, and each
#' row will have a value corresponding to the argument name from which it came.
#'
#' @return A data frame created by binding the rows of the input data frames.
#' @keywords internal

bind_rows_with_characters <- function(..., .id = NULL) {
  dfs <- list(...)

  all_cols <- unique(unlist(lapply(dfs, colnames)))

  # Convert columns to character if any column in any dataframe is character
  dfs <- lapply(dfs, function(df) {
    for (col in all_cols) {
      if (col %in% colnames(df)) {
        if (any(sapply(dfs, function(x) col %in% colnames(x) && is.character(x[[col]])))) {
          df[[col]] <- as.character(df[[col]])
        }
      }
    }
    return(df)
  })
  dplyr::bind_rows(dfs, .id = .id)
}


#' Read the FReD dataset
#'
#' This function loads the FReD dataset into R. It merges the data from the different sheets into one data frame.
#'
#' @param data Path to the FReD dataset (defaults to current FReD data on OSF), unless the package is in offline mode (`use_FReD_offline()`)
#' @param retain_es_as_character Should effect sizes be retained as character? Defaults to TRUE, so that coded test statistics with df can be converted to common metric.
#' @param verbose Should detailed messages be printed that highlight data conversion issues? Defaults to TRUE. FALSE is quiet mode, and NULL prints a summary of problems.
#' @return A data frame with the FReD dataset

read_fred <- function(data = get_param("FRED_DATA_FILE"), retain_es_as_character = TRUE, verbose = TRUE) {

  if (get_param("FRED_OFFLINE")) return(return_inbuilt("data"))

  tryCatch({

    # New data format: single sheet with all data
    fred_data <- safe_read_xl(data, url = get_param("FRED_DATA_URL"), sheet = 1)

    # Add id column if not present (use fred_id or row number)
    if (!"id" %in% names(fred_data)) {
      if ("fred_id" %in% names(fred_data)) {
        fred_data$id <- fred_data$fred_id
      } else {
        fred_data$id <- seq_len(nrow(fred_data))
      }
    }

    numeric_variables <- c("n_o", "n_r", "es_value_o", "es_value_r")

    # Only coerce variables that exist in the dataset
    numeric_variables <- intersect(numeric_variables, names(fred_data))

    fred_data <- coerce_to_numeric(fred_data, numeric_variables, id_var = "id", verbose = verbose)

    return(fred_data)

    }, error = function(e) {
      return(return_inbuilt("data"))
    })

}

#' Coerce specified variables to numeric and identify problematic values
#'
#' Attempts to convert specified columns in a data frame to numeric. If values cannot be coerced,
#' the function optionally issues warnings or summaries listing problematic values or IDs.
#'
#' @param df A data frame containing the variables to be coerced.
#' @param numeric_vars A character vector of variable names to be coerced to numeric.
#' @param id_var A string specifying the name of the ID variable used to identify problematic entries.
#' @param verbose Logical or NULL. If TRUE, warns with IDs for problematic values.
#' If FALSE, runs silently. If NULL, prints a summary per variable.
#'
#' @return The input data frame with specified variables coerced to numeric (if possible).

coerce_to_numeric <- function(df, numeric_vars, id_var, verbose = TRUE) {
  problematic_entries <- list()

  for (var in numeric_vars) {
    # Suppress "NAs introduced by coercion" warnings - we detect and report these ourselves below
    problematic_rows <- which(!is.na(df[[var]]) & is.na(suppressWarnings(as.numeric(df[[var]]))))

    if (length(problematic_rows) > 0) {
      problematic_entries[[var]] <- df[problematic_rows, id_var, drop = FALSE]
    }

    df[[var]] <- suppressWarnings(as.numeric(df[[var]]))
  }

  if (length(problematic_entries) > 0) {
    if (isTRUE(verbose)) {
      warning_message <- "The following fields contain values that could not be coerced to numeric:\n"
      for (var in names(problematic_entries)) {
        warning_message <- paste0(warning_message, "Variable '", var, "' has issues in IDs: ",
                                  paste(problematic_entries[[var]][[id_var]], collapse = ", "), "\n")
      }
      warning(warning_message)
    } else if (is.null(verbose)) {
      for (var in names(problematic_entries)) {
        message(sprintf("Variable '%s': %d entries could not be converted to numeric.",
                        var, nrow(problematic_entries[[var]])))
      }
    }
  }

  df
}


#' Clean variables
#' Perform some specific operations (e.g., recoding some NA as "") required to get the Shiny apps to work.
#' This may be a temporary solution, as much of it should likely be handled through validation in the data sheet, and at import time.

#' @param fred_data FReD dataset
#' @return FReD dataset with cleaned variables

clean_variables <- function(fred_data) {

  # Initialize columns that may not exist in new data format
  if (!"description" %in% names(fred_data)) fred_data$description <- ""
  if (!"tags" %in% names(fred_data)) fred_data$tags <- NA
  if (!"contributors" %in% names(fred_data)) fred_data$contributors <- NA
  if (!"result" %in% names(fred_data)) fred_data$result <- NA
  if (!"notes" %in% names(fred_data)) fred_data$notes <- NA
  if (!"exclusion" %in% names(fred_data)) fred_data$exclusion <- NA
  if (!"validated" %in% names(fred_data)) fred_data$validated <- 1
  if (!"osf_link" %in% names(fred_data)) {
    fred_data$osf_link <- ifelse(!is.na(fred_data$url_r), fred_data$url_r, NA)
  }
  if (!"source" %in% names(fred_data)) fred_data$source <- NA
  if (!"orig_journal" %in% names(fred_data)) {
    fred_data$orig_journal <- if ("journal_o" %in% names(fred_data)) fred_data$journal_o else NA
  }

  # recode variables for app to work
  fred_data$description <- ifelse(is.na(fred_data$description), "", fred_data$description)

  fred_data$closeness <- NA
  fred_data$result <- ifelse(!is.na(fred_data$result) & fred_data$result == "0", NA, fred_data$result)

  # compute year the original study was published - use year_o if available, otherwise extract from ref_o
  if ("year_o" %in% names(fred_data)) {
    fred_data$orig_year <- as.numeric(fred_data$year_o)
  } else {
    fred_data$orig_year <- as.numeric(gsub(".*((18|19|20)\\d{2}).*", "\\1", fred_data$ref_o))
  }

  # delete duplicates and non-replication studies (only if notes column exists and has values)
  if ("notes" %in% names(fred_data)) {
    fred_data <- fred_data[is.na(fred_data$notes) | fred_data$notes != "duplicate", ]
    fred_data <- fred_data[is.na(fred_data$notes) | fred_data$notes != "No actual replication conducted", ]
  }

  # remove entries with reasons for exclusions (only if exclusion column exists)
  if ("exclusion" %in% names(fred_data)) {
    fred_data <- fred_data[is.na(fred_data$exclusion), ]
  }

  # Collapse validated categories (# 2: error detected and corrected)
  if ("validated" %in% names(fred_data)) {
    fred_data$validated <- ifelse(fred_data$validated == 1 | fred_data$validated == 2, 1, fred_data$validated)
  }

  # Strip DOIs by removing everything before first 10.
  if ("doi_o" %in% names(fred_data)) {
    fred_data$doi_o <- gsub("^.*?(10\\.\\d+/.*$)", "\\1", fred_data$doi_o) %>% str_trim_base()
  }
  if ("doi_r" %in% names(fred_data)) {
    fred_data$doi_r <- gsub("^.*?(10\\.\\d+/.*$)", "\\1", fred_data$doi_r) %>% str_trim_base()
  }

  # Remove DOIs from references
  if ("ref_o" %in% names(fred_data)) {
    fred_data$ref_o <- fred_data$ref_o %>%
      stringr::str_remove_all("https?://(dx\\.)?doi\\.org/10\\.[^ >,]+") %>%
      stringr::str_remove_all("doi:10\\.[^ >,]+") %>%
      stringr::str_remove_all("10\\.[^ >,]+") %>%
      str_trim_base()
  }

  if ("ref_r" %in% names(fred_data)) {
    fred_data$ref_r <- fred_data$ref_r %>%
      stringr::str_remove_all("https?://(dx\\.)?doi\\.org/10\\.[^ >,]+") %>%
      stringr::str_remove_all("doi:10\\.[^ >,]+") %>%
      stringr::str_remove_all("10\\.[^ >,]+") %>%
      str_trim_base()
  }

  fred_data
}

#' Load RetractionWatch data
#'
#' Loads the RetractionWatch data from Crossref to update the FReD dataset with the most recent retraction data.
#'
#' @param data URL to download RetractionWatch data from - defaults to use the `RETRACTIONWATCH_DATA` parameter, which enables temporary caching of the download
#' @noRd

load_retractionwatch <- function(data = get_param("RETRACTIONWATCH_DATA_FILE")) {
  utils::read.csv(data, stringsAsFactors = FALSE)
}

#' Update inbuilt data
#'
#' If you set the package to work offline (`use_FReD_offline(TRUE)`) or if any
#' downloads fail, FReD will use offline data stored in the package. Use this
#' function if you want to update the data to the current state. (NB: you can also use this
#' if you want to work persistently with your own version of the dataset).
#'
#' @param data_file Path to FReD data file.
#' @param items Vector of items to update - defaults to all necessary items.
#'
#' @export

update_offline_data <- function(data_file = get_param("FRED_DATA_FILE"), items = c("data", "data_description", "data_changelog", "citation")) {
  offline_param <- get_param("FRED_OFFLINE")
  if (offline_param) use_FReD_offline(FALSE)
  success_items <- c()
  failed_items <- c()

  update_item <- function(item) {
    tryCatch({
      if (item == "data") {
        data <- read_fred(data_file)
      } else if (item == "data_description") {
        data <- load_variable_descriptions(data = data_file)
      } else if (item == "data_changelog") {
        data <- get_dataset_changelog()
      } else if (item == "citation") {
        data <- create_citation(data_file)
      }
      attr(data, "last_updated") <- Sys.time()
      file_path <- system.file("extdata", "snapshot", paste0(item, ".RDS"), package = "FReD")
      saveRDS(data, file_path)
      success_items <<- c(success_items, item)
    }, error = function(e) {
      failed_items <<- c(failed_items, item)
    })
  }

  lapply(items, update_item)

  if (length(success_items) > 0) {
    message("The following items were updated successfully: ", paste(success_items, collapse = ", "))
  }

  if (length(failed_items) > 0) {
    message("The following items failed to update: ", paste(failed_items, collapse = ", "))
  }

  if (offline_param) use_FReD_offline(TRUE)
}
