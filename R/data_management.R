
#' Returns an inbuilt data item
#'
#' This is used then the package is set to work offline, or when the data cannot be loaded.
#'
#' @param item Which inbuilt item to return?
#' @noRd

return_inbuilt <- function(item = c("data", "data_description", "changelog", "citation")) {
  assert_choice(item, c("data", "data_description", "changelog", "citation"))
  data <- system.file("extdata", "snapshot", paste0(item, ".RDS"), package = "FReD")
  data <- readRDS(data)
  if (!get_param("FRED_OFFLINE")) {
    last_updated <- format(attr(data, "last_updated"), "%d-%m-%Y %I:%M%p")
    message("Using inbuilt ", item, " last updated on ", last_updated, ". This is likely because of an issue with your internet connection, or with the online data source. If this is unexpected and persists, please report this issue on GitHub.")
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
#' @return A data frame with the FReD dataset

read_fred <- function(data = get_param("FRED_DATA_FILE")) {

  if (get_param("FRED_OFFLINE")) return(return_inbuilt("data"))

  tryCatch({

    red <- safe_read_xl(data, url = get_param("FRED_DATA_URL"), sheet = "Data") # .xlsx file
    red <- red[-(1:2), ] # exclude labels and "X" column
    forrt  <- safe_read_xl(data, url = get_param("FRED_DATA_URL"), sheet = "FORRT R&R (editable)", startRow = 1)
    forrt <- forrt[-(1:2), ] # exclude labels and "X" column
    forrt <- forrt[!(forrt$doi_original %in% red$doi_original), ] # exclude forrt entries of original study that already appear in FReD (based on DOIs)

    # additional studies
    as <-  safe_read_xl(data, url = get_param("FRED_DATA_URL"), sheet = "Additional Studies to be added", startRow = 2)
    as$id <- paste("uncoded_studies_", rownames(as), sep = "")
    as <- as[as$`Study.listed.in.ReD?` != "1.0", ] # exclude additional studies that are already listed in the main dataset
    as <- as[!is.na(as$doi_original), ] # exclude studies for which doi_original is unavailable because they will not be findable in the annotator anyway

    numeric_variables <- c("n_original", "n_replication", "es_orig_value", "es_rep_value",
                           "validated", "published_rep", "same_design", "same_test",
                           "original_authors",
                           "significant_original", "significant_replication", "power",
                           "es_orig_RRR", "es_rep_RRR")

    # Function to coerce to numeric and track problematic values
    coerce_to_numeric <- function(df, numeric_vars, id_var) {
      problematic_entries <- list()

      for (var in numeric_vars) {
        # Identify problematic values
        problematic_rows <- which(!is.na(df[[var]]) & is.na(suppressWarnings(as.numeric(df[[var]]))))

        if (length(problematic_rows) > 0) {
          problematic_entries[[var]] <- df[problematic_rows, id_var, drop = FALSE]
        }

        # Coerce to numeric
        df[[var]] <- suppressWarnings(as.numeric(df[[var]]))
      }

      # If there are any problematic entries, generate a warning
      if (length(problematic_entries) > 0) {
        warning_message <- "The following fields contain values that could not be coerced to numeric:\n"
        for (var in names(problematic_entries)) {
          warning_message <- paste0(warning_message, "Variable '", var, "' has issues in IDs: ",
                                    paste(problematic_entries[[var]][[id_var]], collapse = ", "), "\n")
        }
        warning(warning_message)
      }

      df

    }

  # Assuming 'red' and 'forrt' have a unique ID column named "id"
  red <- coerce_to_numeric(red, numeric_variables, id_var = "id")
  forrt <- coerce_to_numeric(forrt, numeric_variables, id_var = "id")

  # merge the data, aligning column types where one is character (as empty colums are imported as numeric)
  return(bind_rows_with_characters(red, forrt, as))

    }, error = function(e) {
      return(return_inbuilt())
    })

}


#' Clean variables
#' Perform some specific operations (e.g., recoding some NA as "") required to get the Shiny apps to work.
#' This may be a temporary solution, as much of it should likely be handled through validation in the data sheet, and at import time.

#' @param fred_data FReD dataset
#' @return FReD dataset with cleaned variables

clean_variables <- function(fred_data) {

  # recode variables for app to work
  fred_data$pc_tags <- NA
  fred_data$pc_contributors <- NA
  fred_data$description <- ifelse(is.na(fred_data$description), "", fred_data$description)
  fred_data$contributors <- ifelse(is.na(fred_data$contributors), fred_data$pc_contributors, fred_data$contributors)
  fred_data$tags <- ifelse(is.na(fred_data$tags), fred_data$pc_tags, fred_data$tags)
  fred_data$subjects <- NA
  fred_data$description <- ifelse(is.na(fred_data$description), fred_data$pc_title, fred_data$description)

  fred_data$closeness <- NA
  fred_data$result <- ifelse(fred_data$result == "0", NA, fred_data$result)

  fred_data$result

  # compute year the original study was published (match 1800-2099 only, and require consecutive numbers)
  fred_data$orig_year <- as.numeric(gsub(".*((18|19|20)\\d{2}).*", "\\1", fred_data$ref_original))

  # # delete duplicates and non-replication studies
  fred_data <- fred_data[fred_data$notes != "duplicate" | is.na(fred_data$notes), ] # ADDED: study exclusions due to duplicates
  fred_data <- fred_data[fred_data$notes != "No actual replication conducted" | is.na(fred_data$notes), ] # ADDED: some registrations had no corresponding replication study

  # remove entries with reasons for exclusions
  fred_data <- fred_data[is.na(fred_data$exclusion), ]

  # Collapse validated categories (# 2: error detected and corrected)
  fred_data$validated <- ifelse(fred_data$validated == 1 | fred_data$validated == 2, 1, fred_data$validated)

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

' Update inbuilt data
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
      } else if (item == "changelog") {
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
