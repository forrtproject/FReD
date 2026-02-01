#' Run a shiny app within the package - potentially as an RStudio job
#'
#' This function is used to run the shiny apps within the package
#'
#' @return A shiny app
#' @param offer_install Should user be prompted to install required packages if they are missing?
#' @param app The name of the app folder within the package (i.e. within the inst folder)
#' @param in_background Should the app be run in the background (i.e. not block the R console)? Default to TRUE if RStudio is used.
#' @param auto_close Should the app be automatically ended when the browser is closed (or refreshed)?
#' @param port The port to run the app on (can usually be left at the default value)
#' @param timeout The timeout for waiting for the app to become available if launched in background (in seconds)

run_app <- function(offer_install = interactive(), app = "fred_explorer", in_background = NULL,  auto_close = interactive(), port = 3838, timeout = 30) {
  assert_logical(in_background, null.ok = TRUE)
  assert_logical(auto_close, null.ok = TRUE)
  Sys.setenv("SHINY_FRED_AUTOCLOSE" = auto_close)

  check_port <- function(port) {
    con <- suppressWarnings(try(socketConnection(host = "127.0.0.1", port = port, open = "r+"), silent = TRUE))
    if (inherits(con, "try-error")) {
      return(FALSE)
    } else {
      close(con)
      return(TRUE)
    }
  }

  find_free_port <- function(start_port = 3838, end_port = 4000) {
    for (p in start_port:end_port) {
      if (!check_port(p)) {
        return(p)
      }
    }
    stop("No free port available in the specified range.")
  }

  wait_for_app <- function(url, timeout = 30) {
    start_time <- Sys.time()
    sp <- cli::make_spinner(template = "{spin} Waiting for app to launch - please allow up to 15 seconds.")
    while (Sys.time() - start_time < timeout) {
      sp$spin()
      response <- try(httr::GET(url), silent = TRUE)
      if (!inherits(response, "try-error") && httr::status_code(response) == 200) {
        sp$finish()
        return(TRUE)
      }
      Sys.sleep(.5)
    }
    sp$finish()
    stop("App did not become ready within the timeout period. Please have a look at the background job output to see why and report any errors. If you are on a slow internet connection, consider setting a longer timeout in the function call, or using the dataset included with the package by running `use_FReD_offline()`")
  }

  if (check_port(port)) {
    message(glue::glue("Port {port} is already in use. The app may be running already."))
    user_input <- utils::menu(c("Open that location in the browser", "Use a new port", "Abort"), title = "Select an option:")
    if (user_input == 1) {
      browseURL(url = glue::glue("http://127.0.0.1:{port}"))
      return(invisible(NULL))
    } else if (user_input == 2) {
      port <- find_free_port()
    } else {
      cli::cli_inform("Aborted by user.")
      return(invisible(NULL))
    }
  }

  if (is.null(in_background)) {
    in_background <- check_rstudio()
  } else if (in_background == TRUE) {
    in_background <- check_rstudio()
    if (!in_background) {
      message("Not running within RStudio (or `rstudioapi` package not available), so app will launch in R Console")
    }
  }

  # find and launch the app
  appDir <- system.file(app, package = "FReD")

  if (!in_background) {
    if (is.null(port)) {
      port <- find_free_port()
    }
    if (auto_close) cli::cli_inform("NB: The app will stop automatically when you close or refresh the browser window.")
    shiny::runApp(appDir, display.mode = "normal", port = port)
  } else {
    f <- tempfile(fileext = ".R")
    launch_code <- glue::glue("
    tryCatch({{library(FReD)}}, error = function(e) {{
      message('FReD package could not be loaded. Try re-installing FReD.', call. = FALSE)
    }})
    appDir <- '{appDir}'
    if (appDir == '') {{
      stop('Could not find the app directory. Try re-installing FReD.', call. = FALSE)
    }}
    shiny::runApp(appDir = '{appDir}', display.mode = 'normal', quiet = TRUE, host = '127.0.0.1', port = {port})
    ")
    writeLines(launch_code, f)

    tryCatch(eval(parse(text = glue::glue("invisible(rstudioapi::jobRunScript(path = '{f}', importEnv = TRUE, exportEnv = 'R_GlobalEnv', name = '{app} Shiny app'))"))), error = function(e) {
      message("App could not be loaded in background - likely due to an issue with tempfile permissions. Starting in Console instead.")
      run_app(offer_install = offer_install, app = app, in_background = FALSE, port = port)
      return(TRUE)
    })

    rstudioapi::executeCommand(commandId = "activateConsole")

    app_url <- glue::glue("http://127.0.0.1:{port}")
    wait_for_app(app_url, timeout = timeout)

    browseURL(url = glue::glue("http://127.0.0.1:{port}"))

    cli::cli_inform(c(glue::glue("App launched in the background."),
                      ifelse(auto_close, "NB: The app will stop automatically when you close or refresh the browser window.",
                      glue::glue("If you want to reopen it later, go to http://127.0.0.1:{port} in your browser. To stop the app, click on STOP in the Background Jobs pane."))))
  }
}

check_rstudio <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    if (rstudioapi::isAvailable()) {
      return(TRUE)
    }
  }
  FALSE
}

#' Run the Replication Explorer
#'
#' Running this function will launch the FReD Replication Explorer shiny app
#'
#' @return Replication Explorer shiny app
#' @inheritParams run_app
#' @export
#' @examples
#' if (interactive()) {
#'   # To run the Replication Explorer app:
#'   run_explorer()
#' }
run_explorer <- function(offer_install = interactive(), in_background = NULL, auto_close = interactive(), port = 3838, timeout = 30) {
  run_app(offer_install = offer_install, app = "fred_explorer", in_background = in_background, auto_close = auto_close, port = port, timeout = timeout)
}


#' Run the Replication Annotator
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function previously launched a local Shiny app but now opens the
#' hosted web version of the FReD Annotator.
#'
#' @return Opens the web annotator in the default browser (invisibly returns NULL).
#' @export
#' @examples
#' if (interactive()) {
#'   run_annotator()
#' }
run_annotator <- function() {
  .Deprecated(msg = "The local annotator app has been deprecated. Opening the web version.")
  utils::browseURL("http://forrt.org/apps/fred_annotator.html")
  invisible(NULL)
}

#' Get the date of last modification
#'
#' This function returns the date of last modification to be displayed in an app. It represents
#' either the date when a specific element of the app was last modified, or the date of the most
#' recent modification to the package DESCRIPTION file (which represents broader updates).
#'
#' @param app_folder The name of the app folder within the package (i.e. within the inst folder)

get_last_modified <- function(app_folder = "fred_explorer") {
  # Get the path to the app folder within the package
  appDirPath <- system.file(app_folder, package = "FReD")
  appDirPath <- normalizePath(appDirPath)

  # List all files in the directory recursively and get their last modified times
  files <- list.files(appDirPath, recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) {
    warning("No files found in the directory")
    most_recent_file_time <- 0
  } else {
    file_times <- lapply(files, function(f) file.info(f)$mtime)
    file_times_combined <- do.call(c, file_times)
    most_recent_file_time <- max(file_times_combined)
  }

  # Get the last modified time of the DESCRIPTION file
  descPath <- system.file("DESCRIPTION", package = "FReD")
  descPath <- normalizePath(descPath)
  desc_time <- file.info(descPath)$mtime

  # Find the most recent modification time between files and DESCRIPTION
  most_recent_time <- max(c(most_recent_file_time, desc_time))

  format(most_recent_time, "%d %B, %Y")
}
