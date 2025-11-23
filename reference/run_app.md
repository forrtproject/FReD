# Run a shiny app within the package - potentially as an RStudio job

This function is used to run the shiny apps within the package

## Usage

``` r
run_app(
  offer_install = interactive(),
  app = "fred_explorer",
  in_background = NULL,
  auto_close = interactive(),
  port = 3838,
  timeout = 30
)
```

## Arguments

- offer_install:

  Should user be prompted to install required packages if they are
  missing?

- app:

  The name of the app folder within the package (i.e. within the inst
  folder)

- in_background:

  Should the app be run in the background (i.e. not block the R
  console)? Default to TRUE if RStudio is used.

- auto_close:

  Should the app be automatically ended when the browser is closed (or
  refreshed)?

- port:

  The port to run the app on (can usually be left at the default value)

- timeout:

  The timeout for waiting for the app to become available if launched in
  background (in seconds)

## Value

A shiny app
