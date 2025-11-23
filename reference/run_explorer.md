# Run the Replication Explorer

Running this function will launch the FReD Replication Explorer shiny
app

## Usage

``` r
run_explorer(
  offer_install = interactive(),
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

Replication Explorer shiny app

## Examples

``` r
if (interactive()) {
  # To run the Replication Explorer app:
  run_explorer()
}
```
