# Setting Parameters for the FReD Package

Generally, the FReD package is designed to work with the latest FReD
dataset. However, you might want to use an older version, or even your
own. For such advanced use, you can set parameters by setting
environment variables manually or in your `.Renviron` file. The
following environment variables can be set (before loading the package):

## Details

- `FRED_DATA_URL`: The URL of the FReD dataset, needs to return the
  .xlsx file.

- `FRED_DATA_FILE`: The path to the .xlsx file, if you have downloaded
  it already (or want it to be saved to a particular location). If the
  file exists, it will be used - otherwise, the file will be downloaded
  and saved there.

- `RETRACTIONWATCH_DATA_FILE`: The path to the RetractionWatch database,
  if you have downloaded it already (or want it to be saved to a
  particular location). If the file exists, it will be used - otherwise,
  the file will be downloaded and saved there.

- `RETRACTIONWATCH_URL`: The URL to download the RetractionWatch
  database. Needs to return the .csv file.

- `FRED_OFFLINE`: Should FReD work offline (TRUE) or online (FALSE). If
  TRUE, FReD will not download the latest data every time it is loaded.
  Defaults to FALSE.

- `FRED_SUPPRESS_STARTUP_MENU`: Should the interactive menu checking for
  data updates be suppressed (TRUE) or shown (FALSE). If TRUE, all
  startup messages and interactive prompts related to offline data
  updates will be suppressed. Defaults to FALSE.

## Examples

``` r
## Not run:
Sys.setenv(FRED_DATA_URL = "http://your_url")
## End(Not run)
```
