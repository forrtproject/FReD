# Read the FReD dataset

This function loads the FReD dataset into R. It merges the data from the
different sheets into one data frame.

## Usage

``` r
read_fred(
  data = get_param("FRED_DATA_FILE"),
  retain_es_as_character = TRUE,
  verbose = TRUE
)
```

## Arguments

- data:

  Path to the FReD dataset (defaults to current FReD data on OSF),
  unless the package is in offline mode
  ([`use_FReD_offline()`](http://forrt.org/FReD/reference/use_FReD_offline.md))

- retain_es_as_character:

  Should effect sizes be retained as character? Defaults to TRUE, so
  that coded test statistics with df can be converted to common metric.

- verbose:

  Should detailed messages be printed that highlight data conversion
  issues? Defaults to TRUE. FALSE is quiet mode, and NULL prints a
  summary of problems.

## Value

A data frame with the FReD dataset
