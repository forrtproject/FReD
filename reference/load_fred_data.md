# Load the FReD dataset

This function loads the FReD dataset into R, and conducts variable
transformations to prepare for analyses.

## Usage

``` r
load_fred_data(data = get_param("FRED_DATA_FILE"), verbose = TRUE)
```

## Arguments

- data:

  Path to the FReD dataset (defaults to current FReD data on OSF),
  unless the package is in offline mode
  ([`use_FReD_offline()`](http://forrt.org/FReD/reference/use_FReD_offline.md))

- verbose:

  Should detailed messages be printed that highlight data conversion
  issues? Defaults to TRUE. FALSE is quiet mode, and NULL prints a
  summary of problems.

## Value

A data frame with the processed FReD dataset
