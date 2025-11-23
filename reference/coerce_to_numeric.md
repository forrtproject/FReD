# Coerce specified variables to numeric and identify problematic values

Attempts to convert specified columns in a data frame to numeric. If
values cannot be coerced, the function optionally issues warnings or
summaries listing problematic values or IDs.

## Usage

``` r
coerce_to_numeric(df, numeric_vars, id_var, verbose = TRUE)
```

## Arguments

- df:

  A data frame containing the variables to be coerced.

- numeric_vars:

  A character vector of variable names to be coerced to numeric.

- id_var:

  A string specifying the name of the ID variable used to identify
  problematic entries.

- verbose:

  Logical or NULL. If TRUE, warns with IDs for problematic values. If
  FALSE, runs silently. If NULL, prints a summary per variable.

## Value

The input data frame with specified variables coerced to numeric (if
possible).
