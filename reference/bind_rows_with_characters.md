# Bind Rows with Character Columns

A wrapper for
[`dplyr::bind_rows`](https://dplyr.tidyverse.org/reference/bind_rows.html)
that ensures any columns that are character in one of the data frames
are converted to character in all data frames before binding. This
reduces the likelihood of errors, but does not give up on type-checking
entirely.

## Usage

``` r
bind_rows_with_characters(..., .id = NULL)
```

## Arguments

- ...:

  Data frames to combine. Each argument should be a data frame.

- .id:

  An optional string that will be used to create a column in the output.
  If supplied, this will create a new column with the name given by
  `.id`, and each row will have a value corresponding to the argument
  name from which it came.

## Value

A data frame created by binding the rows of the input data frames.
