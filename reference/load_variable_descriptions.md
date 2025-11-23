# Load variable descriptions

This reads names and variable descriptions of key variables from FReD.
This can used for subsetting and describing the dataset.

## Usage

``` r
load_variable_descriptions(
  sheet_name = "Key Variables",
  data = get_param("FRED_DATA_FILE")
)
```

## Arguments

- sheet_name:

  Path to the variable descriptions

- data:

  Path to the FReD dataset (defaults to current FReD data on OSF)

## Value

A data frame with variable names (`Variable`) and descriptions
(`Description`)
