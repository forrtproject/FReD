# Update inbuilt data

If you set the package to work offline (`use_FReD_offline(TRUE)`) or if
any downloads fail, FReD will use offline data stored in the package.
Use this function if you want to update the data to the current state.
(NB: you can also use this if you want to work persistently with your
own version of the dataset).

## Usage

``` r
update_offline_data(
  data_file = get_param("FRED_DATA_FILE"),
  items = c("data", "data_description", "data_changelog", "citation")
)
```

## Arguments

- data_file:

  Path to FReD data file.

- items:

  Vector of items to update - defaults to all necessary items.
