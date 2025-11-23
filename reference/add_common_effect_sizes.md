# Add common effect size columns to FReD dataset

Converts original and replication effect sizes to common metric (r) and
adds them to the dataset.

## Usage

``` r
add_common_effect_sizes(
  fred_data,
  es_value_columns = c("es_orig_value", "es_rep_value"),
  es_type_columns = c("es_orig_estype", "es_rep_estype"),
  es_common_names = c("es_original", "es_replication"),
  coalesce_values = TRUE
)
```

## Arguments

- fred_data:

  FReD dataset

- es_value_columns:

  Character vector of column names for effect sizes

- es_type_columns:

  Character vector of column names for effect size types

- es_common_names:

  Names of columns where effect sizes should be saved. If
  coalesce_values is TRUE, existing values in these columns will be
  retained *if* no conversion is possible.

- coalesce_values:

  Logical. Should existing values in es_type_columns be retained?

## Value

FReD dataset with additional columns for common effect sizes
