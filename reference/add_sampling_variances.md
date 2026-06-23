# Add confidence intervals

Adds sampling variances for common-metric effect sizes (r) to the FReD
dataset, using metafor::escalc

## Usage

``` r
add_sampling_variances(
  fred_data,
  es_value_columns = c("es_o", "es_r"),
  N_columns = c("n_o", "n_r"),
  vi_columns = c("vi_o", "vi_r")
)
```

## Arguments

- fred_data:

  FReD dataset

- es_value_columns:

  Character vector of column names with correlation values

- N_columns:

  Character vector of column names with sample sizes

- vi_columns:

  Character vector of target columns for sampling variances

## Value

FReD dataset with additional columns for sampling variances (metafor's
`vi`)
