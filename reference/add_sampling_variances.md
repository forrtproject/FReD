# Add confidence intervals

Adds sampling variances for common-metric effect sizes (r) to the FReD
dataset, using metafor::escalc

## Usage

``` r
add_sampling_variances(
  fred_data,
  es_value_columns = c("es_original", "es_replication"),
  N_columns = c("n_original", "n_replication"),
  vi_columns = c("vi_original", "vi_replication")
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
