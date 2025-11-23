# Add power

Estimates the power of the replication study, given the original effect
size, the sample size of the replication study, and the usual focus on a
two-tailed test.

## Usage

``` r
add_replication_power(
  fred_data,
  es_original = "es_original",
  N_replication = "n_replication",
  power_column = "power_r"
)
```

## Arguments

- fred_data:

  FReD dataset

- es_original:

  Character. Name of original effect size column.

- N_replication:

  Character. Name of replication sample size column.

- power_column:

  Character. Name of target column for power.

## Value

Augmented FReD dataset with power column.
