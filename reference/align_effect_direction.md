# Align effect direction

Ensure that all original effects are coded as positive, and that
replication effects are coded in the same direction (so that they
*would* be positive if successful.

## Usage

``` r
align_effect_direction(
  fred_data,
  es_original = "es_original",
  es_replication = "es_replication"
)
```

## Arguments

- fred_data:

  FReD dataset

- es_original:

  Character. Name of original effect size column.

- es_replication:

  Character. Name of replication effect size column.

## Value

Augmented FReD dataset with aligned effect directions.
