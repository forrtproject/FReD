# Code replication outcomes

Different frameworks have been proposed to code replication outcomes.
Here we code:

- signal vs no-signal (i.e., significant vs all others)

- consistent vs inconsistent (i.e., replication confidence interval
  overlaps original point estimate) Based on
  https://etiennelebel.com/documents/lebeletal%282018,ampss%29a-unified-framework-to-quantify-the-credibility-of-scientific-findings.pdf

- and success vs failure (significant *in right direction* vs all
  others)

## Usage

``` r
code_replication_outcomes(
  fred_data,
  es_original = "es_original",
  p_original = "p_value_original",
  p_replication = "p_value_replication",
  ci_lower_replication = "ci.lower_replication",
  ci_upper_replication = "ci.upper_replication",
  es_replication = "es_replication"
)
```

## Arguments

- fred_data:

  FReD dataset

- es_original:

  Character. Name of original effect size column.

- p_original:

  Character. Significance of original effect size.

- p_replication:

  Character. Significance of replication effect size.

- ci_lower_replication:

  Character. Lower bound of replication confidence interval.

- ci_upper_replication:

  Character. Upper bound of replication confidence interval.

- es_replication:

  Character. Name of replication effect size column.

## Value

Augmented FReD dataset with replication outcome columns, including
`signal`
