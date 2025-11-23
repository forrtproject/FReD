# Assess Replication Outcomes Based on Various Criteria

This function evaluates the outcomes of replication studies against the
original studies using various statistical criteria (see below, and
[`vignette("success_criteria")`](http://forrt.org/FReD/articles/success_criteria.md)
for more details).

## Usage

``` r
assess_replication_outcome(es_o, n_o, es_r, n_r, criterion)
```

## Arguments

- es_o:

  Numeric. The effect size from the original study.

- n_o:

  Integer. The sample size of the original study.

- es_r:

  Numeric. The effect size from the replication study.

- n_r:

  Integer. The sample size of the replication study.

- criterion:

  Character. The criterion to use for assessing the replication outcome.
  Options include: "significance_r", "significance_agg",
  "consistency_ci", "consistency_pi", "homogeneity",
  "homogeneity_significance" and "small_telescopes".

## Value

A data frame with the outcome of the assessment based on the specified
criterion, with three columns: `outcome` (success, failure or 'OS not
significant'), `outcome_detailed` (a specific description of the
criterion) and `outcome_report` (usually the same as outcome, but
further broken out where there are distinct reasons for failure).

## Details

The function assesses the replication outcome using one of several
criteria:

- significance_r:

  Evaluates the statistical significance of the replication effect (and
  whether its direction is consistent with the original effect). If the
  original study was not significant, this is highlighted, as the
  criterion is meaningless

- significance_agg:

  Aggregates the effect sizes from the original and replication studies
  using a meta-analytic approach and assesses whether the combined
  effect is significantly different from zero.

- consistency_ci:

  Checks whether the *original* effect size falls within the confidence
  interval of the *replication* effect size, thus assessing consistency
  between the original and replication findings.

- consistency_pi:

  Evaluates whether the replication effect size falls within the
  prediction interval derived from the original study and the size of
  the replication sample. This accounts for the expected variability in
  replication results.

- homogeneity:

  Assesses whether the effects from the original and replication studies
  are homogeneous (i.e., consistent) using a heterogeneity test
  (Q-test).

- homogeneity_significance:

  Combines the assessment of homogeneity with the significance of the
  effect sizes. It checks whether the two effects are homogeneous and
  jointly significantly different from zero.

- small_telescopes:

  Tests whether the replication effect size is larger than the effect
  size that would have given the original study a power of 33%. Derived
  from Simonsohn (2015), the idea here is that replications should only
  count as successful if they indicate that the original study provided
  evidence (rather than a lucky guess).

## Examples

``` r
es_o <- 0.3  # Effect size from the original study
n_o <- 100   # Sample size of the original study
es_r <- 0.25 # Effect size from the replication study
n_r <- 120   # Sample size of the replication study
assess_replication_outcome(es_o, n_o, es_r, n_r, "significance_r")
#>   outcome                  outcome_detailed outcome_report
#> 1 success replication effect is significant        success
```
