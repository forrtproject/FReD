# Calculate Prediction Interval for a Correlation Coefficient

This function calculates the prediction interval for a correlation
coefficient based on the Fisher's Z-transformation. It uses the approach
proposed in the supplementary materials of Patil et al. (2016).

## Usage

``` r
calculate_prediction_interval(r, n_o, n_r = n_o, confidence_level = 0.95)
```

## Arguments

- r:

  Numeric. The correlation coefficient from the original study.

- n_o:

  Integer. The sample size of the original study.

- n_r:

  Integer. The sample size of the replication study. Defaults to the
  same as `n_o`.

- confidence_level:

  Numeric. The confidence level for the prediction interval. Defaults to
  0.95.

## Value

A named numeric vector with two elements:

- lower:

  The lower bound of the prediction interval.

- upper:

  The upper bound of the prediction interval.

## References

Patil, P., Peng, R. D., & Leek, J. T. (2016). A statistical framework
for enhancing reproducibility in data analysis. *Perspectives on
Psychological Science, 11*(5), 633-640.
<https://doi.org/10.1177/1745691616646366>

## Examples

``` r
# Example usage:
r <- 0.5   # Correlation coefficient
n_o <- 30  # Sample size of the original study
n_r <- 30  # Sample size of the replication study

calculate_prediction_interval(r, n_o, n_r)
#> $lower
#> [1] 0.01587018
#> 
#> $upper
#> [1] 0.7942133
#> 
```
