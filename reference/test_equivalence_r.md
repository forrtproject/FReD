# Equivalence Test for a Correlation

This function performs an equivalence test for a given correlation \\ r
\\ against a specified smallest effect size of interest (SESOI). The
test assesses whether the observed correlation \\ r \\ is statistically
equivalent to a range defined by the SESOI.

## Usage

``` r
test_equivalence_r(r, n, sesoi, alpha = 0.05)
```

## Arguments

- r:

  Numeric. The observed correlation.

- n:

  Integer. The sample size.

- sesoi:

  Numeric. The smallest effect size of interest (SESOI) for the
  correlation. The SESOI is symmetric around zero, so the function tests
  equivalence within the range `-sesoi` to `sesoi`.

- alpha:

  Numeric. The significance level for the test. Default is 0.05.

## Value

Numeric. The p-value for the equivalence test. A p-value less than the
significance level `alpha` indicates that the observed correlation is
equivalent to the SESOI. When the return value is printed, a full
sentence detailing the result is provided.

## Details

The logic behind the test is based on the Two One-Sided Tests (TOST)
procedure. The function tests two null hypotheses: (1) that the true
correlation is less than or equal to the lower bound of the SESOI, and
(2) that the true correlation is greater than or equal to the upper
bound of the SESOI. If both null hypotheses can be rejected (i.e., if
the p-value is less than the significance level `alpha`), it is
concluded that the correlation is equivalent to the SESOI.

## Examples

``` r
r <- 0.3
n <- 100
sesoi <- 0.1
alpha <- 0.05
test_equivalence_r(r, n, sesoi, alpha)
#> The correlation of  0.3  is not statistically equivalent to the SESOI (±  0.1 ) (p =  0.98 ). 
```
