# Convert effect sizes to common metric (r)

Takes vectors of effect sizes and their types and converts them to a
common metric (r). It also converts test statistics (e.g., *t*, *F* with
1 degree of freedom in the numerator, *z* with *N*, and \\\chi^2\\ with
1 degree of freedom and *N*) to *r*. Other test statistics cannot be
consistently converted, so are returned as `NA`.

## Usage

``` r
convert_effect_sizes(es_values, es_types, quiet = FALSE)
```

## Arguments

- es_values:

  Numeric vector of effect sizes

- es_types:

  Character vector of effect size types (types/wordings that are not
  supported are flagged in warning)

- quiet:

  Logical. Should dataset warnings (unknown effect sizes and values not
  convertible to numeric) and status messages be suppressed?

## Value

Numeric vector of effect sizes in common metric (r)

## Details

Conversion formulas:

- r: \\r\\

- r^2: \\r = \sqrt{r^2}\\

- d / Cohen's g: \\r = \frac{d}{\sqrt{d^2 + 4}}\\

- odds ratio: \\d = \log(\mathrm{OR}) \cdot \sqrt{3} / \pi\\, then \\r =
  \frac{d}{\sqrt{d^2 + 4}}\\

- eta^2: \\d = 2 \cdot \sqrt{\frac{\eta}{1 - \eta}}\\, then \\r =
  \frac{d}{\sqrt{d^2 + 4}}\\

- Cohen's f: \\d = 2 \cdot f\\, then \\r = \frac{d}{\sqrt{d^2 + 4}}\\

- t(df): \\r = \frac{t}{\sqrt{t^2 + df}}\\

- F(1, df2): convert to t via \\t = \sqrt{F}\\, then \\r =
  \frac{t}{\sqrt{t^2 + df2}}\\

- z with N: \\r = \frac{z}{\sqrt{z^2 + N}}\\

- \\\chi^2\\(1, N): \\r = \sqrt{\frac{\chi^2}{N}}\\

Accepted test statistic formats (case-insensitive, with optional
spaces):

- `t(10) = 2.5`

- `F(1, 20) = 4.5` (only where the first df is 1)

- `z = 2.81, N = 34`

- `χ2(1, N = 12) = 5` or `x2(1, N = 12) = 5` (only where the first df is
  1)

## Examples

``` r
convert_effect_sizes(
  c("t(10) = 2.5", "F(1, 20) = 4.5", "z = 2.81, N = 34", "χ2(1, N = 12) = 5"),
  rep("test statistic", 4)
)
#> [1] 0.6201737 0.4285714 0.4341297 0.6454972
```
