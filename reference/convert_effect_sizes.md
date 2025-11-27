# Convert effect sizes to a common metric (Pearson's r)

Converts a variety of effect sizes and test statistics to Pearson's r
for comparability. Effect sizes that cannot be meaningfully converted
are returned as `NA` with appropriate warnings.

## Usage

``` r
convert_effect_sizes(es_values, es_types, quiet = FALSE)
```

## Arguments

- es_values:

  Numeric vector of effect sizes, or character vector for test
  statistics formatted in APA style (e.g., `"t(10) = 2.5"`).

- es_types:

  Character vector of effect size types (case-insensitive). See Details
  for accepted values. Unrecognized types trigger a warning.

- quiet:

  Logical. If `TRUE`, suppresses warnings about unknown effect size
  types and messages about non-convertible or missing values.

## Value

Numeric vector of effect sizes converted to Pearson's r. Returns `NA`
for values that cannot be converted or are missing.

## Details

### Supported Effect Sizes

The following effect sizes can be converted to r. Accepted `es_types`
values are case-insensitive.

- Pearson's r / phi:

  `"r"`, `"phi"`, `"φ"`  
  **Conversion:** Returned as-is.  
  **Sign:** Preserved.

- R-squared:

  `"r2"`, `"r^2"`, `"r²"`, `"r-square"`  
  **Conversion:** \\r = \sqrt{R^2}\\  
  **Sign:** Always positive (R² is non-directional).

- Cohen's d / Hedges' g:

  `"d"`, `"cohen's d"`, `"hedges' g"`, `"smd"`  
  **Conversion:** \\r = \frac{d}{\sqrt{d^2 + 4}}\\  
  **Sign:** Preserved (if input d is negative, r is negative).

- Odds ratio:

  `"or"`, `"odds ratio"`  
  **Conversion:** \\d = \ln(OR) \cdot \sqrt{3} / \pi\\, then d to r.  
  **Sign:** Preserved (OR \< 1 implies negative r).

- Eta-squared:

  `"etasq"`, `"eta^2"`, `"η²"`  
  **Conversion:** \\d = 2\sqrt{\frac{\eta^2}{1 - \eta^2}}\\, then d to
  r.  
  **Sign:** Always positive.

- Cohen's f / f²:

  `"f"`, `"cohen's f"`, `"f2"`, `"f^2"`, `"f²"`  
  **Conversion (f):** \\d = 2f\\, then d to r.  
  **Conversion (f²):** \\R^2 = \frac{f^2}{1 + f^2}\\, then \\r =
  \sqrt{R^2}\\.  
  **Sign:** Always positive.

### Test Statistics

When `es_types` is `"test statistic"` the function parses APA-formatted
strings in `es_values`.

- t-test:

  **Format:** `"t(df) = value"`  
  **Conversion:** \\r = \frac{t}{\sqrt{t^2 + df}}\\  
  **Sign:** Preserved.

- F-test:

  **Format:** `"F(df1, df2) = value"`  
  **Constraint:** df1 must equal 1.  
  **Conversion:** \\t = \sqrt{F}\\, then t to r.  
  **Sign:** Always positive.

- z-test:

  **Format:** `"z = value, N = value"`  
  **Conversion:** \\r = \frac{z}{\sqrt{z^2 + N}}\\  
  **Sign:** Preserved.

- Chi-squared:

  **Format:** `"x2(1, N = value) = value"`  
  **Constraint:** df must equal 1.  
  **Conversion:** \\r = \sqrt{\frac{\chi^2}{N}}\\  
  **Sign:** Always positive.

### Non-Convertible Effect Sizes

The following effect sizes cannot be reliably converted to r and return
`NA`: partial eta-squared, Cramer's V, Cohen's h, Cohen's \\d_z\\,
regression coefficients (`"b"`, `"beta"`), and semi-partial
correlations.

## References

Sánchez-Meca, J., Marín-Martínez, F., & Chacón-Moscoso, S. (2003).
Effect-size indices for dichotomized outcomes in meta-analysis.
*Psychological Methods*, *8*(4), 448–467.
[doi:10.1037/1082-989X.8.4.448](https://doi.org/10.1037/1082-989X.8.4.448)
