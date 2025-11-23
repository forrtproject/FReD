# Test statistic transformations
library(testthat)

test_that("convert_effect_sizes correctly converts t(df) to r", {
  es_values <- c("t(10) = 2.5", "t(30) = -1.8", "t(5) = 0")
  es_types <- c("test statistic", "test statistic", "test statistic")

  expected_r <- c(
    2.5 / sqrt(2.5^2 + 10),  # Matches output from effectsize::t_to_r(2.5, 10)
    -1.8 / sqrt((-1.8)^2 + 30),
    0 / sqrt(0^2 + 5)
  )

  result <- convert_effect_sizes(es_values, es_types)
  expect_equal(result, expected_r, tolerance = 1e-6)
})

test_that("convert_effect_sizes correctly converts F(df1=1, df2) to r", {
  es_values <- c("F(1, 20) = 4.5", "F(1, 50) = 2.0")
  es_types <- c("test statistic", "test statistic")

  expected_r <- c(
    sqrt(4.5) / sqrt((sqrt(4.5))^2 + 20), # Matches output from effectsize::F_to_r(4.5, 1, 20)
    sqrt(2.0) / sqrt((sqrt(2.0))^2 + 50)
  )

  result <- convert_effect_sizes(es_values, es_types)
  expect_equal(result, expected_r, tolerance = 1e-6)
})

test_that("convert_effect_sizes returns NA for F(df1 > 1, df2) and F < 1", {
  es_values <- c("F(2, 15) = 3.2", "F(3, 40) = 5.1", "F < 1")
  es_types <- c("test statistic", "test statistic", "test statistic")

  result <- convert_effect_sizes(es_values, es_types)
  expect_true(all(is.na(result)))  # Should return NA for non-convertible F-tests
})

test_that("convert_effect_sizes returns NA for invalid test statistic formats", {
  es_values <- c("t(df) = 2.5", "F(10) = 4.5", "f(1,20) 4.5", "random text")
  es_types <- c("test statistic", "test statistic", "test statistic", "test statistic")

  result <- convert_effect_sizes(es_values, es_types)
  expect_true(all(is.na(result)))  # All should fail conversion
})

test_that("convert_effect_sizes converts z-scores with N", {
  es_values <- c(" z = 2.81, N = 34", "z = 3.1, N = 68")
  es_types <- c("test statistic", "test statistic")

  expected_r <- c(
    2.81 / sqrt(2.81^2 + 34), # Matches output from effectsize::z_to_r(2.81, 34)
    3.1 / sqrt(3.1^2 + 68)
  )

  result <- convert_effect_sizes(es_values, es_types)
  expect_equal(result, expected_r, tolerance = 1e-6)
})

test_that("convert_effect_sizes converts chi-square(1, N) to r", {
  es_values <- c("x2(1, N = 12) = 5", "X2(1,N=50)=3.84", "χ2(1, N = 30) = 4")
  es_types <- rep("test statistic", length(es_values))

  expected_r <- c(
    sqrt(5 / 12),
    sqrt(3.84 / 50),
    sqrt(4 / 30)
  )

  result <- convert_effect_sizes(es_values, es_types)
  expect_equal(result, expected_r, tolerance = 1e-6)
})


test_that("convert_effect_sizes correctly returns r for types 'r' and 'phi'", {
  es_values <- c("0.5", "-0.7")
  es_types <- c("r", "phi")
  result <- convert_effect_sizes(es_values, es_types)
  expect_equal(result, c(0.5, -0.7), tolerance = 1e-6)
})

test_that("convert_effect_sizes correctly converts r2 to r", {
  es_values <- c("0.25", "0.49")
  es_types <- c("r2", "r2")
  expected_r <- c(sqrt(0.25), sqrt(0.49))
  result <- convert_effect_sizes(es_values, es_types)
  expect_equal(result, expected_r, tolerance = 1e-6)
})

test_that("convert_effect_sizes correctly converts d to r and retains sign", {
  es_values <- c("0.5", "-0.5")
  es_types <- c("d", "d")
  # Expected conversion: d * sqrt(d^2 + 4)
  expected_r <- c( 0.5 / sqrt(0.5^2 + 4), -0.5 / sqrt(0.5^2 + 4)) # Matches output from effectsize::d_to_r(0.5)
  result <- convert_effect_sizes(es_values, es_types)
  expect_equal(result, expected_r, tolerance = 1e-6)
})

test_that("convert_effect_sizes correctly converts odds ratios to r", {
  es_values <- c("2", "0.5")
  es_types <- c("or", "or")
  ds1 <- log(2) * sqrt(3) / pi
  ds2 <- log(0.5) * sqrt(3) / pi
  expected_r <- c(
    ds1 / sqrt((ds1^2 + 4)),
    ds2 / sqrt((ds2^2 + 4))
  ) # Matches effectsize::oddsratio_to_r(c(.5, 2))
  result <- convert_effect_sizes(es_values, es_types)
  expect_equal(result, expected_r, tolerance = 1e-6)
})

test_that("convert_effect_sizes correctly converts eta squared to r", {
  es_values <- c("0.2", "0.8")
  es_types <- c("etasq", "etasq")
  ds1 <- 2 * sqrt(0.2 / (1 - 0.2))
  ds2 <- 2 * sqrt(0.8 / (1 - 0.8))
  expected_r <- c(
    ds1 / sqrt((ds1^2 + 4)),
    ds2 / sqrt((ds2^2 + 4))
  ) # Matches esc::pearsons_r(eta = c(.2, .8))
  result <- convert_effect_sizes(es_values, es_types)
  expect_equal(result, expected_r, tolerance = 1e-6)
})

test_that("convert_effect_sizes correctly converts f to r", {
  es_values <- c("0.3", "1")
  es_types <- c("f", "f")
  ds1 <- 2 * 0.3
  ds2 <- 2 * 1
  expected_r <- c(
    ds1 / sqrt((ds1^2 + 4)),
    ds2 / sqrt((ds2^2 + 4))
  ) # Matches esc::pearsons_r(f = c(.3, 1))
  result <- convert_effect_sizes(es_values, es_types)
  expect_equal(result, expected_r, tolerance = 1e-6)
})

test_that("convert_effect_sizes warns for unknown effect types and returns NA", {
  es_values <- c("0.5")
  es_types <- c("unknown type")
  expect_warning(result <- convert_effect_sizes(es_values, es_types), "Unknown effect size types")
  expect_true(is.na(result))
})

test_that("convert_effect_sizes handles missing values", {
  es_values <- c(NA, "0.5")
  es_types <- c("r", NA)
  result <- convert_effect_sizes(es_values, es_types)
  expect_true(is.na(result[1]))
  expect_true(is.na(result[2]))
})

# Omnibus test to ensure that order is maintained etc
test_that("convert_effect_sizes works for all supported effect sizes", {
  es_values <- c(
    "0.5",             # r
    "-0.3",            # phi
    "0.16",            # r2
    "0.8",             # d (positive)
    "-0.8",            # d (negative)
    "2",               # odds ratio
    "0.3",             # etasq
    "0.5",             # f
    "t(10) = 2.5",     # test statistic: t-test
    "F(1, 20) = 4.5",   # test statistic: F-test (df1 == 1)
    "z = 2.81, N = 34",  # test statistic: z-test
    "χ2(1, N = 12) = 5", # test statistic: chi-square with df1 == 1
    NA,
    2,
    1
  )

  es_types <- c(
    "r",
    "phi",
    "r2",
    "d",
    "d",
    "odds ratio",
    "etasq",
    "f",
    "test statistic",
    "test statistic",
    "test statistic",
    "test statistic",
    "invalid",
    "none",
    NA
  )

  expected_r <- c(
    0.5,                                # r
    -0.3,                               # phi
    sqrt(0.16),                         # r2: sqrt(0.16) = 0.4
    0.8 / sqrt(0.8^2 + 4),                # d (positive)
    -0.8 / sqrt(0.8^2 + 4),               # d (negative, sign retained)
    { ds <- log(2) * sqrt(3) / pi; sqrt(ds^2 / (ds^2 + 4)) },  # odds ratio
    { ds <- 2 * sqrt(0.3 / (1 - 0.3)); sqrt(ds^2 / (ds^2 + 4)) },# etasq
    { ds <- 2 * 0.5; sqrt(ds^2 / (ds^2 + 4)) },               # f
    2.5 / sqrt(2.5^2 + 10),               # t-test
    { t_val <- sqrt(4.5); t_val / sqrt(t_val^2 + 20) },         # F-test
    2.81 / sqrt(2.81^2 + 34),              # z-test
    sqrt(5 / 12),                          # chi-square test with df1 == 1
    NA,
    NA,
    NA
  )

  expect_warning(result <- convert_effect_sizes(es_values, es_types))
  expect_equal(result, expected_r, tolerance = 1e-6)
})
