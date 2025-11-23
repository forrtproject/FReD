#' Convert effect sizes to common metric (r)
#'
#' Takes vectors of effect sizes and their types and converts them to a common metric (r).
#' It also converts test statistics (e.g., *t*, *F* with 1 degree of freedom in the numerator,
#' *z* with *N*, and \eqn{\chi^2} with 1 degree of freedom and *N*) to *r*. Other test statistics cannot be
#' consistently converted, so are returned as `NA`.
#'
#' Conversion formulas:
#' - r: \eqn{r}
#' - r^2: \eqn{r = \sqrt{r^2}}
#' - d / Cohen's g: \eqn{r = \frac{d}{\sqrt{d^2 + 4}}}
#' - odds ratio: \eqn{d = \log(\mathrm{OR}) \cdot \sqrt{3} / \pi}, then \eqn{r = \frac{d}{\sqrt{d^2 + 4}}}
#' - eta^2: \eqn{d = 2 \cdot \sqrt{\frac{\eta}{1 - \eta}}}, then \eqn{r = \frac{d}{\sqrt{d^2 + 4}}}
#' - Cohen's f: \eqn{d = 2 \cdot f}, then \eqn{r = \frac{d}{\sqrt{d^2 + 4}}}
#' - t(df): \eqn{r = \frac{t}{\sqrt{t^2 + df}}}
#' - F(1, df2): convert to t via \eqn{t = \sqrt{F}}, then \eqn{r = \frac{t}{\sqrt{t^2 + df2}}}
#' - z with N: \eqn{r = \frac{z}{\sqrt{z^2 + N}}}
#' - \eqn{\chi^2}(1, N): \eqn{r = \sqrt{\frac{\chi^2}{N}}}
#'
#' Accepted test statistic formats (case-insensitive, with optional spaces):
#' - `t(10) = 2.5`
#' - `F(1, 20) = 4.5` (only where the first df is 1)
#' - `z = 2.81, N = 34`
#' - `χ2(1, N = 12) = 5` or `x2(1, N = 12) = 5` (only where the first df is 1)
#'
#' @param es_values Numeric vector of effect sizes
#' @param es_types Character vector of effect size types (types/wordings that are not supported are flagged in warning)
#' @param quiet Logical. Should dataset warnings (unknown effect sizes and values not convertible to numeric)
#' and status messages be suppressed?
#' @return Numeric vector of effect sizes in common metric (r)
#' @export
#' @examples
#' convert_effect_sizes(
#'   c("t(10) = 2.5", "F(1, 20) = 4.5", "z = 2.81, N = 34", "χ2(1, N = 12) = 5"),
#'   rep("test statistic", 4)
#' )

convert_effect_sizes <- function(es_values, es_types, quiet = FALSE) {
  es_types <- tolower(es_types)

  # TK: dataset has a lot of different ways to refer to the same effect size type
  # TK: should be cleaned up there eventually

  # Replace any round apostrophes (’) with straight apostrophes (')
  es_types <- gsub("\u2019", "'", es_types)

  # Define effect sizes that cannot be converted
  cannot_convert <- c("beta (std)", "partial etasq", "\u03C72", # χ2
                      "b (unstd)",
                      "b", "etasq (partial)", "cohen's f^2",
                      "cramer's v", "dz", "hazards ratio", "beta", "b",
                      "percentage", "squared seminpartial correlation (sr2)",
                      "regression coefficient", "unstandardized coefficient",
                      "cohen's h", "h")

  estype_map <- c(
    "or" = "or", "odds ratio" = "or", "odds ratio (study 3)" = "or",
    "d" = "d", "cohen's d" = "d", "hedges' g" = "d", "hedges'g" = "d",
    "hedge's g" = "d", "hedges g" = "d", "smd" = "d",
    "etasq" = "eta", "etaq" = "eta", "\u03B7\u00B2" = "eta", # η²
    "f" = "f", "cohen's f" = "f", # f
    "r" = "r", "phi" = "r", "\u03C6" = "r", # φ
    "r2" = "r2", "r\u00B2" = "r2", "r-square" = "r2", # r²
    "test statistic" = "test-stat", "test statistics" = "test-stat", "test" = "test-stat", # Test statistics
    "__NA__" = "__NA__" # to handle missing effect sizes in if clauses
  )


  es_values_r <- rep(NA, length(es_values))

  # Identify non-convertible effect sizes present in the data
  non_convertible_present <- unique(es_types[es_types %in% tolower(cannot_convert)])

  # Identify missing effect sizes
  missing_count <- sum(is.na(es_values) | is.na(es_types))
  es_types[is.na(es_types)] <- "__NA__"

  # Warn if there are unknown effect size types
  known_convertible <- names(estype_map)
  unknown_types <- setdiff(na.omit(unique(es_types)), c(cannot_convert, known_convertible, "__NA__"))

  # Identify which effect sizes are convertible
  convertible <- !(es_types %in% tolower(cannot_convert))

  estype_map <- na.omit(estype_map[unique(es_types)])


  if (any(!is.na(estype_map))) {

  for (original_type in names(estype_map)) {
    # Only process convertible effect sizes
    idx <- !is.na(es_types) & es_types == original_type & convertible
    estype <- estype_map[original_type]
    if (estype == "__NA__") {
      es_values_r[idx] <- NA
    } else if (estype == "r") {
      # Directly assign r values
      es_values_r[idx] <- as_numeric_verbose(es_values[idx], quiet = quiet)
    } else if (estype == "r2") {
      # Convert r² to r
      es_values_r[idx] <- sqrt(as_numeric_verbose(es_values[idx], quiet = quiet))
    } else if (estype == "d") {
      ds <- as_numeric_verbose(es_values[idx], quiet = quiet)
      es_values_r[idx] <- ds / sqrt(ds^2 + 4)
    } else if (estype == "or") {
      ors <- as_numeric_verbose(es_values[idx], quiet = quiet)
      # per Sánchez-Meca, J., Marín-Martínez, F., & Chacón-Moscoso, S. (2003). Effect-size indices for dichotomized outcomes in meta-analysis. Psychological Methods, 8(4), 448-467.
      ds <- log(ors)*sqrt(3)/pi
      es_values_r[idx] <- ds / sqrt(ds^2 + 4)
    } else if (estype == "eta") {
      etas <- as_numeric_verbose(es_values[idx], quiet = quiet)
      ds <- 2 * (sqrt(etas/(1 - etas))) # from esc package
      es_values_r[idx] <-ds / sqrt(ds^2 + 4)
    } else if (estype == "f") {
      fs <- as_numeric_verbose(es_values[idx], quiet = quiet)
      ds <- 2 * fs # from esc package
      es_values_r[idx] <- ds / sqrt(ds^2 + 4)
    } else if (estype == "test-stat") {
      # Convert test statistics formatted in APA style to r
      vals <- es_values[idx]
      converted <- vapply(vals, FUN.VALUE = 1.5, function(x) {
        x <- trimws(x)

        # Match APA formatted test statistics
        # t(df) = value
        t_match <- grepl("^t\\(\\d+\\)\\s*=\\s*-?\\d+\\.?\\d*$", x)
        # F(df1, df2) = value
        f_match <- grepl("^f\\(\\d+\\s*,\\s*\\d+\\)\\s*=\\s*\\d+\\.?\\d*$", x, ignore.case = TRUE)
        # z = value, N = value
        z_match <- grepl("^z\\s*=\\s*-?\\d+\\.?\\d*\\s*,\\s*n\\s*=\\s*\\d+$", x, ignore.case = TRUE)
        # x2/χ2(1, N = value) = value
        chi_candidate <- gsub("^[\u03c7\u03a7]", "x", x, perl = TRUE)
        chi_match <- grepl("^x2\\(\\s*1\\s*,\\s*n\\s*=\\s*\\d+\\s*\\)\\s*=\\s*\\d+\\.?\\d*$", chi_candidate, ignore.case = TRUE)

        if (t_match) {
          # Extract df and t-value
          df <- as_numeric_verbose(sub(".*t\\((\\d+)\\).*", "\\1", x), quiet = quiet)
          tval <- as_numeric_verbose(sub(".*=\\s*(-?\\d+\\.?\\d*).*", "\\1", x), quiet = quiet)
          return(tval / sqrt(tval^2 + df)) # Convert t to r
        } else if (f_match) {
          # Extract df1, df2, and F-value
          df1 <- as_numeric_verbose(sub(".*f\\((\\d+)\\s*,.*", "\\1", x, ignore.case = TRUE), quiet = quiet)
          df2 <- as_numeric_verbose(sub(".*f\\(\\d+\\s*,\\s*(\\d+)\\).*", "\\1", x, ignore.case = TRUE), quiet = quiet)
          fval <- as_numeric_verbose(sub(".*=\\s*(\\d+\\.?\\d*).*", "\\1", x, ignore.case = TRUE))

          if (df1 == 1) {
            # Convert F to t and then to r if df1 == 1
            tval <- sqrt(fval)
            return(tval / sqrt(tval^2 + df2))
          } else {
            return(NA) # Not convertible
          }
        } else if (z_match) {
          # Extract z-value and N
          zval <- as_numeric_verbose(sub(".*z\\s*=\\s*(-?\\d+\\.?\\d*).*", "\\1", x, ignore.case = TRUE), quiet = quiet)
          nval <- as_numeric_verbose(sub(".*n\\s*=\\s*(\\d+).*", "\\1", x, ignore.case = TRUE), quiet = quiet)
          return(zval / sqrt(zval^2 + nval)) # Convert z to r
        } else if (chi_match) {
          # Extract chi-square value and N
          nval <- as_numeric_verbose(sub(".*x2\\(\\s*1\\s*,\\s*n\\s*=\\s*(\\d+)\\s*\\).*", "\\1", chi_candidate, ignore.case = TRUE), quiet = quiet)
          chi_val <- as_numeric_verbose(sub(".*=\\s*(\\d+\\.?\\d*).*", "\\1", chi_candidate, ignore.case = TRUE), quiet = quiet)
          return(sqrt(chi_val / nval)) # Convert chi-square(1) to r
        } else {
          return(NA) # Not a valid test statistic format
        }
      })
      es_values_r[idx] <- converted
    } else {
      warning("Effect size type ", estype, " not recognized. Setting to missing.")
      es_values_r[idx] <- NA
    }
  }
  }

  # Identify effect sizes that could not be converted
  could_not_convert <- unique(es_types[!convertible & !is.na(es_types)])

  # Output messages in a clean format
  if (length(unknown_types) > 0) {
    warning("Unknown effect size types detected:\n  - ", paste(unknown_types, collapse = "\n  - "))
  }

  if (length(could_not_convert) > 0) {
    message("\n", length(could_not_convert), " effect sizes used cannot be converted into a common metric:\n  - ",
            paste(could_not_convert, collapse = "\n  - "))
  }

  if (missing_count > 0) {
    message("\n", missing_count, " rows had missing effect sizes or effect size types.\n")
  }

  es_values_r
}


signed_d_to_r <- function(d) {
  sign(d) * sqrt(d^2 / (d^2 + 4))
}

#' @title Convert to Numeric with Warnings
#' Converts input to numeric, reporting non-convertible values (for debugging) or transparency.
#' @param x Input vector.
#' @param quiet Logical; if FALSE, prints non-convertible values.
#' @return A numeric vector.
#' @keywords internal

as_numeric_verbose <- function(x, quiet = FALSE) {
  numeric_x <- suppressWarnings(as.numeric(x))
  failed_indices <- which(is.na(numeric_x) & !is.na(x))

  if (!quiet) {
  if (length(failed_indices) > 0) {
    failed_values <- unique(x[failed_indices])

    if (length(failed_values) < 10) {
      message("These values could not be converted to numeric: ", paste(failed_values, collapse = ", "))
    } else {
      message("These values could not be converted to numeric: ",
              paste(utils::head(failed_values, 5), collapse = ", "),
              " ... and ", length(failed_values) - 5, " more")
    }
  }
  }
  return(numeric_x)
}



#' Add common effect size columns to FReD dataset
#'
#' Converts original and replication effect sizes to common metric (r) and adds them to the dataset.
#'
#' @param fred_data FReD dataset
#' @param es_value_columns Character vector of column names for effect sizes
#' @param es_type_columns Character vector of column names for effect size types
#' @param es_common_names Names of columns where effect sizes should be saved. If coalesce_values is TRUE, existing values in these columns will be retained *if* no conversion is possible.
#' @param coalesce_values Logical. Should existing values in es_type_columns be retained?
#' @return FReD dataset with additional columns for common effect sizes

add_common_effect_sizes <- function(fred_data, es_value_columns = c("es_orig_value", "es_rep_value"),
                                    es_type_columns = c("es_orig_estype", "es_rep_estype"), es_common_names = c("es_original", "es_replication"),
                                    coalesce_values = TRUE) {
  if (!all.equal(length(es_value_columns), length(es_type_columns), length(es_common_names))) {
    stop("Length of es_value_columns, es_type_columns, and es_common_names must be equal")
  }
  for (i in seq_along(es_value_columns)) {
    if (!es_value_columns[i] %in% colnames(fred_data)) {
      stop("Column ", es_value_columns[i], " not found in FReD dataset")
    }
    if (!es_type_columns[i] %in% colnames(fred_data)) {
      stop("Column ", es_type_columns[i], " not found in FReD dataset")
    }

    if (coalesce_values && es_common_names[i] %in% names(fred_data)) {
      prev_es <- as.numeric(fred_data[, es_common_names[i]])

      if (any(!is.na(prev_es) & abs(prev_es) > 1)) {
        warning("Some existing effect sizes are outside the range of -1 to 1. Check input IDs ", paste(unique(fred_data$id[which(prev_es > 1)]), collapse = ", "),
                ". They will set to missing.")
        prev_es[abs(prev_es) > 1] <- NA
      }

      if (any(!is.na(prev_es) & abs(prev_es) == 1)) {
        message("Some existing effect sizes are entered as -1 or 1. Check input IDs ", paste(unique(fred_data$id[which(abs(prev_es) == 1)]), collapse = ", "),
                ". They will set to .9999 / -.9999 respectively, but should be double-checked.")
        prev_es[prev_es == 1] <- .9999
        prev_es[prev_es == -1] <- -.9999
      }

      fred_data[, es_common_names[i]] <- dplyr::coalesce(convert_effect_sizes(fred_data[, es_value_columns[i]], fred_data[, es_type_columns[i]]), prev_es)
    } else {
      fred_data[, es_common_names[i]] <- convert_effect_sizes(fred_data[, es_value_columns[i]], fred_data[, es_type_columns[i]])
    }

  }

  fred_data
}

#' Align effect direction
#'
#' Ensure that all original effects are coded as positive, and that replication effects are coded in the same direction (so that they *would* be positive if successful.
#'
#' @param fred_data FReD dataset
#' @param es_original Character. Name of original effect size column.
#' @param es_replication Character. Name of replication effect size column.
#' @return Augmented FReD dataset with aligned effect directions.

align_effect_direction <- function(fred_data, es_original = "es_original", es_replication = "es_replication") {
  orig_direction <- sign(fred_data[, es_original])
  fred_data[, es_original] <- abs(fred_data[, es_original])
  fred_data[, es_replication] <- fred_data[, es_replication] * orig_direction
  fred_data
}

#' Add Sampling Variances, Confidence Intervals, and P-values
#'
#' Adds sampling variances, confidence intervals (asymmetric, using z-transformation), and p-values for common-metric effect sizes (r) to the FReD dataset.
#'
#' @param fred_data FReD dataset
#' @param es_value_columns Character vector of column names with correlation values
#' @param N_columns Character vector of column names with sample sizes
#' @param vi_columns Character vector of target columns for sampling variances
#' @param ci_lower_columns Character vector of target columns for lower bounds of confidence intervals
#' @param ci_upper_columns Character vector of target columns for upper bounds of confidence intervals
#' @param p_values Character vector of target columns for p-values
#' @return FReD dataset with additional columns for standard errors, confidence intervals, and p-values
#'
#' @noRd
#' @examples
#' fred_data <- data.frame(es_original = c(0.3, 0.5), es_replication = c(0.4, 0.6),
#'                         n_original = c(30, 40), n_replication = c(50, 60))
#' add_uncertainty(fred_data)

add_uncertainty <- function(fred_data, es_value_columns = c("es_original", "es_replication"),
                            N_columns = c("n_original", "n_replication"),
                            vi_columns = c("vi_original", "vi_replication"),
                            ci_lower_columns = c("ci.lower_original", "ci.lower_replication"),
                            ci_upper_columns = c("ci.upper_original", "ci.upper_replication"),
                            p_values = c("p_value_original", "p_value_replication")) {
  if (!all.equal(length(es_value_columns), length(N_columns), length(vi_columns), length(ci_lower_columns), length(ci_upper_columns))) {
    stop("Length of all column character vectors must be equal")
  }
  for (i in seq_along(es_value_columns)) {
    if (!es_value_columns[i] %in% colnames(fred_data)) {
      stop("Column ", es_value_columns[i], " not found in FReD dataset")
    }
    if (!N_columns[i] %in% colnames(fred_data)) {
      stop("Column ", N_columns[i], " not found in FReD dataset")
    }

    fred_data[, vi_columns[i]] <- metafor::escalc(measure = "COR", ri = fred_data[, es_value_columns[i]], ni = fred_data[, N_columns[i]])$vi

    ci <- compute_ci_r(r =  fred_data[, es_value_columns[i]], n = fred_data[, N_columns[i]])

    fred_data[, ci_lower_columns[i]] <- ci[3]
    fred_data[, ci_upper_columns[i]] <- ci[4]

    fred_data[, p_values[i]] <- p_from_r(fred_data[, es_value_columns[i]], fred_data[, N_columns[i]])

    }

  fred_data
}

#' Code replication outcomes
#'
#' Different frameworks have been proposed to code replication outcomes. Here we code:
#' - signal vs no-signal (i.e., significant vs all others)
#' - consistent vs inconsistent (i.e., replication confidence interval overlaps original point estimate)
#' Based on https://etiennelebel.com/documents/lebeletal%282018,ampss%29a-unified-framework-to-quantify-the-credibility-of-scientific-findings.pdf
#' - and success vs failure (significant *in right direction* vs all others)
#'
#' @param fred_data FReD dataset
#' @param es_original Character. Name of original effect size column.
#' @param es_replication Character. Name of replication effect size column.
#' @param p_original Character. Significance of original effect size.
#' @param p_replication Character. Significance of replication effect size.
#' @param ci_lower_replication Character. Lower bound of replication confidence interval.
#' @param ci_upper_replication Character. Upper bound of replication confidence interval.
#' @return Augmented FReD dataset with replication outcome columns, including `signal`
#' @importFrom dplyr sym
#' @importFrom dplyr mutate case_when

code_replication_outcomes <- function(fred_data,
                           es_original = "es_original",
                           p_original = "p_value_original",
                           p_replication = "p_value_replication",
                           ci_lower_replication = "ci.lower_replication",
                           ci_upper_replication = "ci.upper_replication",
                           es_replication = "es_replication") {

  # Convert column names to symbols for dplyr evaluation
  es_original_sym <- dplyr::sym(es_original)
  p_original_sym <- dplyr::sym(p_original)
  p_replication_sym <- dplyr::sym(p_replication)
  ci_lower_replication_sym <- dplyr::sym(ci_lower_replication)
  ci_upper_replication_sym <- dplyr::sym(ci_upper_replication)
  es_replication_sym <- dplyr::sym(es_replication)

  fred_data <- fred_data %>%
    dplyr::mutate(
      signal = ifelse(!!p_replication_sym < 0.05, "signal", "no signal"),
      os_ns = ifelse(!!p_original_sym >= 0.05, "OS not significant", NA_character_),
      consistent = ifelse(!!ci_lower_replication_sym <= !!es_original_sym & !!ci_upper_replication_sym >= !!es_original_sym, "consistent", NA_character_),
      inconsistent = case_when(
        is.na(consistent) & signal == "signal" & !!ci_lower_replication_sym > !!es_original_sym ~ "inconsistent, larger",
        is.na(consistent) & signal == "signal" & !!ci_upper_replication_sym < !!es_original_sym ~ "inconsistent, smaller",
        is.na(consistent) & signal == "signal" & sign(!!es_replication_sym) != sign(!!es_original_sym) ~ "inconsistent, opposite",
        is.na(consistent) & signal != "signal" & !!ci_upper_replication_sym < !!es_original_sym ~ "inconsistent",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::mutate(
      consistency = dplyr::coalesce(.data$os_ns, .data$consistent, .data$inconsistent),
      result = dplyr::case_when(
        !!p_original_sym >= 0.05 ~ "OS not significant",
        !!p_replication_sym < 0.05 & sign(!!es_original_sym) == sign(!!es_replication_sym) ~ "successful replication",
        !!p_replication_sym >= 0.05 | sign(!!es_original_sym) != sign(!!es_replication_sym) ~ "failed replication",
        TRUE ~ NA_character_
      ),
      result2 = paste(.data$signal, .data$consistency, sep = " - ")
    ) %>%
    dplyr::select(-.data$consistent, -.data$inconsistent, -.data$os_ns)

  return(fred_data)
}

#' Add power
#'
#' Estimates the power of the replication study, given the original effect size, the sample size of the replication study, and the usual focus on a two-tailed test.
#'
#' @param fred_data FReD dataset
#' @param es_original Character. Name of original effect size column.
#' @param N_replication Character. Name of replication sample size column.
#' @param power_column Character. Name of target column for power.
#' @return Augmented FReD dataset with power column.

add_replication_power <- function(fred_data, es_original = "es_original", N_replication = "n_replication", power_column = "power_r") {
  # NA where N_replication is missing
  fred_data[, power_column] <- NA
  # Return 0 where sample_replication < 4, as pwr.r.test does not work for n < 4
  # This will underestimate power for tiny samples, but they should be visible as low power rather than missing
  fred_data[fred_data[, N_replication] %>% {!is.na(.) & . < 4}, power_column] <- 0
  # Return power where sample_replication >= 4
  fred_data[fred_data[, N_replication] %>% {!is.na(.) & . >= 4}, power_column] <-
    compute_power_r(r = fred_data[fred_data[, N_replication] %>% {!is.na(.) & . >= 4}, es_original], n = fred_data[fred_data[, N_replication] %>% {!is.na(.) & . >= 4}, N_replication])
  fred_data
}

#' Calculate p-values from correlation coefficients and sample size
#'
#' This function calculates the p-value associated with a given correlation
#' coefficient and sample size, assuming the null hypothesis that the true
#' population correlation is zero.
#'
#' @param r Correlation coefficient.
#' @param N Sample size.
#'
#' @return Numeric p-value for the two-tailed test of the correlation.
#' @noRd
#' @examples
#' p_from_r(r = c(0.5, 0.3), N = c(30, 25))

p_from_r <- function(r, N) {

    if (!is.numeric(r) || !is.numeric(N)) {
    stop("Both 'r' and 'N' must be numeric")
  }
  if (length(r) != length(N)) {
    stop("Length of 'r' and 'N' must be the same")
  }

  p_value <- rep(NA, length(r))

  # Handle valid cases (non-NA, N > 2)
  valid_indices <- !(is.na(r) | is.na(N) | N <= 2)

  if (any(valid_indices)) {
    t_value <- r[valid_indices] * sqrt((N[valid_indices] - 2) / (1 - r[valid_indices]^2))
    p_value[valid_indices] <- 2 * pt(-abs(t_value), df = N[valid_indices] - 2)
  }

  return(p_value)
}

#' Add confidence intervals
#'
#' Adds sampling variances for common-metric effect sizes (r) to the FReD dataset, using metafor::escalc
#'
#' @param fred_data FReD dataset
#' @param es_value_columns Character vector of column names with correlation values
#' @param N_columns Character vector of column names with sample sizes
#' @param vi_columns Character vector of target columns for sampling variances
#' @return FReD dataset with additional columns for sampling variances (metafor's `vi`)

add_sampling_variances <- function(fred_data, es_value_columns = c("es_original", "es_replication"),
                                   N_columns = c("n_original", "n_replication"), vi_columns = c("vi_original", "vi_replication")) {
  if (!all.equal(length(es_value_columns), length(N_columns))) {
    stop("Length of es_value_columns, N_columns and vi_columns must be equal")
  }
  for (i in seq_along(es_value_columns)) {
    if (!es_value_columns[i] %in% colnames(fred_data)) {
      stop("Column ", es_value_columns[i], " not found in FReD dataset")
    }
    if (!N_columns[i] %in% colnames(fred_data)) {
      stop("Column ", N_columns[i], " not found in FReD dataset")
    }
    fred_data[, vi_columns[i]] <- metafor::escalc(measure = "COR", ri = fred_data[, es_value_columns[i]], ni = fred_data[, N_columns[i]], data = fred_data)$vi
  }
  fred_data
}

#' Augment Data for Z-Curve Analysis
#'
#' This function calculates and appends the standard error and z-score
#' for z-curve analysis based on the original effect size and sample size.
#'
#' @param fred_data A dataframe containing `es_original`
#'   for the effect size of the original study, and `n_original` for the
#'   sample size of the original study.
#' @return A dataframe with the original `fred_data` and two additional columns:
#'   `se` for the standard error, and `z` for the z-score.
#' @noRd

augment_for_zcurve <- function(fred_data) {

  # Ensure fred_data has required columns
  if (!all(c("es_original", "n_original") %in% names(fred_data))) {
    stop("fred_data must contain es_original and n_original columns")
  }

  # Initialize se and z as NA
  fred_data$se <- fred_data$z <- NA

  valid_indices <- !(is.na(fred_data$es_original) | is.na(fred_data$n_original) | fred_data$n_original <= 3)

  if (any(valid_indices)) {
    # Fisher's z transformation
    z <- 0.5 * (log(1 + fred_data$es_original[valid_indices]) - log(1 - fred_data$es_original[valid_indices]))
    fred_data$se[valid_indices] <- 1 / sqrt(fred_data$n_original[valid_indices] - 3)
    fred_data$z[valid_indices] <- z / fred_data$se[valid_indices]
  }

  return(fred_data)
}
