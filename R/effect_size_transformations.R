#' Convert effect sizes to a common metric (Pearson's r)
#'
#' @description
#' Converts a variety of effect sizes and test statistics to Pearson's r for
#' comparability. Effect sizes that cannot be meaningfully converted are
#' returned as \code{NA} with appropriate warnings.
#'
#' @details
#' \subsection{Supported Effect Sizes}{
#'
#' The following effect sizes can be converted to r. Accepted \code{es_types}
#' values are case-insensitive.
#'
#' \describe{
#'   \item{Pearson's r / phi}{
#'     \code{"r"}, \code{"phi"}, \code{"φ"} \cr
#'     \strong{Conversion:} Returned as-is. \cr
#'     \strong{Sign:} Preserved.
#'   }
#'   \item{R-squared}{
#'     \code{"r2"}, \code{"r^2"}, \code{"r²"}, \code{"r-square"} \cr
#'     \strong{Conversion:} \eqn{r = \sqrt{R^2}}{r = sqrt(R²)} \cr
#'     \strong{Sign:} Always positive (R² is non-directional).
#'   }
#'   \item{Cohen's d / Hedges' g}{
#'     \code{"d"}, \code{"cohen's d"}, \code{"hedges' g"}, \code{"smd"} \cr
#'     \strong{Conversion:} \eqn{r = \frac{d}{\sqrt{d^2 + 4}}}{r = d / sqrt(d² + 4)} \cr
#'     \strong{Sign:} Preserved (if input d is negative, r is negative).
#'   }
#'   \item{Odds ratio}{
#'     \code{"or"}, \code{"odds ratio"} \cr
#'     \strong{Conversion:} \eqn{d = \ln(OR) \cdot \sqrt{3} / \pi}{d = ln(OR) * sqrt(3) / π},
#'     then d to r. \cr
#'     \strong{Sign:} Preserved (OR < 1 implies negative r).
#'   }
#'   \item{Eta-squared}{
#'     \code{"etasq"}, \code{"eta^2"}, \code{"η²"} \cr
#'     \strong{Conversion:} \eqn{d = 2\sqrt{\frac{\eta^2}{1 - \eta^2}}}{d = 2 * sqrt(η² / (1 - η²))},
#'     then d to r. \cr
#'     \strong{Sign:} Always positive.
#'   }
#'   \item{Cohen's f / f²}{
#'     \code{"f"}, \code{"cohen's f"}, \code{"f2"}, \code{"f^2"}, \code{"f²"} \cr
#'     \strong{Conversion (f):} \eqn{d = 2f}{d = 2f}, then d to r. \cr
#'     \strong{Conversion (f²):} \eqn{R^2 = \frac{f^2}{1 + f^2}}{R² = f² / (1 + f²)},
#'     then \eqn{r = \sqrt{R^2}}{r = sqrt(R²)}. \cr
#'     \strong{Sign:} Always positive.
#'   }
#' }
#' }
#'
#' \subsection{Test Statistics}{
#'
#' When \code{es_types} is \code{"test statistic"}, \code{"test statistics"},
#' or \code{"test"}, the function parses APA-formatted strings in \code{es_values}.
#'
#' \describe{
#'   \item{t-test}{
#'     \strong{Format:} \code{"t(df) = value"} \cr
#'     \strong{Conversion:} \eqn{r = \frac{t}{\sqrt{t^2 + df}}}{r = t / sqrt(t² + df)} \cr
#'     \strong{Sign:} Preserved.
#'   }
#'   \item{F-test}{
#'     \strong{Format:} \code{"F(df1, df2) = value"} \cr
#'     \strong{Constraint:} df1 must equal 1. \cr
#'     \strong{Conversion:} \eqn{t = \sqrt{F}}{t = sqrt(F)}, then t to r. \cr
#'     \strong{Sign:} Always positive.
#'   }
#'   \item{z-test}{
#'     \strong{Format:} \code{"z = value, N = value"} \cr
#'     \strong{Conversion:} \eqn{r = \frac{z}{\sqrt{z^2 + N}}}{r = z / sqrt(z² + N)} \cr
#'     \strong{Sign:} Preserved.
#'   }
#'   \item{Chi-squared}{
#'     \strong{Format:} \code{"x2(1, N = value) = value"} \cr
#'     \strong{Constraint:} df must equal 1. \cr
#'     \strong{Conversion:} \eqn{r = \sqrt{\frac{\chi^2}{N}}}{r = sqrt(χ² / N)} \cr
#'     \strong{Sign:} Always positive.
#'   }
#' }
#' }
#'
#' \subsection{Non-Convertible Effect Sizes}{
#' The following effect sizes cannot be reliably converted to r and return
#' \code{NA}: partial eta-squared, Cramer's V, Cohen's h, Cohen's \eqn{d_z}{dz},
#' regression coefficients (\code{"b"}, \code{"beta"}), and semi-partial correlations.
#' }
#'
#' @param es_values Numeric vector of effect sizes, or character vector for
#'   test statistics formatted in APA style (e.g., \code{"t(10) = 2.5"}).
#' @param es_types Character vector of effect size types (case-insensitive).
#'   See Details for accepted values. Unrecognized types trigger a warning.
#' @param quiet Logical. If \code{TRUE}, suppresses warnings about unknown
#'   effect size types and messages about non-convertible or missing values.
#'
#' @return Numeric vector of effect sizes converted to Pearson's r. Returns
#'   \code{NA} for values that cannot be converted or are missing.
#'
#' @references
#' Sánchez-Meca, J., Marín-Martínez, F., & Chacón-Moscoso, S. (2003).
#' Effect-size indices for dichotomized outcomes in meta-analysis.
#' \emph{Psychological Methods}, \emph{8}(4), 448--467.
#' \doi{10.1037/1082-989X.8.4.448}
#'
#' @export

convert_effect_sizes <- function(es_values, es_types, quiet = FALSE) {
  es_types <- tolower(es_types)

  # TK: dataset has a lot of different ways to refer to the same effect size type
  # TK: should be cleaned up there eventually

  # Replace any round apostrophes (’) with straight apostrophes (')
  es_types <- gsub("\u2019", "'", es_types)

  # Define effect sizes that cannot be converted
  cannot_convert <- c("beta (std)", "partial etasq", "\u03C72", # χ2
                      "b (unstd)",
                      "b", "etasq (partial)",
                      "cramer's v", "dz", "hazards ratio", "beta", "b",
                      "percentage", "squared seminpartial correlation (sr2)",
                      "regression coefficient", "unstandardized coefficient",
                      "cohen's h", "h")

estype_map <- c(
  # Odds ratios
  "or"              = "or",
  "odds ratio"      = "or",

  # Standardised mean differences (Cohen's d, Hedges' g)
  "d"               = "d",
  "cohen's d"       = "d",
  "hedges' g"       = "d",
  "hedges'g"        = "d",
  "hedge's g"       = "d",
  "hedges g"        = "d",
  "hedgesg"         = "d",
  "smd"             = "d",

  # Eta-squared (η²)
  "etasq"           = "eta2",
  "etaq"            = "eta2",
  "eta^2"           = "eta2",      
  "\u03b7\u00b2"    = "eta2",       # η² (Unicode)

  # Cohen's f
  "f"               = "f",
  "cohen's f"       = "f",

  # Cohen's f²
  "f2"              = "f2",
  "f^2"             = "f2",
  "f\u00b2"         = "f2",        # f² (Unicode)
  "cohen's f^2"     = "f2",

  # Correlations (r / phi)
  "r"               = "r",
  "phi"             = "r",
  "\u03c6"          = "r",         # φ (Unicode)

  # R-squared (R²)
  "r2"              = "r2",
  "r^2"             = "r2",
  "r\u00b2"         = "r2",        # r² (Unicode)
  "r-square"        = "r2",

  # Test statistics bucket
  "test statistic"  = "test-stat",
  "test statistics" = "test-stat",
  "test"            = "test-stat",

  # Explicit NA placeholder
  "__NA__"          = "__NA__"
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
    } else if (estype == "eta2") {
      eta2s <- as_numeric_verbose(es_values[idx], quiet = quiet)
      ds <- 2 * (sqrt(eta2s/(1 - eta2s))) # from esc package
      es_values_r[idx] <-ds / sqrt(ds^2 + 4)
    } else if (estype == "f") {
      fs <- as_numeric_verbose(es_values[idx], quiet = quiet)
      ds <- 2 * fs # from esc package
      es_values_r[idx] <- ds / sqrt(ds^2 + 4)
    } else if (estype == "f2") {
      f2s <- as_numeric_verbose(es_values[idx], quiet = quiet)
      # Convert f^2 to r^2, then to r
      r2s <- f2s / (1 + f2s)
      es_values_r[idx] <- sqrt(r2s)
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
