% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/effect_size_transformations.R
\name{add_sampling_variances}
\alias{add_sampling_variances}
\title{Add confidence intervals}
\usage{
add_sampling_variances(
  fred_data,
  es_value_columns = c("es_original", "es_replication"),
  N_columns = c("n_original", "n_replication"),
  vi_columns = c("vi_original", "vi_replication")
)
}
\arguments{
\item{fred_data}{FReD dataset}

\item{es_value_columns}{Character vector of column names with correlation values}

\item{N_columns}{Character vector of column names with sample sizes}

\item{se_columns}{Character vector of target columns for standard errors}
}
\value{
FReD dataset with additional columns for standard errors
}
\description{
Adds standard errors for common-metric effect sizes (r) to the FReD dataset, using metafor::escalc
}
