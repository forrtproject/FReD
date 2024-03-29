% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_apps.R
\name{run_annotator}
\alias{run_annotator}
\title{Run the Replication Annotator}
\usage{
run_annotator(offer_install = interactive())
}
\arguments{
\item{offer_install}{Should user be prompted to install required packages if they are missing?}
}
\value{
Replication Annotator shiny app
}
\description{
Running this function will launch the FReD Replication Annotator shiny app
}
\examples{
if (interactive()) {
  # To run the CReplication Annotator app:
  run_annotator()
}
}
