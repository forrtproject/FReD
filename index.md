# FReD

The goal of the FReD package is to provide various interfaces to the
FORRT Replication Database. The package includes two shiny apps - the
FReD Explorer and the FReD Annotator - as well as functions to directly
access and analyse the dataset.

## FReD Explorer

The FReD Explorer allows users to explore the dataset, and to assess
replicability and effect sizes of the included studies based on various
filters. It primarily serves as a gateway to meta-science. You can
access the FReD Explorer
[online](https://forrt-replications.shinyapps.io/fred_explorer/) or by
calling
[`run_explorer()`](http://forrt.org/FReD/reference/run_explorer.md) in
your R console after you have loaded the package.

## FReD Annotator

The FReD Annotator allows users to annotate lists of articles
(e.g. reading lists for classes, or reference lists for draft articles)
with any replication findings. This is intended to make it easier to use
replications in teaching and research, and to thus ensure that claims
are based on the best available evidence. You can access the FReD
Annotator
[online](https://forrt-replications.shinyapps.io/fred_annotator/) or by
calling
[`run_annotator()`](http://forrt.org/FReD/reference/run_annotator.md) in
your R console after you have loaded the package.

## Installation

You can install the development version of FReD like so:

``` r
devtools::install_github("forrtproject/FReD")
```

## Launching the apps

``` r
library(FReD)
run_explorer()
run_annotator()
```

## Accessing the data

To load the dataset and prepare it for analyses, you can use the
following code:

``` r
library(FReD)
load_fred_data()
```
