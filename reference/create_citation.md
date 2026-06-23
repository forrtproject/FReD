# Get FReD dataset citation

Retrieves the current citation for the FReD dataset from GitHub.

## Usage

``` r
create_citation(
  citation_url =
    "https://raw.githubusercontent.com/forrtproject/FReD-data/main/output/citation.txt",
  cache = TRUE
)
```

## Arguments

- citation_url:

  URL to the citation file on GitHub

- cache:

  Should the citation be returned from cache, if already requested
  during this session? Defaults to TRUE.

## Value

A markdown-formatted citation for the FReD dataset.
