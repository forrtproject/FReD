# Create FReD dataset citation

Pulls current contributor list and dynamicalky creates a
*markdown-formatted* citation for the FReD dataset.

## Usage

``` r
create_citation(data_file = get_param("FRED_DATA_FILE"), cache = TRUE)
```

## Arguments

- data_file:

  Path to the FReD dataset, defaults to the current FReD dataset on OSF

- cache:

  Should the citation be returned from cache, if already requested
  during this session? Defaults to TRUE.

## Value

A markdown-formatted citation for the FReD dataset, including the
current dataset version.
