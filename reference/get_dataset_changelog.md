# Get the dataset changelog from OSF

Downloads and reads the changelog file from OSF, and returns it as a
character string.

## Usage

``` r
get_dataset_changelog(
  changelog_file = "https://osf.io/fj3xc/download",
  cache = TRUE
)
```

## Arguments

- changelog_file:

  The URL of the changelog file on OSF

- cache:

  Should the changelog be returned from cache, if already requested
  during this session? Defaults to TRUE.
