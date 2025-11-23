# Get the date of last modification

This function returns the date of last modification to be displayed in
an app. It represents either the date when a specific element of the app
was last modified, or the date of the most recent modification to the
package DESCRIPTION file (which represents broader updates).

## Usage

``` r
get_last_modified(app_folder = "fred_explorer")
```

## Arguments

- app_folder:

  The name of the app folder within the package (i.e. within the inst
  folder)
