# Set FReD to work offline (or back to online)

By default, FReD loads the latest data and meta-data every time it is
loaded. If you work offline, or don't need the latest data, you may
prefer to work from cached data. For that, you can call
'use_FReD_offline()'. If you want this to persist even when FReD is
reloaded, you can use `Sys.setenv(FRED_OFFLINE = TRUE)` (or FALSE) as
needed, and include it in your `.Renviron` file to persist across
sessions.

## Usage

``` r
use_FReD_offline(state = TRUE)
```

## Arguments

- state:

  Should FReD work offline (TRUE) or online (FALSE)
