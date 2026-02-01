This version history contains noteworthy changes. For a full history of changes, see the [commit history](https://github.com/forrtproject/FReD/commits/main/)

# FReD 0.2.0

## Breaking Changes
- **New data source URL**: The package now uses a new data source with updated variable naming conventions
- **Variable naming convention**: All variable names now use `_o` suffix for original study variables and `_r` suffix for replication study variables (e.g., `es_original` → `es_o`, `n_replication` → `n_r`, `ref_original` → `ref_o`, `doi_replication` → `doi_r`)
- **`run_annotator()` deprecated**: The local annotator app has been removed. `run_annotator()` now opens the web version at forrt.org instead

## Notes
- If you have code that references old variable names, you will need to update it to use the new `_o`/`_r` suffixes

# FReD 0.1.0

## New features
- The Replication Database has merged with the FORRT Reversals & Replications project to become FReD
- The FReD R package now wraps the shiny apps, to facilitate installation and maintenance
- The Shiny apps now offer the choice between many methods to assess replication success, detailled in a vignette

## Enhancements
- Further expansion of the dataset
- The package now defaults to using offline data to speed up the apps, but prompts users to update data on package load after 30 days if a new release is available. To always use the latest data, run `use_FReD_offline(FALSE)`.

## Bug Fixes
- Change user defaults from options to environment variables, so that they are respected in background apps.
- Effect size conversions now retain the sign of the original effect size (and introduce it for odds ratios)

