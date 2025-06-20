This version history contains noteworthy changes. For a full history of changes, see the [commit history](https://github.com/forrtproject/FReD/commits/main/)

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

