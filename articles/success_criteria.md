# Assessing replication success

``` r
library(FReD)
library(dplyr)
library(ggplot2)
```

## Different criteria for replication success

Whether a replication is successful can be assessed in many different
ways that answer different questions. Many criteria have been proposed,
and some of them have been implemented in
[`assess_replication_outcome()`](http://forrt.org/FReD/reference/assess_replication_outcome.md) -
we will briefly introduce them here, and then compare their results.

### Repeated significance

Potentially the most basic question is whether a replication again
yields a statistically significant result in the same direction as the
original finding. This has been used to generate many of the previous
headline findings about replicability, and is easily interpretable.
However, there are some limitations:

- It ignores the evidence provided by the original study entirely, and
  might thus assume that a difference in significance is statistically
  significant.
- It does not consider the size of the effect, so that large-scale
  replications that find tiny effects may still be considered
  successful.
- It does not consider the quality of the replication, so that a
  replication with a tiny sample size may be reported as a failed
  replication, while it does not tell us anything about the original
  effect.

To use this criterion, set `criterion = "significance_r"` in
[`assess_replication_outcome()`](http://forrt.org/FReD/reference/assess_replication_outcome.md).

``` r
assess_replication_outcome(es_o = .5, n_o = 50, es_r = .1, n_r = 1000,
                           criterion = "significance_r")
#>   outcome                  outcome_detailed outcome_report
#> 1 success replication effect is significant        success
```

### Aggregated Significance

Another approach is to aggregate the effect sizes from the original and
replication studies using a meta-analytic approach and assess whether
their combined effect is significantly different from zero. This method
considers the evidence from both studies together, offering a more
integrated perspective.

However, it assumes that the quality of evidence from both studies is
comparable - which may not be the case if the original study is likely
to have been subject to selective reporting (i.e., publication bias) or
other questionable research practices. In that case, the reported effect
sizes are likely inflated, so that more trust should be placed in the
replication study.

To use this criterion, set `criterion = "significance_agg"` in
[`assess_replication_outcome()`](http://forrt.org/FReD/reference/assess_replication_outcome.md):

``` r
assess_replication_outcome(es_o = .5, n_o = 50, es_r = .1, n_r = 50,
                           criterion = "significance_agg")
#>   outcome                                             outcome_detailed
#> 1 success aggregated effect is larger than 0 and in the same direction
#>   outcome_report
#> 1        success
```

### Consistency with Replication Confidence Interval

This criterion checks whether the original effect size falls within the
confidence interval of the replication effect size. If it does, it
suggests that the replication is consistent with the original finding,
taking into account the uncertainty of the replication study.

This method is beneficial because it directly compares the two studies,
emphasizing whether the original result is plausible given the
replication data. However, it makes lower-powered replications more
likely to be considered “successful”, even if their point estimate is
far from the original effect size. Conversely, it makes large (and thus
more precise) replications more likely to be considered “failed”, even
if their point estimate is close to the original effect size.

To use this criterion, set `criterion = "consistency_ci"`.

``` r
assess_replication_outcome(es_o = .5, n_o = 50, es_r = .3, n_r = 50,
                           criterion = "consistency_ci")
#>   outcome                                outcome_detailed outcome_report
#> 1 success Original effect size is within replication's CI        success
assess_replication_outcome(es_o = .5, n_o = 50, es_r = .4, n_r = 500,
                           criterion = "consistency_ci")
#>   outcome                                    outcome_detailed outcome_report
#> 1 failure Original effect size is not within replication's CI        failure
```

### Prediction Intervals

Prediction intervals offer another way to assess consistency by checking
whether the replication effect size falls within the prediction interval
derived from the original study. This interval accounts for the expected
variability in replication results and provides a broader test than
confidence intervals.

This method is beneficial because it considers the uncertainty in both
the original and replication studies. However, particularly for small
replication studies, the prediction interval may be very wide, making it
very difficult to reject them through replications.

To use this criterion, set `criterion = "consistency_pi"`

``` r
assess_replication_outcome(es_o = .5, n_o = 50, es_r = .3, n_r = 500,
                           criterion = "consistency_pi")
#>   outcome                                          outcome_detailed
#> 1 success Replication effect size is within the prediction interval
#>   outcome_report
#> 1        success
```

### Homogeneity of Effects

This criterion tests whether the effects from the original and
replication studies are statistically different from each other, using a
heterogeneity test (i.e., a Q-test). This is the most rigorous test for
the question whether a replication contradicts an original study (if the
original study’s claim is taken to encompass its full statistical
uncertainty).

However, this test generally has low power, making replication failures
for *small* original studies rather unlikely. Also, note that effect
sizes can be homogeneous even if the joint effect is not significant, so
that a replication might be considered successful even if the aggregate
evidence does not support the existence of an effect.

To use this criterion, set `criterion = "homogeneity"`:

``` r
assess_replication_outcome(es_o = .2, n_o = 100, es_r = 0, n_r = 500,
                           criterion = "homogeneity")
#>   outcome              outcome_detailed outcome_report
#> 1 success effects are not heterogeneous        success
```

### Homogeneity and Significance

This criterion combines the assessment of homogeneity with the
significance of the effect sizes. It considers a replication as
successful if the effect is not statistically different from the
original study and the combined effect is significantly different from
zero. This method provides a balanced assessment that considers both
consistency and evidence strength - yet still has low power to identify
replication failures driven by an exaggeration of the reported effect
size in small studies.

To use this criterion, set `criterion = "homogeneity_significance"`:

``` r
assess_replication_outcome(es_o = .2, n_o = 100, es_r = 0, n_r = 500,
                           criterion = "homogeneity_significance")
#>   outcome                                 outcome_detailed
#> 1 failure effect sizes are homogeneous but not significant
#>                              outcome_report
#> 1 failure (homogeneous but not significant)
```

### Small Telescopes

Derived from Simonsohn (2015), the small telescopes approach tests
whether the replication effect size is larger than the effect size that
would have given the original study a power of 33%. This criterion is
based on the idea that a successful replication should indicate that the
original study provided some evidence (rather than being a lucky guess).
However, this can label replications with substantial effect sizes as
failures if the original study was very small, so it is not an
appropriate criterion if the focus is on the truth of claims, rather
than the quality of evidence.

To use this criterion, set `criterion = "small_telescopes"`:

``` r
assess_replication_outcome(es_o = .5, n_o = 50, es_r = .2, n_r = 100,
                           criterion = "small_telescopes")
#>   outcome                                             outcome_detailed
#> 1 success original study had >= 33% power to detect replication effect
#>   outcome_report
#> 1        success

assess_replication_outcome(es_o = .5, n_o = 30, es_r = .2, n_r = 100,
                           criterion = "small_telescopes")
#>   outcome                                            outcome_detailed
#> 1 failure original study had < 33% power to detect replication effect
#>   outcome_report
#> 1        failure
```

## Comparing different criteria for replication success

To compare the different criteria, we plot the aggregate outcomes based
on the FReD dataset. For this, we ignore studies that have not been
coded.

``` r
df <- load_fred_data() %>% 
  filter(!is.na(es_replication) & !is.na(es_original)) 

criteria <- tribble(
  ~criterion,                  ~name,                             
   "significance_r",            "Significance\n(replication)",               
   "significance_agg",          "Significance\n(aggregated)",             
   "consistency_ci",            "Consistency\n(confidence interval)", 
   "consistency_pi",            "Consistency\n(prediction interval)",   
   "homogeneity",               "Homogeneity",                    
   "homogeneity_significance",  "Homogeneity +\nSignificance",
   "small_telescopes",          "Small Telescopes"
)

results <- lapply(criteria$criterion, function(criterion) {
  df %>% 
    mutate(assess_replication_outcome(es_original, n_original, es_replication, n_replication, criterion = criterion)) %>% 
    count(outcome, outcome_detailed) %>% 
    mutate(criterion = criterion)
})

results <- results %>%
  bind_rows() %>%
  left_join(criteria, by = "criterion") %>%
  arrange(outcome != "success", -n) %>%
  mutate(criterion = factor(name, levels = unique(name)))

success_shares <- results %>% 
  bind_rows() %>%
  group_by(criterion) %>% 
  summarise(success_share = sum(n[!is.na(outcome) & outcome == "success"]) / sum(n),
            success_count = sum(n[!is.na(outcome) & outcome == "success"])) %>% 
  mutate(outcome = "success")

p <- results %>%
  ggplot(aes(criterion, n, fill = outcome, text = outcome_detailed)) +
  geom_col(position = "stack", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = paste(round(success_share*100, 1), "%"), text = NULL, y = success_count/2), data = success_shares) +
  labs(y = "k", x = "Success criterion") +
    scale_fill_manual(values = c(
    "success" = "#66C2A5",        
    "failure" = "#FC8D62",        
    "OS not significant" = "#BFBFBF", 
    "NA" = "#808080"              
  ))


p %>% 
  plotly::ggplotly(tooltip = c("text", "n"))
```
