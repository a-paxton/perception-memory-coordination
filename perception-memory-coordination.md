<!-- *** -->

<!-- ```{r} -->

<!-- more_similar_changes_st = lmer(normalized_error ~ trial_number + training_improvement + -->
<!--                                     (1 | participant)  + -->
<!--                                     (1 | dyad), -->
<!--                                   data = winnowed_info_df) -->
<!-- pander_lme(more_similar_changes_st, stats.caption = TRUE) -->

<!-- more_similar_changes_raw = lmer(normalized_error ~ trial_number*training_improvement + -->
<!--                                     (1 | participant)  + -->
<!--                                     (1 | dyad), -->
<!--                                   data = final_guess_plot) -->
<!-- pander_lme(more_similar_changes_raw, stats.caption = TRUE) -->

<!-- # does ignoring directionality matter? -->
<!-- more_similar_changes_raw_poisson = glmer(round(abs(normalized_error)*100,0) ~ trial_number + training_improvement + -->
<!--                                     (1 | participant)  + -->
<!--                                     (1 | dyad), -->
<!--                                   data = final_guess_plot, -->
<!--                                   family = poisson) -->
<!-- summary(more_similar_changes_raw_poisson) -->


<!-- ``` -->


