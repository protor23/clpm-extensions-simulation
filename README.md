# clpm-extensions-simulation

This is the repository for my master's dissertation investigating the dangers of between-person variability when studying causal relationships in panel designs.

The code above simulates data from three models (CLPM, RI-CLPM, LGCM-SR) and fits the same three models to all datasets automatically, recording effect estimates and some fit indices. 

Scripts should be run in the order in which they are presented, as also indicated by the indexing number at the beginning of script names. 

0_functions_extract-info_fit-models.R => loads functions that automatically fit models and retrieve fitting info (e.g., estimates and fit indices)

0_models_syntax.R => loads the models with lavaan synax

1_data-generation-model-fitting_clpm.R => generates data from the CLPM and fits all models to it => saves model fitting results (1_log_clpm.csv) and conditions (1_conditions_clpm.csv) in the simualtions file

2_data-generation-model-fitting_riclpm.R => generates data from the RI-CLPM and fits all models to it => saves model fitting results (2_log_riclpm.csv) and conditions (2_conditions_riclpm.csv) in the simualtions file

3_data-generation-model-fitting_riclpm.R => generates data from the LGCM-SR and fits all models to it => saves model fitting results (3_log_lgcmsr.csv) and conditions (3_conditions_lgcmsr.csv) in the simualtions file

In all cases, generated data is a dataframe with variable names x1, x2, x3, x4, y1, y2, y3, y4 representing each measurement occasion for each of the two variables.

4_results_convergence-fit-bias-ci => retrieves information about fit indices, effect sizes, and confidence intervals accuracy 

5_graphs_bias-conditions => produces ggplot2 graphs outlining the mean relative bias of parameters across simulation conditions => saves them in the graphs folder (e.g., 1_bias_by-dominance_clpm.pdf)
