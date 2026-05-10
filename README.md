Fully vectorized script for running and visualizing the supervaluationist rational speech act framework of Cremers, Wilcox & Spector (2023) for the pragmatics of bare plurals[^1]

# How to use
- `synthesis.R`: the library of relevant functions and parameters, including listener and speaker probabilities and utilities, setup of model parameters, and visualization pipelines
- `synthesis_maximin.R`: replace the speaker utility function with the maximin version (as opposed to the expected value version in `synthesis.R`)
- `run_model_expected.qmd`: Quarto notebook that runs the expected utility model and visualizes the results for cases with different discourse parameters
- `run_model_maximin.qmd`: Quarto notebook that runs the maximin utility model and visualizes the results for cases with different discourse parameters


[^1]: Cremers, A., Wilcox, E. G., & Spector, B. (2023). Exhaustivity and anti‐exhaustivity in the RSA framework: Testing the effect of prior beliefs. Cognitive Science, 47(5), 1–44. <https://doi.org/10.1111/cogs.13286>
