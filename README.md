# Statistical Analysis and Predictive Modeling using R

**Week 3 Task | Data Analysis Internship, Yuva Intern**
**Author:** Pulivarthi Sai Teja

## Overview
This project predicts car fuel economy (mpg) from design features using the built-in `mtcars` dataset (32 cars). It covers the full workflow: assumption checks, hypothesis tests, regression modelling, cross-validation, diagnostics and optimisation.

## What is in the analysis
| Stage | Methods |
|-------|---------|
| Distribution and normality | Shapiro-Wilk test, histogram, Q-Q plot |
| Correlation | Pearson and Spearman tests |
| Hypothesis tests | Correlation test (weight vs mpg), Welch t-test and Wilcoxon (transmission), ANOVA, Tukey, Welch ANOVA and Kruskal-Wallis (cylinders) |
| Modelling | Seven linear regression candidates compared with AIC, 5-fold CV and leave-one-out CV |
| Diagnostics | Residual plots, Shapiro-Wilk on residuals, Breusch-Pagan, VIF, Cook's distance |
| Optimisation | Log transform and extra predictors tested with cross-validation |

## Key results
- Final model: `mpg ~ wt * hp` (adjusted R-squared 0.872, lowest AIC of seven candidates).
- Cross-validated error: RMSE 2.43 mpg (5-fold) and 2.34 mpg (leave-one-out), versus 3.17 mpg for the weight-only baseline.
- Diagnostics: linearity and normality hold, mild unequal variance, no highly influential cars.

## Repository structure
```
analysis.R      # full R script (tests, models, cross-validation, diagnostics)
images/         # exported charts
report/         # final Word report
```

## How to run
1. Install R and RStudio.
2. Install packages: `install.packages(c("ggplot2", "dplyr", "car", "lmtest"))`
3. Open `analysis.R` and run it from top to bottom. Cross-validation folds are fixed (not random), so results are reproducible.

## Tools
R, ggplot2, dplyr, car, lmtest
