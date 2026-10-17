# ---- setup
# Install once if needed:
# install.packages(c("ggplot2", "dplyr", "car", "lmtest"))
library(ggplot2)
library(dplyr)
library(car)      # vif()
library(lmtest)   # bptest()

data(mtcars)      # 32 cars, 11 variables (built into R)

cars <- mtcars %>%
  mutate(
    car   = rownames(mtcars),
    cyl_f = factor(cyl),
    trans = factor(am, levels = c(0, 1), labels = c("Automatic", "Manual"))
  )

str(mtcars)
# ---- desc
summary(cars[, c("mpg", "wt", "hp", "qsec")])
sapply(cars[, c("mpg", "wt", "hp", "qsec")], sd)
# ---- normality
shapiro.test(cars$mpg)
shapiro.test(cars$wt)
shapiro.test(cars$hp)

# Histogram with normal curve, and Q-Q plot
p1a <- ggplot(cars, aes(x = mpg)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 3, boundary = 10,
                 closed = "left", fill = "#264653", color = "white") +
  stat_function(fun = dnorm,
                args = list(mean = mean(cars$mpg), sd = sd(cars$mpg)),
                color = "#E76F51", linewidth = 1) +
  labs(x = "Miles per gallon", y = "Density") + theme_minimal(base_size = 13)

p1b <- ggplot(cars, aes(sample = mpg)) +
  stat_qq(color = "#264653") + stat_qq_line(color = "#E76F51") +
  labs(x = "Theoretical normal quantiles", y = "Sample quantiles (mpg)") +
  theme_minimal(base_size = 13)
# ---- corr
num_vars <- c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
cor_mpg  <- sapply(mtcars[, num_vars], cor, y = mtcars$mpg)
cor_df   <- data.frame(variable = names(cor_mpg), r = cor_mpg) %>%
  mutate(variable = reorder(variable, r))

p2 <- ggplot(cor_df, aes(x = r, y = variable, fill = r > 0)) +
  geom_col(show.legend = FALSE) +
  geom_vline(xintercept = 0) +
  scale_fill_manual(values = c("TRUE" = "#2A9D8F", "FALSE" = "#E76F51")) +
  labs(x = "Pearson correlation with mpg", y = NULL) +
  theme_minimal(base_size = 13)

round(sort(cor_mpg), 2)
# ---- h1
# H0: true correlation between weight and mpg is 0;  H1: it is not 0
cor.test(cars$wt, cars$mpg)                         # Pearson
cor.test(cars$wt, cars$mpg, method = "spearman",    # rank-based check
         exact = FALSE)
# ---- h2
# H0: mean mpg is equal for automatic and manual cars;  H1: it is not
var.test(mpg ~ trans, data = cars)       # equal-variance check
t.test(mpg ~ trans, data = cars)         # Welch t-test (default)
wilcox.test(mpg ~ trans, data = cars,    # non-parametric check
            exact = FALSE)
# ---- h3
# H0: mean mpg is equal across 4, 6 and 8 cylinder cars;  H1: at least one differs
anova_fit <- aov(mpg ~ cyl_f, data = cars)
summary(anova_fit)
TukeyHSD(anova_fit)

# Assumption checks and robust alternatives
leveneTest(mpg ~ cyl_f, data = cars)     # equal variances?
oneway.test(mpg ~ cyl_f, data = cars)    # Welch ANOVA (unequal variances)
kruskal.test(mpg ~ cyl_f, data = cars)   # non-parametric

p3 <- ggplot(cars, aes(x = cyl_f, y = mpg, fill = cyl_f)) +
  geom_boxplot(width = 0.5, alpha = 0.85, outlier.shape = NA, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  scale_fill_manual(values = c("#2A9D8F", "#E9C46A", "#E76F51")) +
  labs(x = NULL, y = "Miles per gallon") + theme_minimal(base_size = 13)
# ---- cv
# Candidate models, from simple to complex
model_formulas <- list(
  M1 = mpg ~ wt,
  M2 = mpg ~ wt + hp,
  M3 = mpg ~ wt + qsec + am,
  M4 = mpg ~ wt + hp + am,
  M5 = mpg ~ wt * hp,
  M6 = mpg ~ wt + cyl_f,
  M7 = mpg ~ cyl + disp + hp + drat + wt + qsec + am
)

# Fixed (non-random) fold labels 1,2,3,4,5,1,2,... so results are reproducible.
# With folds = 1:n the same function performs leave-one-out cross-validation.
folds <- rep(1:5, length.out = nrow(cars))

cv_predictions <- function(formula, data, folds, inv = identity) {
  pred <- numeric(nrow(data))
  for (k in sort(unique(folds))) {
    fit <- lm(formula, data = data[folds != k, ])          # train on other folds
    pred[folds == k] <- inv(predict(fit, newdata = data[folds == k, ]))
  }
  pred                                                       # out-of-fold predictions
}

rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))
mae  <- function(obs, pred) mean(abs(obs - pred))
r2   <- function(obs, pred) 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)

results <- do.call(rbind, lapply(names(model_formulas), function(m) {
  f    <- model_formulas[[m]]
  fit  <- lm(f, data = cars)
  pred <- cv_predictions(f, cars, folds)
  loo  <- cv_predictions(f, cars, seq_len(nrow(cars)))
  data.frame(Model      = m,
             Adj_R2     = summary(fit)$adj.r.squared,
             AIC        = AIC(fit),
             CV5_RMSE   = rmse(cars$mpg, pred),
             LOOCV_RMSE = rmse(cars$mpg, loo))
}))
print(results, row.names = FALSE, digits = 4)

res_long <- data.frame(
  Model = rep(results$Model, 2),
  Type  = rep(c("5-fold CV RMSE", "Leave-one-out RMSE"), each = nrow(results)),
  RMSE  = c(results$CV5_RMSE, results$LOOCV_RMSE))

p4 <- ggplot(res_long, aes(x = Model, y = RMSE, fill = Type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#264653", "#E9C46A"), name = NULL) +
  labs(y = "Prediction error, RMSE (mpg)") + theme_minimal(base_size = 13)
# ---- final
final <- lm(mpg ~ wt * hp, data = cars)
summary(final)
confint(final)

# Predict a new car: 3000 lbs and 150 hp
predict(final, newdata = data.frame(wt = 3.0, hp = 150), interval = "prediction")
predict(final, newdata = data.frame(wt = 3.0, hp = 150), interval = "confidence")

# Interaction plot: predicted mpg at three horsepower levels
grid <- expand.grid(wt = seq(1.5, 5.4, length.out = 100),
                    hp = quantile(cars$hp, c(0.10, 0.50, 0.90)))
grid$mpg_hat <- predict(final, newdata = grid)
p7 <- ggplot(grid, aes(x = wt, y = mpg_hat, color = factor(round(hp)))) +
  geom_line(linewidth = 1.2) +
  geom_point(data = cars, aes(x = wt, y = mpg), inherit.aes = FALSE,
             color = "grey60", alpha = 0.7) +
  labs(x = "Weight (1000 lbs)", y = "Predicted mpg", color = "Horsepower") +
  theme_minimal(base_size = 13)
# ---- perf
cv_final <- cv_predictions(model_formulas$M5, cars, folds)
cv_base  <- cv_predictions(model_formulas$M1, cars, folds)
loo_fin  <- cv_predictions(model_formulas$M5, cars, seq_len(nrow(cars)))

perf <- data.frame(
  Model  = c("Baseline mpg ~ wt (5-fold)", "Final mpg ~ wt * hp (5-fold)",
             "Final mpg ~ wt * hp (leave-one-out)"),
  RMSE   = c(rmse(cars$mpg, cv_base), rmse(cars$mpg, cv_final), rmse(cars$mpg, loo_fin)),
  MAE    = c(mae(cars$mpg, cv_base),  mae(cars$mpg, cv_final),  mae(cars$mpg, loo_fin)),
  R2     = c(r2(cars$mpg, cv_base),   r2(cars$mpg, cv_final),   r2(cars$mpg, loo_fin)))
print(perf, row.names = FALSE, digits = 4)

# Largest out-of-fold errors
err <- cars$mpg - cv_final
head(data.frame(car = cars$car, actual = cars$mpg, predicted = cv_final,
                error = err)[order(-abs(err)), ], 4)

p6 <- ggplot(data.frame(pred = cv_final, actual = cars$mpg),
             aes(x = pred, y = actual)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#E76F51") +
  geom_point(size = 3, color = "#264653") +
  coord_equal(xlim = c(8, 36), ylim = c(8, 36)) +
  labs(x = "Predicted mpg (5-fold cross-validation)", y = "Actual mpg") +
  theme_minimal(base_size = 13)
# ---- diag
par(mfrow = c(2, 2))
plot(final)                      # residuals vs fitted, Q-Q, scale-location, leverage
par(mfrow = c(1, 1))

shapiro.test(residuals(final))   # are residuals normal?
bptest(final)                    # constant variance? (Breusch-Pagan)
vif(final)                       # multicollinearity

# Centring the predictors removes most of the collinearity
cars$wt_c <- cars$wt - mean(cars$wt)
cars$hp_c <- cars$hp - mean(cars$hp)
final_c   <- lm(mpg ~ wt_c * hp_c, data = cars)
vif(final_c)

# Influential observations
cooks <- cooks.distance(final)
sort(cooks, decreasing = TRUE)[1:5]
names(which(cooks > 4 / nrow(cars)))
# ---- improve
candidates <- list(
  list(name = "mpg ~ wt * hp (final)",   f = mpg ~ wt * hp,          inv = identity),
  list(name = "log(mpg) ~ wt * hp",      f = log(mpg) ~ wt * hp,     inv = exp),
  list(name = "mpg ~ wt * hp + qsec",    f = mpg ~ wt * hp + qsec,   inv = identity),
  list(name = "mpg ~ wt * hp + am",      f = mpg ~ wt * hp + am,     inv = identity),
  list(name = "mpg ~ wt * hp + disp",    f = mpg ~ wt * hp + disp,   inv = identity),
  list(name = "mpg ~ wt * hp + I(wt^2)", f = mpg ~ wt * hp + I(wt^2), inv = identity)
)

improve <- do.call(rbind, lapply(candidates, function(m) {
  data.frame(
    Model      = m$name,
    CV5_RMSE   = rmse(cars$mpg, cv_predictions(m$f, cars, folds, m$inv)),
    LOOCV_RMSE = rmse(cars$mpg, cv_predictions(m$f, cars, seq_len(nrow(cars)), m$inv)))
}))
print(improve, row.names = FALSE, digits = 4)
