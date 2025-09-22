# 1. Simple linear regression
lm_model = lm(mpg ~ wt, data = mtcars)
lm_model

# 2. Multiple linear regression
lm_model_multi = lm(mpg ~ wt + hp + disp, data = mtcars)
lm_model_multi

# 3. Polynomial regression (degree 2)
lm_model_poly = lm(mpg ~ poly(wt, 2), data = mtcars)
lm_model_poly

# 4. Interaction term in regression
lm_model_interact = lm(mpg ~ wt * hp, data = mtcars)
lm_model_interact

# 5. Weighted least squares regression
lm_model_wls = lm(mpg ~ wt, data = mtcars, weights = hp)
lm_model_wls

# 6. Ridge regression
library(MASS)
lm_ridge = lm.ridge(mpg ~ wt + hp, data = mtcars)
lm_ridge

# 7. Lasso regression
library(glmnet)
lasso_model = cv.glmnet(as.matrix(mtcars[, c("wt", "hp")]), mtcars$mpg)
lasso_model

# 8. Stepwise regression
lm_model_stepwise = step(lm_model_multi, direction = "both")
lm_model_stepwise

# 9. Principal Component Regression (PCR)
install.packages("pls")
library(pls)
pcr_model = pcr(mpg ~ wt + hp, data = mtcars, scale = TRUE)
View(pcr_model)
plot(pcr_model, main = "PCR Model", col = "red")

# 10. Partial Least Squares (PLS) Regression
pls_model = plsr(mpg ~ wt + hp, data = mtcars, scale = TRUE)
View(pls_model)
plot(pls_model, main = "PLS Model", col = "green")

# 11. Quantile regression
library(quantreg)
quantile_model = rq(mpg ~ wt + hp, data = mtcars)
quantile_model

# 12. Huber regression
install.packages("robustbase")
library(robustbase)
huber_model = lmrob(mpg ~ wt + hp, data = mtcars)
huber_model

# 13. Generalized Additive Model (GAM)
library(mgcv)
gam_model = gam(mpg ~ s(wt) + s(hp), data = mtcars)
gam_model
plot(gam_model, main = "Gam model", col = "darkblue")

# 14. Bayesian regression
library(rstanarm)
bayesian_model = stan_lm(mpg ~ wt + hp, data = mtcars)
bayesian_model

# 15. Robust regression
lm_model_robust = lmrob(mpg ~ wt + hp, data = mtcars)
lm_model_robust

# 16. Support Vector Regression (SVR)
library(e1071)
svr_model = svm(mpg ~ wt + hp, data = mtcars, kernel = "radial")
svr_model

# 17. Elastic Net regression
library(glmnet)
elastic_net_model = cv.glmnet(as.matrix(mtcars[, c("wt", "hp")]), mtcars$mpg, alpha = 0.5)
elastic_net_model

# 18. Generalized Linear Model (GLM)
glm_model = glm(mpg ~ wt + hp, data = mtcars, family = gaussian)
glm_model

# 19. Tobit regression
library(AER)
tobit_model = tobit(mpg ~ wt + hp, data = mtcars)
tobit_model

# 20. Poisson regression
poisson_model = glm(mpg ~ wt + hp, data = mtcars, family = poisson)
poisson_model

# 21. Negative Binomial regression
library(MASS)
neg_binom_model <- glm.nb(mpg ~ wt + hp, data = mtcars)
neg_binom_model

# 22. Zero-Inflated Poisson regression
library(pscl)
zip_model = zeroinfl(mpg ~ wt + hp, data = mtcars, dist = "poisson")
zip_model

# 23. Zero-Inflated Negative Binomial regression
zinb_model = zeroinfl(mpg ~ wt + hp, data = mtcars, dist = "negbin")
zinb_model

# 24. Tweedie regression
library(statmod)
tweedie_model = glm(mpg ~ wt + hp, data = mtcars, family = tweedie(var.power = 1.5))
tweedie_model

# 25. Quantile Regression Forest
library(quantregForest)
qrf_model = quantregForest(mpg ~ wt + hp, data = mtcars)
qrf_model

# 26. MARS (Multivariate Adaptive Regression Splines)
library(earth)
mars_model = earth(mpg ~ wt + hp, data = mtcars)
mars_model

# 27. Decision Tree Regression
library(rpart)
tree_model = rpart(mpg ~ wt + hp, data = mtcars)
tree_model

# 28. Random Forest Regression
library(randomForest)
rf_model = randomForest(mpg ~ wt + hp, data = mtcars)
rf_model

# 29. Gradient Boosting Regression
library(xgboost)
xgb_model = xgboost(data = as.matrix(mtcars[, c("wt", "hp")]), label = mtcars$mpg, nrounds = 100)
xgb_model
# 30. Neural Network Regression
install.packages("neuralnet")
library(neuralnet)
nn_model = neuralnet(mpg ~ wt + hp, data = mtcars)
nn_model
