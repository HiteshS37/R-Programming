# 1. Binary Classification vs. point estimation
mtcars$binary_outcome = ifelse(mtcars$mpg > median(mtcars$mpg), 1, 0)
mtcars$binary_outcome
help(ifelse)
 
logistic_model = glm(binary_outcome ~ wt + hp, data = mtcars, family = binomial)
logistic_model

point_estimation = predict(logistic_model, newdata = mtcars, type = "response")
point_estimation

# 2. Odds VS Probability
odds = exp(point_estimation)
odds
probability = odds / (1 + odds)
probability

# 3. Logit Function
logit_function = log(odds)
logit_function

# 4. Classification matrix
classification_matrix = table(mtcars$binary_outcome, point_estimation > 0.5)
classification_matrix

# 5. Confusion matrix
conf_matrix = table(mtcars$binary_outcome, point_estimation > 0.5)
conf_matrix

# 6. Individual Group Classification Efficiency
group_classification_efficiency = diag(conf_matrix) / rowSums(conf_matrix)
group_classification_efficiency

# 7. Nagelkerke R Square
library(rms)
nagelkerke_r2 = validate(logistic_model)
nagelkerke_r2

# 8. Receiver Operating Characteristic Curve
library(pROC)
roc_curve = roc(mtcars$binary_outcome, point_estimation)
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

# 9. Sensitivity
sensitivity = conf_matrix[2, 2] / sum(conf_matrix[2, ])
sensitivity

# 10. Specificity
specificity = conf_matrix[1, 1] / sum(conf_matrix[1, ])
specificity

# 11. Area under ROC curve
auc_value = auc(roc_curve)
auc_value

# 12. Cut-off for optimal sensitivity and specificity
# coords = coordinates of roc curve
optimal_cutoff = coords(roc_curve, "best")
optimal_cutoff

# 13. True Positive and False positive rate
true_positive_rate = conf_matrix[2, 2] / sum(conf_matrix[2, ])
true_positive_rate
false_positive_rate = conf_matrix[1, 2] / sum(conf_matrix[1, ])
false_positive_rate


# Assuming 'mtcars' dataset is loaded

# 1. Binary outcome variable creation (e.g., mpg > median)
mtcars$binary_outcome <- ifelse(mtcars$mpg > median(mtcars$mpg), 1, 0)

# 2. Logistic Regression
logistic_model <- glm(binary_outcome ~ wt + hp, data = mtcars, family = binomial)

# 3. Summary of Logistic Regression
summary(logistic_model)

# 4. Predictions from the model
predictions <- predict(logistic_model, newdata = mtcars, type = "response")

# 5. Confusion Matrix
conf_matrix <- table(mtcars$binary_outcome, predictions > 0.5)

# 6. Accuracy calculation
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

# 7. ROC curve
library(pROC)
roc_curve <- roc(mtcars$binary_outcome, predictions)
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

# 8. AUC (Area Under the Curve) calculation
auc_value <- auc(roc_curve)

# 9. Sensitivity and Specificity
sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
specificity <- conf_matrix[1, 1] / sum(conf_matrix[1, ])

# 10. Odds ratios
odds_ratios <- exp(coef(logistic_model))
odds_ratios

# 11. Wald test for each coefficient
wald_test <- summary(logistic_model)$coefficients[, "Pr(>|z|)"]

# 12. Likelihood Ratio Test
lr_test <- lrtest(logistic_model, null = glm(binary_outcome ~ 1, data = mtcars, family = binomial))

# 13. Akaike Information Criterion (AIC)
model_aic <- AIC(logistic_model)

# 14. Cross-validation for logistic regression
library(boot)
cv_results <- cv.glm(data = mtcars, glmfit = logistic_model, cost = function(y, yhat) -sum((y - yhat)^2))

# 15. Variable selection with stepwise logistic regression
stepwise_model <- step(logistic_model, direction = "both")

# 16. Regularized logistic regression (L1 penalty - Lasso)
library(glmnet)
lasso_model <- cv.glmnet(as.matrix(mtcars[, c("wt", "hp")]), mtcars$binary_outcome, alpha = 1, family = "binomial")

# 17. Regularized logistic regression (L2 penalty - Ridge)
ridge_model <- cv.glmnet(as.matrix(mtcars[, c("wt", "hp")]), mtcars$binary_outcome, alpha = 0, family = "binomial")

# 18. Regularized logistic regression (Elastic Net)
elastic_net_model <- cv.glmnet(as.matrix(mtcars[, c("wt", "hp")]), mtcars$binary_outcome, alpha = 0.5, family = "binomial")

# 19. Bayesian logistic regression
library(rstanarm)
bayesian_logistic_model <- stan_glm(binary_outcome ~ wt + hp, data = mtcars, family = "binomial")

# 20. Robust logistic regression
library(MASS)
robust_logistic_model <- glmrob(binary_outcome ~ wt + hp, data = mtcars, family = binomial)

# 21. Generalized Additive Model (GAM) for logistic regression
library(mgcv)
gam_logistic_model <- gam(binary_outcome ~ s(wt) + s(hp), data = mtcars, family = binomial)

# 22. Zero-Inflated Logistic regression
library(pscl)
zeroinfl_logistic_model <- zeroinfl(binary_outcome ~ wt + hp | 1, data = mtcars, dist = "binomial")

# 23. Neural Network for logistic regression
library(neuralnet)
nn_logistic_model <- neuralnet(binary_outcome ~ wt + hp, data = mtcars, linear.output = TRUE)

# 24. Decision Tree for logistic regression
library(rpart)
tree_logistic_model <- rpart(binary_outcome ~ wt + hp, data = mtcars, method = "class")

# 25. Random Forest for logistic regression
library(randomForest)
rf_logistic_model <- randomForest(binary_outcome ~ wt + hp, data = mtcars)

# 26. Gradient Boosting for logistic regression
library(xgboost)
xgb_logistic_model <- xgboost(data = as.matrix(mtcars[, c("wt", "hp")]), label = mtcars$binary_outcome, nrounds = 100, objective = "binary:logistic")

# 27. Support Vector Machine (SVM) for logistic regression
library(e1071)
svm_logistic_model <- svm(binary_outcome ~ wt + hp, data = mtcars, kernel = "radial", probability = TRUE)

# 28. GLMNET with cross-validation for logistic regression
cv_glmnet_logistic_model <- cv.glmnet(as.matrix(mtcars[, c("wt", "hp")]), mtcars$binary_outcome, alpha = 1, family = "binomial")

# 29. Multinomial logistic regression
multinom_model <- multinom(vs ~ wt + hp, data = mtcars)

# 30. Ordinal logistic regression
library(MASS)
polr_model <- polr(vs ~ wt + hp, data = mtcars, Hess = TRUE)
polr_model

# Loading package
library(dplyr)

# Summary of dataset in package
summary(mtcars)

# For Logistic regression
install.packages("caTools")

# For ROC curve to evaluate model
install.packages("ROCR")	

# Loading package
library(caTools)
library(ROCR)
# Splitting dataset
split = sample.split(mtcars, SplitRatio = 0.8)
split

train_reg = subset(mtcars, split == "TRUE")
test_reg = subset(mtcars, split == "FALSE")

# Training model
logistic_model = glm(vs ~ wt + disp,
                      data = train_reg,
                      family = "binomial")
logistic_model

# Summary
summary(logistic_model)
plot(logistic_model)