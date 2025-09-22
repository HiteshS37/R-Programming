# Load required libraries
library(forecast)
 
View(AirPassengers)

# 1. Smoothing Techniques (Simple Moving Average)
data(AirPassengers)
ma = ma(AirPassengers, order = 12)  # 12-month moving average
plot(AirPassengers, type = "l", col = "blue", xlab = "Year", ylab = "Passengers", main = "Air Passengers with 12-Month Moving Average")
lines(ma, col = "red")
legend("topleft", legend=c("Original", "12-Month Moving Average"), col=c("blue", "red"), lty=1)

# 2. Time Series Decomposition
decomp = decompose(AirPassengers)
decomp

# 3. Moving Average
ma_6 = filter(AirPassengers, rep(1/6, 6), sides = 2)
plot(AirPassengers, type = "l", col = "blue", xlab = "Year", ylab = "Passengers", main = "Air Passengers with 6-Month Moving Average")
lines(ma_6, col = "red")
legend("topleft", legend=c("Original", "6-Month Moving Average"), col=c("blue", "red"), lty=1)

# 4. Weighted Moving Average
w <- c(1, 2, 3, 4, 5, 6)  # Example weights
wma <- filter(AirPassengers, w, sides = 2)
plot(AirPassengers, type = "l", col = "blue", xlab = "Year", ylab = "Passengers", main = "Air Passengers with Weighted Moving Average")
lines(wma, col = "red")
legend("topleft", legend=c("Original", "Weighted Moving Average"), col=c("blue", "red"), lty=1)

# 5. Exponential Smoothing
fit <- HoltWinters(AirPassengers)
plot(fit, main = "Exponential Smoothing")
legend("topleft", legend=c("Original", "Fitted", "Seasonal"), col=c("blue", "red", "green"), lty=1)

# 6. Double Exponential Smoothing
fit <- HoltWinters(AirPassengers, seasonal = "additive")
plot(fit, main = "Double Exponential Smoothing")
legend("topleft", legend=c("Original", "Fitted", "Seasonal"), col=c("blue", "red", "green"), lty=1)

# 7. Regression Trend Analysis
fit <- lm(AirPassengers ~ time(AirPassengers))
plot(AirPassengers, type = "l", col = "blue", xlab = "Year", ylab = "Passengers", main = "Air Passengers with Regression Trend")
abline(fit, col = "red")
legend("topleft", legend=c("Original", "Regression Trend"), col=c("blue", "red"), lty=1)

# 8. Autocorrelation
acf(AirPassengers, main = "Autocorrelation")

# 9. Autoregression
fit <- ar(AirPassengers)
predicted_values <- predict(fit, n.ahead = length(AirPassengers))
plot(AirPassengers, type = "l", col = "blue", main = "AirPassengers Time Series with AR Model Fit")
lines(predicted_values$pred, col = "red")
legend("topleft", legend=c("Original", "Predicted values"), col=c("blue", "red"), lty=1)

# Load required library
library(pROC)

# Load the Iris dataset
data(iris)

# Convert the species column into a binary outcome (setosa or non-setosa)
iris$Species = factor(iris$Species)
iris$Species
iris$Outcome = ifelse(iris$Species == "setosa", 1, 0)
iris$Outcome

# Create a logistic regression model
model = glm(Outcome ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris, family = binomial)
model

# Predict probabilities
predicted_probabilities = predict(model, type = "response")
predicted_probabilities

# Calculate AUC
auc = roc(iris$Outcome, predicted_probabilities)
auc
# Plot ROC curve
plot(auc, main = "ROC Curve for Setosa Classification", col = "blue")

# Add AUC value to the plot
text(0.8, 0.2, paste("AUC =", round(auc$auc, 2)), col = "red", cex = 1.5)


# Weekly data of COVID-19 positive cases from
# 22 January, 2020 to 15 April, 2020
x = c(580, 7813, 28266, 59287, 75700,
       87820, 95314, 126214, 218843, 471497,
       936851, 1508725, 2072113)

# library required for decimal_date() function
library(lubridate)

# output to be created as png file
png(file ="timeSeries.png")

# creating time series object
# from date 22 January, 2020
mts = ts(x, start = decimal_date(ymd("2020-01-22")),
          frequency = 365.25 / 7)

# plotting the graph
plot(mts, xlab ="Weekly Data",
     ylab ="Total Positive Cases",
     main ="COVID-19 Pandemic",
     col.main ="darkgreen")

# saving the file
dev.off()

# Weekly data of COVID-19 positive cases and
# weekly deaths from 22 January, 2020 to
# 15 April, 2020

positiveCases = c(580, 7813, 28266, 59287,
                   75700, 87820, 95314, 126214,
                   218843, 471497, 936851,
                   1508725, 2072113)

deaths = c(17, 270, 565, 1261, 2126, 2800,
            3285, 4628, 8951, 21283, 47210,
            88480, 138475)

# library required for decimal_date() function
library(lubridate)

# output to be created as png file
png(file="multivariateTimeSeries.png")

# creating multivariate time series object
# from date 22 January, 2020
mts = ts(cbind(positiveCases, deaths),
          start = decimal_date(ymd("2020-01-22")),
          frequency = 365.25 / 7)

# plotting the graph
plot(mts, xlab ="Weekly Data",
     main ="COVID-19 Cases",
     col.main ="darkgreen")

# saving the file
dev.off()

# Weekly data of COVID-19 cases from
# 22 January, 2020 to 15 April, 2020
x = c(580, 7813, 28266, 59287, 75700,
       87820, 95314, 126214, 218843,
       471497, 936851, 1508725, 2072113)

# library required for decimal_date() function
library(lubridate)

# library required for forecasting
library(forecast)

# output to be created as png file
png(file ="forecastTimeSeries.png")

# creating time series object
# from date 22 January, 2020
mts = ts(x, start = decimal_date(ymd("2020-01-22")),
          frequency = 365.25 / 7)

# forecasting model using arima model
fit = auto.arima(mts)

# Next 5 forecasted values
forecast(fit, 5)

# plotting the graph with next
# 5 weekly forecasted values
plot(forecast(fit, 5), xlab ="Weekly Data",
     ylab ="Total Positive Cases",
     main ="COVID-19 Pandemic", col.main ="darkgreen")

# saving the file
dev.off()

# Example daily return data for Stock A and Stock B
stock_A_returns = c(0.01, 0.02, -0.03, 0.01, 0.02, -0.01, 0.005, 0.015)
stock_B_returns = c(0.02, 0.01, -0.02, 0.03, 0.01, -0.01, 0.02, -0.005)

# Function to perform Schmidt-Phillips test for each window
schmidt_phillips_time_series = function(series1, series2, window_size) {
  if (length(series1) < window_size || length(series2) < window_size) {
    stop("Series length is less than window size.")
  }
  
  num_windows = length(series1) - window_size + 1
  p_values = numeric(num_windows)
  
  for (i in 1:num_windows) {
    window_series1 = series1[i:(i + window_size - 1)]
    window_series2 = series2[i:(i + window_size - 1)]
    p_values[i] = t.test(window_series1, window_series2)$p.value
  }
  
  return(p_values)
}

length(stock_A_returns)
length(stock_B_returns)


# Example usage
window_size = 5
p_values = schmidt_phillips_time_series(stock_A_returns, stock_B_returns, window_size)

# Aggregate p-values
aggregated_p_value = mean(p_values)
aggregated_p_value
