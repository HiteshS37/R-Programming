install.packages("forecast")
library(forecast)

# Load the nottem dataset
data(nottem)

# Check the class of the dataset
class(nottem)

# Create a color palette for the box plot
my_colors = rainbow(12)
my_colors

# Box plot by month with customizations
boxplot(split(nottem, cycle(nottem)),
        xlab = "Month", ylab = "Temperature (°F)",
        col = my_colors,  # Assign colors to each box
        border = "black",  # Set the border color
        main = "Monthly Average Temperatures in Nottingham",
        names = month.abb,  # Use abbreviated month names as labels
        outline = FALSE)  # Remove outliers

# Plot the time series data
plot(nottem)

# Convert the data into a time series object
data_ts = ts(nottem, frequency = 12)
data_ts

# Decompose the time series data using multiplicative method
d_multiplicative = decompose(data_ts, "multiplicative")
plot(d_multiplicative)

# Decompose the time series data using additive method
d_additive = decompose(data_ts, "additive")
plot(d_additive)

# Convert the data into a time series object
data_ts = ts(nottem, frequency = 12)

# Decompose the time series data
decomp = decompose(data_ts)

# Set the graphical parameters to adjust figure margins
par(mfrow=c(4,1), mar=c(3, 3, 2, 1), oma=c(0, 0, 2, 0))

# Extract components
secular_trend <- decomp$trend
seasonal_trend <- decomp$seasonal
cyclic_variation <- decomp$random
irregular_variation <- data_ts - secular_trend - seasonal_trend - cyclic_variation

# Plot the Secular Trend
plot(secular_trend, main="Secular Trend", ylab="Temperature", xlab="")

# Plot the Seasonal Trend
plot(seasonal_trend, main="Seasonal Trend", ylab="Temperature", xlab="")

# Plot the Cyclic Variation
plot(cyclic_variation, main="Cyclic Variation", ylab="Temperature", xlab="")

# Plot the Irregular Variation
plot(irregular_variation, main="Irregular Variation", ylab="Temperature", xlab="Year")

# Add a common title for all plots
title("Decomposition of Temperature Time Series (nottem dataset)",side = 3, outer = TRUE, line = 1, cex = 2)

