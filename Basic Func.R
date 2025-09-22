# 1. View the first few rows of the dataset
head(mtcars)

# 2. Summary statistics of the dataset
summary(mtcars)

# 3. Dimensions of the dataset
dim(mtcars)

# 4. Names of columns in the dataset
colnames(mtcars)

# 5. Data types of columns
str(mtcars)

# 6. Accessing a specific column
mtcars$mpg

# 7. Number of unique values in a column
length(unique(mtcars$mpg))

# 8. Mean of a column
mean(mtcars$mpg)

# 9. Median of a column
median(mtcars$mpg)

# 10. Standard deviation of a column
sd(mtcars$mpg)

# 11. Plotting a scatter plot
plot(mtcars$mpg, mtcars$wt)

# 12. Creating a histogram
hist(mtcars$mpg)

# 13. Boxplot
boxplot(mtcars$mpg)

# 14. Correlation matrix
cor(mtcars)

# 15. Subsetting data based on a condition
subset(mtcars, mpg > 20)

# 16. Adding a new column
mtcars$disp_per_cyl = mtcars$disp / mtcars$cyl

# 17. Filtering rows based on multiple conditions
mtcars_sub <- subset(mtcars, mpg > 20 & wt < 3)

# 18. Grouping and summarizing data
library(dplyr)
mtcars %>% group_by(cyl) %>% summarise(mean_mpg = mean(mpg))

# 19. Sorting data
mtcars_sorted <- mtcars[order(mtcars$mpg), ]

# 20. Merging datasets
# Assuming another dataset named 'other_data'
merged_data <- merge(mtcars, other_data, by = "common_column")

# 21. Frequency table of a categorical variable
table(mtcars$gear)

# 22. Crosstabulation
table(mtcars$gear, mtcars$vs)

# 23. Linear regression
lm_model <- lm(mpg ~ wt + hp, data = mtcars)

# 24. ANOVA
anova(lm_model)

# 25. t-test
t.test(mtcars$mpg, mu = 20)

# 26. Chi-squared test
chisq.test(mtcars$gear, mtcars$vs)

# 27. Creating a matrix
matrix_data <- matrix(1:9, nrow = 3, ncol = 3)

# 28. Transposing a matrix
t(matrix_data)

# 29. Applying a function to each element of a vector
sapply(mtcars$mpg, function(x) x * 2)

# 30. Writing to a CSV file
write.csv(mtcars, "mtcars_output.csv", row.names = FALSE)

# 31. Square root of a column
sqrt(mtcars$mpg)

# 32. Exponential function
exp(mtcars$mpg)

# 33. Natural logarithm
log(mtcars$mpg)

# 34. Logarithm base 10
log10(mtcars$mpg)

# 35. Absolute values
abs(mtcars$mpg)

# 36. Ceiling and floor functions
ceiling(mtcars$mpg)
floor(mtcars$mpg)

# 37. Trigonometric functions (sin, cos, tan)
sin(mtcars$mpg)
cos(mtcars$mpg)
tan(mtcars$mpg)

# 38. Round to a specified number of decimal places
round(mtcars$mpg, digits = 2)

# 39. Cumulative sum
cumsum(mtcars$mpg)

# 40. Cumulative product
cumprod(mtcars$mpg)

# 41. Min and Max values
min(mtcars$mpg)
max(mtcars$mpg)

# 42. Range of values
range(mtcars$mpg)

# 43. Mean absolute deviation
mad(mtcars$mpg)

# 44. Percentile
quantile(mtcars$mpg, c(0.25, 0.75))

# 45. Rank of values
rank(mtcars$mpg)

# 46. Standardize values (z-score)
scale(mtcars$mpg)

# 47. Element-wise multiplication
mtcars$mpg * 2

# 48. Element-wise addition
mtcars$mpg + mtcars$wt

# 49. Element-wise subtraction
mtcars$mpg - mtcars$wt

# 50. Element-wise division
mtcars$mpg / mtcars$wt

