head(airquality)
mean(airquality$Solar.R)
mean(airquality$Ozone)
mean(airquality$Wind)
mean(airquality$Solar.R, na.rm = TRUE)
mean(airquality$Ozone, na.rm = TRUE)
summary(airquality)
boxplot(airquality)

New_df = airquality

New_df$Ozone = ifelse(is.na(New_df$Ozone),
                      median(New_df$Ozone,
                             na.rm = TRUE),
                      New_df$Ozone)

summary(New_df)

New_df$Solar.R = ifelse(is.na(New_df$Solar.R),
                        median(New_df$Solar.R,
                               na.rm = TRUE),
                        New_df$Solar.R)

summary(New_df)
head(New_df)
boxplot(New_df)


# Introduce missing values in a few columns
set.seed(123)
mtcars_with_na = mtcars
mtcars_with_na[sample(1:nrow(mtcars), 5), "mpg"] <- NA
mtcars_with_na[sample(1:nrow(mtcars), 3), "cyl"] <- NA
mtcars_with_na[sample(1:nrow(mtcars), 2), "disp"] <- NA
mtcars_with_na[sample(1:nrow(mtcars), 4), "hp"] <- NA

# 1. Initial Data Inspection


# 2. Calculating Means
mean(mtcars_with_na$mpg)
mean(mtcars_with_na$cyl)
mean(mtcars_with_na$disp)
mean(mtcars_with_na$hp)
mean(mtcars_with_na$gear)

# 3. Mean with NA Removal
mean(mtcars_with_na$mpg, na.rm = TRUE)
mean(mtcars_with_na$cyl, na.rm = TRUE)
mean(mtcars_with_na$disp, na.rm = TRUE)
mean(mtcars_with_na$hp, na.rm = TRUE)

# 4. Summary Statistics
summary(mtcars_with_na)

# 5. Boxplot
boxplot(mtcars_with_na)

# 6. Handling Missing Values
New_df1 = mtcars_with_na

New_df1$mpg = ifelse(is.na(New_df1$mpg), 
                     median(New_df1$mpg, na.rm = TRUE), 
                     New_df1$mpg)
New_df1$cyl = ifelse(is.na(New_df1$cyl), 
                     median(New_df1$cyl, na.rm = TRUE), 
                     New_df1$cyl)
New_df1$disp = ifelse(is.na(New_df1$disp), 
                      median(New_df1$disp, na.rm = TRUE), 
                      New_df1$disp)
New_df1$hp = ifelse(is.na(New_df1$hp), 
                    median(New_df1$hp, na.rm = TRUE), 
                    New_df1$hp)

# 7. Summary Statistics for Imputed Data
summary(New_df1)

# 8. Head of the Imputed Data
head(New_df1)

# 9. Boxplot of the Imputed Data
boxplot(New_df1)


x = c(NA, 3, 4, NA, NA, NA)
is.na(x)

x = c(NA, 3, 4, NA, NA, NA)
x

x = c(1, 2, NA, 3, NA, 4)
d = is.na(x)
x[! d]

x = c(1, 2, 0 / 0, 3, NA, 4, 0 / 0)

# Creating a data frame
df = data.frame(c1 = 1:8, 
                c2 = factor(c("B", "A", "B", "C",
                                 "A", "C", "B", "A")))
df

# Filling some NA in data frame
df[4, 1] = df[6, 2] = NA

# Printing all the levels(NA is not considered one)
levels(df$c2)

# fails if NA is encountered
na.fail (df)

# excludes every row containing even one NA
na.exclude (a)

# Create a data frame with 5 rows and 3 columns
data = data.frame(
  A = c(1, 2, NA, 4, 5),
  B = c(NA, 2, 3, NA, 5),
  C = c(1, 2, 3, NA, NA)
)
data

# Finding missing values in data.
sum(is.na(data))

# Finding missing values column wise
colSums(is.na(data))

# Install and load the 'visdat' package
install.packages("visdat")
library(visdat)

# Create a data frame with missing values
data1 = data.frame(
  A = c(1, NA, 3, NA, 5),
  B = c(NA, 2, NA, 4, NA),
  C = c(1, 2, 3, NA, NA)
)
data1

# Plot the missing value diagram
vis_miss(data1)

# Remove missing values using na.omit function.
data = na.omit(data)
data


install.packages("knitr")
library(knitr)
opts_chunk$set(fig.width=9, fig.height=7)
opts_chunk$set(comment="", fig.align="center", tidy=TRUE, fig.retina=2, cache = TRUE)

install.packages("tidyverse")
library(tidyverse)
install.packages("broom")
library(broom)
install.packages("naniar")
library(naniar)
install.packages("simputation")
library(simputation)
install.packages("VIM")
library(VIM)
install.packages("skimr")
library(skimr)

booking = read.csv("https://raw.githubusercontent.com/NicJC/bookings_na/main/GuestNightsNA.csv") 
skimr::skim(booking)
vis_miss(booking)

ggplot(data = booking) +
  geom_miss_point(mapping = aes(x=obs, 
                                y=Nights))  + 
  labs(title = "Missing data points by year") + 
  facet_wrap(~year)

data_na = booking %>%
  replace_with_na(replace = list(Nights = 999999))
data_na %>% skimr::skim()
gg_miss_var(data_na)

ggplot(data = data_na) +
  geom_miss_point(mapping = aes(x=obs, 
                                y=Nights)) + 
  labs(title = "Missing points",
       color = "Year"
  )

data_na %>%
  mutate(Nights = impute_mean(Nights)) %>%
  skimr::skim()

data_na %>%
  bind_shadow() %>%
  mutate(Nights = impute_mean(Nights)) %>%
  ggplot() +
  geom_point(mapping = aes(x =obs, y = Nights, col=Nights_NA))

data_na %>%
  bind_shadow() %>%
  mutate(Nights = impute_mean(Nights))

data_na %>%
  kNN(k=5) %>%
  ggplot() +
  geom_point(mapping = aes(x = obs, 
                           y = Nights, 
                           col=Nights_imp, 
                           size = 1.2))  + 
  labs(title = "KNN5"
  )

data_na %>%
  kNN(k=50) %>%
  ggplot() +
  geom_point(mapping = aes(x = obs, 
                           y = Nights, 
                           col=Nights_imp, 
                           size = 1.2)) + 
  labs(title = "KNN50"
  )

# Creating a simple dataset
data = data.frame(
  Name = c("Alice", "Bob", "Charlie", "David", "Eva", NA),
  Age = c(22, 25, NA, 23, 24, 21),
  Score = c(85, 92, 78, NA, 95, 88),
  Project_Completed = c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE)
)

# Display the original dataset
print("Original Dataset:")
print(data)

# Remove rows with any missing values
clean_data <- na.omit(data)

# Remove rows with missing values in the 'Age' column
clean_data <- data[complete.cases(data$Age), ]

# Display the cleaned dataset
print("Cleaned Dataset after Handling Missing Values:")
print(clean_data)

# Convert 'Age' column to numeric
data$Age <- as.numeric(data$Age)

# Convert 'Project_Completed' column to logical
data$Project_Completed <- as.logical(data$Project_Completed)

# Display the dataset after data type conversion
print("Dataset after Data Type Conversion:")
print(data)

# Detect and remove outliers using Z-score for 'Score' column
z_scores <- scale(data$Score)
z_scores
clean_data <- data[abs(z_scores) < 2, ]

# Display the cleaned dataset after handling outliers
print("Cleaned Dataset after Handling Outliers:")
print(clean_data)

# Remove duplicate rows based on all columns
clean_data <- unique(data)

# Remove duplicate rows based on the 'Name' column
clean_data <- data[!duplicated(data$Name), ]

# Display the cleaned dataset after removing duplicates
print("Cleaned Dataset after Removing Duplicates:")
print(clean_data)

# Convert 'Name' column to lowercase
data$Name <- tolower(data$Name)

# Display the dataset after converting names to lowercase
print("Dataset after Converting Names to Lowercase:")
print(data)

# Load the mtcars dataset
data(mtcars)
original_data = mtcars

# 1. Handling Missing Values
# Introduce missing values in the 'mpg' column
original_data$mpg[c(3, 10, 15)] = NA
original_data

# Remove rows with any missing values
clean_data = na.omit(original_data)
clean_data

# Remove rows with missing values in 'mpg' column
clean_data = original_data[complete.cases(original_data$mpg), ]
clean_data

# 2. Outliers
# Introduce outliers in the 'mpg' column
original_data$mpg[c(2, 20)] = 50
original_data

# Detect and remove outliers using Z-score
z_scores = scale(original_data$mpg)
z_scores
clean_data = original_data[abs(z_scores) < 3, ]
clean_data

# 3. Duplicate Data Removal
# Introduce duplicate rows
original_data = rbind(original_data, original_data[1:2, ])
original_data

# Remove duplicate rows based on all columns
clean_data = unique(original_data)
clean_data

# Remove duplicate rows based on 'mpg' column
clean_data = original_data[!duplicated(original_data$mpg), ]
clean_data

# 4. Data Type Conversion
# Convert 'mpg' column to character (for demonstration)
original_data$mpg = as.character(original_data$mpg)
original_data

# Convert 'mpg' column back to numeric
original_data$mpg = as.numeric(original_data$mpg)
original_data

# Convert 'am' column to a factor
original_data$am = as.factor(original_data$am)
original_data

# 5. String Cleaning and Standardization
# Convert 'car' column to lowercase
original_data$car = tolower(original_data$car)
original_data

# Remove leading and trailing whitespaces in 'car' column
original_data$car = trimws(original_data$car)
original_data