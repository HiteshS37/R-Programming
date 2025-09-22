# Load necessary libraries
library(ggplot2)
library(reshape2)
library(dplyr)

# Create the data frame
data <- data.frame(
  Month = c("February", "June", "June", "June", "June", "July", "July", "July", "July", "August", 
            "September", "September", "October", "November", "November", "November", "November", 
            "November", "December", "May"),
  Event = c("ICC Cricket World Cup League Two 2023-27", "ICC Mens T20 World Cup 2024", 
            "Jersey tour of Denmark, 2024", "Estonia tour of Cyprus 2024", 
            "ICC Mens T20 World Cup Europe Qualifier A, 2024", "India tour of Zimbabwe, 2024", 
            "West Indies tour of England, 2024", "Zimbabwe tour of Ireland, 2024", 
            "South Africa tour of West Indies, 2024", "Sri Lanka tour of England, 2024", 
            "Australia tour of England, 2024", "Ireland v South Africa in UAE, 2024", 
            "England tour of West Indies, 2024", "Pakistan tour of Australia, 2024", 
            "Bangladesh tour of West Indies, 2024", "India tour of Australia, 2024-25", 
            "Sri Lanka tour of South Africa, 2024", "England tour of New Zealand, 2024", 
            "Pakistan tour of South Africa, 2024 -25", "Zimbabwe tour of England, 2025"),
  Host_Country = c("Various", "Various", "Denmark", "Cyprus", "Various", "Zimbabwe", "England", 
                   "Ireland", "West Indies", "England", "England", "UAE", "West Indies", 
                   "Australia", "West Indies", "Australia", "South Africa", "New Zealand", 
                   "South Africa", "England"),
  Notes = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""),
  Tests = c(0, 0, 0, 0, 0, 2, 3, 0, 3, 0, 5, 0, 3, 3, 2, 4, 3, 2, 2, 2),
  ODIs = c(6, 0, 0, 0, 0, 3, 5, 3, 3, 3, 3, 3, 5, 3, 3, 3, 3, 5, 3, 3),
  T20s = c(0, 45, 3, 3, 3, 5, 5, 5, 5, 5, 3, 3, 3, 5, 5, 5, 5, 3, 5, 5)
)


# Sum total matches by type
totals = colSums(data[, c("Tests", "ODIs", "T20s")])

# Create bar chart
barplot(totals, main = "Total Matches by Type (2024-2025)", col = c("skyblue", "orange", "green"),
        ylab = "Number of Matches", xlab = "Match Type")


# Create pie chart for match type distribution
pie(totals, labels = paste(names(totals), round(totals / sum(totals) * 100, 1), "%"), 
    col = c("skyblue", "orange", "green"), main = "Percentage Distribution of Match Types (2024-2025)")


# Create pie chart for match type distribution
pie(totals, labels = paste(names(totals), round(totals / sum(totals) * 100, 1), "%"), 
    col = c("skyblue", "orange", "green"), main = "Percentage Distribution of Match Types (2024-2025)")


# Reshape data for stacked bar chart
data_melted <- melt(data, id.vars = "Event", measure.vars = c("Tests", "ODIs", "T20s"))

# Create stacked bar chart
ggplot(data_melted, aes(x = Event, y = value, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Matches by Event (2024-2025)", x = "Event", y = "Number of Matches") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Summarize data by month
monthly_data <- data %>% group_by(Month) %>% summarise(
  Tests = sum(Tests),
  ODIs = sum(ODIs),
  T20s = sum(T20s)
)

# Reshape data for heat map
monthly_data_melted <- melt(monthly_data, id.vars = "Month")

# Create heat map
ggplot(monthly_data_melted, aes(x = Month, y = variable, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value), color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Monthly Match Intensity (2024-2025)", x = "Month", y = "Match Type")



