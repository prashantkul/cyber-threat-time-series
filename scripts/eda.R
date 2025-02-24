############################################################################################################
# Exploratory Data Analysis
# This script performs exploratory data analysis on the cyber attack dataset.
# It loads the dataset, performs some basic data cleaning, and generates some visualizations.
# Author: Prashant Kulkarni
############################################################################################################

getwd()

# Source the package loader
source("scripts/packages.R")

############################################################################################################
# Load dataset and perform EDA
############################################################################################################

# Load the dataset
cyber_data <- read.csv("data/cyber_data.csv", stringsAsFactors = FALSE)

# Display the first few rows
head(cyber_data)

# Convert date column if necessary
cyber_data$AttackDate <- as.Date(cyber_data$AttackDate, format = "%d/%m/%Y %H:%M")

# Summary statistics
summary(cyber_data)

# Plot a histogram for one variable
ggplot(cyber_data, aes(x = Spam)) +
  geom_histogram(binwidth = 0.001, fill = "blue", color = "white") +
  theme_minimal() +
  labs(title = "Distribution of Spam Attacks", x = "Spam Percentage", y = "Frequency")

# Save the plot
ggsave(filename = "output/plots/spam_histogram.png")

############################################################################################################
# NA's replacement and Time Series Plot
############################################################################################################

# # Replace NAs with 0 for numeric attack type columns
# numeric_vars <- c("Spam", "Ransomware", "Local.Infection", "Exploit", 
#                   "Malicious.Mail", "Network.Attack", "On.Demand.Scan", "Web.Threat")
# cyber_data[numeric_vars] <- lapply(cyber_data[numeric_vars], function(x) ifelse(is.na(x), 0, x))
# 
# # Check summary to verify NA's are replaced
# summary(cyber_data[numeric_vars])
# 
# # Plot a histogram for one variable
# ggplot(cyber_data, aes(x = Spam)) +
#   geom_histogram(binwidth = 0.001, fill = "blue", color = "white") +
#   theme_minimal() +
#   labs(title = "Distribution of Spam Attacks",
#        x = "Spam Percentage",
#        y = "Frequency")

############################################################################################################
# Time Series Plot for a Specific Attack Type
############################################################################################################

# Aggregate daily averages for Local Infection

daily_trends <- cyber_data %>%
  group_by(AttackDate) %>%
  summarise(Local_Infection = mean(Local.Infection, na.rm = TRUE))

ggplot(daily_trends, aes(x = AttackDate, y = Local_Infection)) +
  geom_line(color = "red") +
  theme_minimal() +
  labs(title = "Daily Trend of Local Infection Attacks",
       x = "Date",
       y = "Average Local Infection Percentage")

# Save the plot
ggsave(filename = "output/plots/daily_local_infection_trend.png")

# Correlation Heatmap of Attack Types
# Compute correlation matrix using complete observations
corr_matrix <- cor(cyber_data[numeric_vars], use = "complete.obs")

# Create a correlation plot
corrplot(corr_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         title = "Correlation Matrix of Attack Types", mar = c(0,0,1,0))

# Save the plot
ggsave(filename = "output/plots/correlation_heatmap.png")

# Summary of correlation
summary(corr_matrix)

# Print significant correlations
# 1. Convert the correlation matrix into a data frame of row/column/correlation
corr_df <- as.data.frame(as.table(corr_matrix))
colnames(corr_df) <- c("Var1", "Var2", "Correlation")

# 2. Filter out pairs with a strong absolute correlation
#    Here we use 0.8 as an example threshold; adjust as needed
threshold <- 0.8
strong_corr <- corr_df %>%
  filter(abs(Correlation) > threshold, Var1 != Var2)

# 3. Remove duplicate pairs (since correlation matrix is symmetric)
#    For example, if Var1=Spam and Var2=Local.Infection is in the list,
#    we don’t also want Var1=Local.Infection and Var2=Spam.
strong_corr <- strong_corr %>%
  mutate(OrderedPair = paste0(pmin(Var1, Var2), "_", pmax(Var1, Var2))) %>%
  distinct(OrderedPair, .keep_all = TRUE) %>%
  select(-OrderedPair)

# 4. View or print the results
strong_corr

#######################################
# Cross referance with Ranks
#######################################

# Suppose these are your columns
numeric_vars <- c("Spam", "Ransomware", "Local.Infection", "Exploit", 
                  "Malicious.Mail", "Network.Attack", "On.Demand.Scan", "Web.Threat")
rank_vars <- c("Rank.Spam", "Rank.Ransomware", "Rank.Local.Infection", 
               "Rank.Exploit", "Rank.Malicious.Mail", "Rank.Network.Attack",
               "Rank.On.Demand.Scan", "Rank.Web.Threat")

# Use Spearman's rank correlation for each pair
cor_results <- sapply(seq_along(numeric_vars), function(i) {
  cor(cyber_data[[numeric_vars[i]]], cyber_data[[rank_vars[i]]], 
      use = "complete.obs", method = "spearman")
})

# Label and print
names(cor_results) <- numeric_vars
cor_results

cat("Negative Spearman correlation values indicate that as the attack percentage increases, the rank number decreases (or vice versa). 
    In other words, a country with a higher attack percentage tends to have a “better” (numerically smaller) rank value. 
    This makes sense if rank 1 is assigned to the country with the highest prevalence of that attack type, and the largest rank number indicates the lowest prevalence.")


## EDA for Time series analysis specifically for Local Infection

country_list <- unique(cyber_data$Country)
country_list

# converting "Rank.Spam" column to numeric
cyber_data$Rank.Spam <- as.numeric(cyber_data$Rank.Spam)
cyber_data$Rank.Ransomware <- as.numeric(cyber_data$Rank.Ransomware)

# top 10 for spam and ransomware. This subset will only contain rows (for all dates) corresponding to countries that ever had a Rank.Spam 
# value of 10 or better (meaning a “worse” or “higher” prevalence, depending on how the ranks are defined).
top10_spam <- cyber_data %>%
  filter(Rank.Spam <= 10)

top10_ransomware <- cyber_data %>%
  filter(Rank.Ransomware <= 10)

top10_ransomware


top_spam_ransomware <- cyber_data %>%
  filter(Rank.Spam <= 10 & Rank.Ransomware <= 10)


top_spam_ransomware

# Note: In this dataset, Rank = 1 indicates the highest prevalence (i.e., “worst”) for that particular attack type on a given day. 
# A smaller rank number corresponds to a higher percentage value for the associated threat, and vice versa.

# Now we will select countries that ever appear in top 10 for Spam and Ransome ware using Static Membership for a TD - this will
# keep *all days* for those countries


# 1) Identify countries that were top 10 for both spam & ransomware at any point
top_countries <- cyber_data %>%
  group_by(Country) %>%
  summarise(
    best_spam_rank = min(Rank.Spam),
    best_ransom_rank = min(Rank.Ransomware),
    .groups = "drop"
  ) %>%
  filter(best_spam_rank <= 10 & best_ransom_rank <= 10) %>%
  pull(Country)

# How many countries made it into top_countries?
length(top_countries)
head(top_countries)

# How many rows remain after filtering?
nrow(top_static_data)

cyber_data$AttackDate <- as.Date(cyber_data$AttackDate, format = "%d/%m/%Y %H:%M")
sum(is.na(cyber_data$AttackDate))
head(cyber_data$AttackDate)

# 2) Filter main data to keep *all rows* for these countries
top_static_data <- cyber_data %>%
  filter(Country %in% top_countries)

nrow(top_static_data)


sum(!is.na(top_static_data$Spam))

# 4) Look at a few rows to confirm spam is populated
head(top_static_data[, c("Country", "AttackDate", "Spam", "Rank.Spam")], 20)

# 3) Aggregate daily (example for Spam)

top_static_spam <- top_static_data %>%
  group_by(AttackDate) %>%
  summarise(
    SpamAvg = mean(Spam, na.rm = TRUE),
    .groups = "drop"
  )

head(top_static_spam)

# 4) Plot the time series
ggplot(top_static_spam, aes(x = AttackDate, y = SpamAvg)) +
  geom_line(color = "blue") +
  labs(
    title = "Average Spam Over Time (Countries Ever in Top 10 for Spam & Ransomware)",
    x = "Date",
    y = "Spam %"
  ) +
  theme_minimal()



























