# Load libraries
library(tidyverse) # Includes dplyr, stringr, ggplot2, and tidyselect
library(readxl)
library(writexl)

# 1. Import dataset
# Using the path you provided
path_in <- "C:/Users/GF53/Documents/DS_TEB2043/DS_LAB7_22011668/titanic.csv"
titanic_raw <- read.csv(path_in, header = TRUE, stringsAsFactors = FALSE)

# View dataset structure
str(titanic_raw)
dim(titanic_raw)

# 2. Data Cleaning
# FIXED: Removed the redundant assignment inside the pipe
titanic_cleaned <- titanic_raw %>%
  # Remove extra spaces from character columns
  mutate(across(where(is.character), str_trim)) %>%
  
  # Ensure numeric types
  mutate(
    PassengerId = as.numeric(PassengerId),
    Survived = as.numeric(Survived),
    Pclass = as.numeric(Pclass),
    Age = as.numeric(Age),
    SibSp = as.numeric(SibSp),
    Parch = as.numeric(Parch),
    Fare = as.numeric(Fare)
  ) %>%
  
  # Standardize casing
  mutate(
    Sex = str_to_lower(Sex),
    Embarked = str_to_upper(Embarked),
    # Convert empty strings in Cabin to NA
    Cabin = na_if(Cabin, "")
  ) %>%
  
  # Remove rows with missing Age
  filter(!is.na(Age))

# Final type adjustment for Age
titanic_cleaned$Age <- as.integer(titanic_cleaned$Age)

# 3. Save Cleaned Data
# Adjusted path to match your input folder for consistency
path_out_clean <- "C:/Users/GF53/Documents/DS_TEB2043/DS_LAB7_22011668/Titanic_Cleaned.xlsx"
write_xlsx(titanic_cleaned, path_out_clean)

# 4. ANALYSIS

# 4.1 Overall survival rate
overall_survival <- titanic_cleaned %>%
  summarise(Survival_Rate = mean(Survived) * 100)

# 4.2 Survival rate by gender
survival_by_gender <- titanic_cleaned %>%
  group_by(Sex) %>%
  summarise(
    Total_Passengers = n(),
    Survival_Rate = mean(Survived) * 100
  )

# 4.3 Survival rate by passenger class
survival_by_class <- titanic_cleaned %>%
  group_by(Pclass) %>%
  summarise(
    Total_Passengers = n(),
    Survival_Rate = mean(Survived) * 100
  )

# 4.4 Survival rate by embarkation port
survival_by_port <- titanic_cleaned %>%
  group_by(Embarked) %>%
  summarise(
    Total_Passengers = n(),
    Survival_Rate = mean(Survived) * 100
  )

# 4.5 Passenger class distribution
class_distribution <- titanic_cleaned %>%
  count(Pclass) %>%
  mutate(Percentage = (n / sum(n)) * 100)

# 4.6 Average age by survival
age_survival <- titanic_cleaned %>%
  group_by(Survived) %>%
  summarise(Average_Age = mean(Age))

# 4.7 Average fare by passenger class
fare_class <- titanic_cleaned %>%
  group_by(Pclass) %>%
  summarise(Average_Fare = mean(Fare))

# 5. WRITE REPORT
path_out_report <- "C:/Users/GF53/Documents/DS_TEB2043/DS_LAB7_22011668/Titanic_Report_Output.xlsx"
write_xlsx(
  list(
    Cleaned_Data = titanic_cleaned,
    Survival_Gender = survival_by_gender,
    Survival_Class = survival_by_class,
    Survival_Port = survival_by_port,
    Class_Distribution = class_distribution,
    Age_Survival = age_survival,
    Fare_Class = fare_class
  ),
  path_out_report
)

# 6. Plot Graph
ggplot(titanic_cleaned, aes(x = Embarked, fill = factor(Survived))) +
  geom_bar(position = "fill") +
  scale_fill_discrete(name = "Survived", labels = c("No", "Yes")) +
  labs(
    title = "Survival Rate by Embarkation Port",
    x = "Embarked Port",
    y = "Proportion"
  ) +
  theme_minimal()