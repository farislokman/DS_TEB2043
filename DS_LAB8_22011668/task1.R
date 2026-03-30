install.packages("readxl")
install.packages("dplyr")
install.packages("plotrix")
library(plotrix)
library(readxl)
library(dplyr)

titanic_cleaned <- read_excel("C:/Users/GF53/Documents/DS_TEB2043/DS_LAB8_22011668/Titanic_Cleaned.xlsx")
# Check data
View(titanic_cleaned)

# Visualization 1: Survival by Gender
gender_table <- table(titanic_cleaned$Sex, titanic_cleaned$Survived)
barplot(gender_table,
        col = c("red","green"),
        beside = TRUE,
        legend = c("Died","Survived"),
        main = "Titanic Survival by Gender",
        xlab = "Gender",
        ylab = "Number of Passengers")

## Visualization 2: Age Distribution

hist(titanic_cleaned$Age,
     col = "lightblue",
     border = "black",
     main = "Age Distribution of Titanic Passengers",
     xlab = "Age",
     ylab = "Frequency",
     breaks = 10)

# Visualization 3: Passenger Class Distribution
class_table <- table(titanic_cleaned$Pclass)

pie3D(class_table,
      labels = names(class_table),
      explode = 0.1,
      main = "Passenger Class Distribution",
      col = rainbow(length(class_table)))
