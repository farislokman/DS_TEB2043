install.packages("dplyr")
install.packages("tidyr")
install.packages("stringr")
library(dplyr)
library(tidyr)
library(stringr)

# Updated Path
path <- "C:/Users/GF53/Documents/DS_TEB2043/Lab Assigment 1/Unclean Dataset.csv"

# 1. Load Data
# Using fill=TRUE and strip.white=TRUE as you did to handle irregular pipe-delimited files
data <- read.table(path,
                   sep = "|",
                   header = FALSE,
                   skip = 1,
                   fill = TRUE,
                   strip.white = TRUE,
                   stringsAsFactors = FALSE)

colnames(data) <- c("Student_ID", "First_Name", "Last_Name", "Age", 
                    "Gender", "Course", "Enrollment_Date", "Total_Payments")

# 2. Initial Cleaning (Strings & Symbols)
# Remove currency symbols and commas immediately before numeric conversion
data$Total_Payments <- str_replace_all(data$Total_Payments, "[\\$,]", "")

# 3. Handle Duplicates (Replacing the nested loop)
# duplicated() is much faster than nested for-loops
data <- data[!duplicated(data[, c("First_Name", "Last_Name")]), ]

# 4. Data Type Conversion
data$Student_ID <- as.numeric(data$Student_ID)
data$Age <- as.numeric(data$Age)
data$Enrollment_Date <- as.Date(data$Enrollment_Date)
data$Total_Payments <- as.numeric(data$Total_Payments)

# 5. Handling Missing Values (NAs)
# Remove rows where Student_ID is missing
data <- data[!is.na(data$Student_ID), ]

# Mode function for Enrollment_Date and Total_Payments
get_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

enroll_mode <- get_mode(data$Enrollment_Date)
data$Enrollment_Date[is.na(data$Enrollment_Date)] <- enroll_mode

pay_mode <- get_mode(data$Total_Payments)
data$Total_Payments[is.na(data$Total_Payments)] <- pay_mode

# 6. Standardize Course Names
# Using regex to clean trailing or missing letters
data$Course <- str_trim(data$Course)
data$Course <- gsub("Machine Learnin(g*)", "Machine Learning", data$Course)
data$Course <- gsub("Web Developmen(t*)", "Web Development", data$Course)
data$Course <- as.factor(data$Course)

# 7. Remove ID suffix from Total_Payments
# Logic: If the payment ends with the Student_ID, strip those digits
for (i in 1:nrow(data)) {
  id_str <- as.character(data$Student_ID[i])
  pay_str <- as.character(data$Total_Payments[i])
  
  if (endsWith(pay_str, id_str)) {
    # Calculate how many characters to remove
    new_val <- substr(pay_str, 1, nchar(pay_str) - nchar(id_str))
    data$Total_Payments[i] <- as.numeric(new_val)
  }
}

# 8. Export Cleaned Data
write.csv(data, file = 'cleanedStudent.csv', row.names = FALSE)

print("Data cleaning complete! File saved as cleanedStudent.csv")
print(head(data))