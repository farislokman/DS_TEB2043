# Given vector
age <- c(55,57,56,52,51,59,58,53,59,55,60,60,60,60,52,55,56,51,60,
         52,54,56,52,57,54,56,58,53,53,50,55,51,57,60,57,55,51,50,57,58)

# Create age ranges
age_range <- cut(age,
                 breaks = c(50,52,54,56,58,60),
                 include.lowest = TRUE,
                 right = TRUE)

# Count staff per range
range_count <- table(age_range)

# Convert to data frame with 2 columns
age_summary <- data.frame(
  Age_Range = names(range_count),
  Total_Staff = as.vector(range_count)
)

#range from 50-60
all_ages <- 50:60

# Count occurrences
age_count <- table(factor(age, levels = all_ages))

# Convert to data frame (2 columns)
staff_age_summary <- data.frame(
  Staff_Age = all_ages,
  Total_Staff = as.vector(age_count)
)


print(staff_age_summary)
print(age_summary)