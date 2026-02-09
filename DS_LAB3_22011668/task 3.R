# 1. Create the data frame
students_data <- data.frame(
  Name = c("Robert", "Hemsworth", "Scarlett", "Evans", "Pratt", 
           "Larson", "Holland", "Paul", "Simu", "Renner"),
  Chemistry = c(59, 71, 83, 68, 65, 57, 62, 92, 92, 59),
  Physics = c(89, 86, 65, 52, 60, 67, 40, 77, 90, 61)
)

# 2. Count failures (score <= 49) for each subject
chem_fails <- sum(students_data$Chemistry <= 49)
phys_fails <- sum(students_data$Physics <= 49)

# 3. Find the highest/best scores
max_chem <- max(students_data$Chemistry)
max_phys <- max(students_data$Physics)

# 4. Identify who got those scores
top_chem_students <- students_data$Name[students_data$Chemistry == max_chem]
top_phys_students <- students_data$Name[students_data$Physics == max_phys]


print(paste("Chemistry Fails:", chem_fails))
print(paste("Physics Fails:", phys_fails))
print(paste("Top Chemistry Student(s):",top_chem_students))
print(paste("Top Physics Student(s):", top_phys_students))