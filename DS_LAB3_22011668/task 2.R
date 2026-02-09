list_student <- list(59, 71, 83, 68, 65, 57, 62, 92, 92, 59)
names(list_student) <- c("Robert", "Hemsworth", "Scarlett", "Evans", "Pratt", 
                         "Larson", "Holland", "Paul", "Simu", "Renner")
scores <- unlist(list_student)

highest <- max(scores)
lowest <- min(scores)
avg <- mean(scores)
high_student <- ""
low_student <- ""

for (student in names(list_student)){
  
  current_score <- list_student[[student]]
  
  if (current_score == highest){
    high_student = student;
  }
  if (current_score == lowest){
    low_student = student;
  }
}

print(paste("Highest Score: ", highest))
print(paste("Lowest Score: ", lowest))
print(paste("Average Score: ", avg))
print(paste("Student with highest score: ", high_student))
print(paste("Student with lowest score: ", low_student))