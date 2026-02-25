student.data <- data.frame(
  student_id = c(1:7),
  student_name = c("Anastasia","Dima","Michael","Matthew","Laura","Kevin","Jonas"),
  student_score = c(12.5,9.0,16.5,12.0,9.0,8.0,19.0),
  student_attempts = c(1,3,2,3,2,1,2)
)

student.data$qualify <- c("yes","no","yes","no","no","no","yes")
v <- student.data
print(v)

