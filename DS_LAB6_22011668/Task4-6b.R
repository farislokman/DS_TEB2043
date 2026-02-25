student.data <- data.frame(
  name = c("Anastasia","Dima","Michael","Matthew","Laura","Kevin","Jonas"),
  score = c(12.5,9.0,16.5,12.0,9.0,8.0,19.0),
  attempts = c(1,3,2,3,2,1,2)
)

student.data$qualify <- c("yes","no","yes","no","no","no","yes")


student.newdata <- data.frame (
  name = c("Emily"),
  score = c(14.5),
  attempts = c(1),
  qualify = c("yes")
)

student.data$qualify <- as.factor(student.data$qualify)
student.finaldata <- rbind(student.data,student.newdata)

str(student.finaldata)
print(summary(student.finaldata))
print(dim(student.finaldata))
print(ncol(student.finaldata))
print(nrow(student.finaldata))

