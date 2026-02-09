#mark <- c(33, 24, 54, 94, 16, 89, 60, 6, 77, 61, 13, 44, 26, 24, 73, 73, 90, 39, 90, 54);

list_marks <- c();
a <-0;
b <-0;
c <-0;
d <-0;
e <-0;
f <-0;
isPass <- TRUE;
marks <- -1;

while (TRUE){
  marks <- as.numeric(readline(prompt="Enter marks: "));
  
  if (marks == 0){
    break
  }
  
  if (marks <= 49){
    isPass <- FALSE;
    print(paste("Pass: ", isPass));
  } else{
    isPass <- TRUE;
    print(paste("Pass: ", isPass));
  }
  
  if (marks != 0) {
    list_marks <- c(list_marks, marks);
  }
}

for (marks in list_marks){
  if (marks >= 90){
    a <- a+1;
  } else if (marks >= 80 && marks <= 89){
    b <- b+1;
  } else if (marks >= 70 && marks <= 79){
    c <- c+1;
  } else if (marks >= 60 && marks <= 69){
    d <- d+1;
  } else if (marks >= 50 && marks <= 59){
    e <- e+1;
  } else if (marks <= 49){
    f <- f+1;
    isPass <- FALSE;
  }
}


print(paste("A: ",a));
print(paste("B: ",b));
print(paste("C: ",c));
print(paste("D: ",d));
print(paste("E: ",e));
print(paste("F: ",f));