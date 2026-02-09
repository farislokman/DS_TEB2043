name <- readline(prompt="Enter name: ")
num <- readline(prompt="Enter phone number: ")

numFirst <- substr(num, 1,3)
lastFour <- substr(num, nchar(num) - 3, nchar(num)) #(start) num of characters -3 , (end) num of characters = length of characters

print(paste0("Hi, ", toupper(name), ". A verification code has been sent to ", numFirst, " -XXXXX ", lastFour))