install.packages("readxl")
install.packages("dplyr")
install.packages("plotrix")
library(plotrix)
library(readxl)
library(dplyr)

#Question 2
data("AirPassengers")

df <- data.frame(
  Month = as.numeric(time(AirPassengers)),
  Passengers = as.numeric(AirPassengers)
)
#showTrend over time
plot(df$Month, df$Passengers,
     type = "l",
     col = "maroon",
     main = "Monthly Airline Passenger Trend (1949–1960)",
     xlab = "Year",
     ylab = "Number of Passengers")

#shOw distribution of passengers
hist(df$Passengers,
     col = "lightpink",
     main = "Distribution of Airline Passengers",
     xlab = "Number of Passengers",
     ylab = "Frequency",
     breaks = 10)
print("The AirPassengers dataset shows a strong increasing trend in airline travel from 1949 to 1960, indicating growing demand for air transportation over time. The distribution shows that passenger numbers are concentrated at lower to mid values, with fewer extreme high values in the early years.")
