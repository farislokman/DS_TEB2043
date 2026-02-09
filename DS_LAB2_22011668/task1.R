weight <- as.numeric(readline(prompt="Enter weight (XX.X kg): "))
height <- as.numeric(readline(prompt="Enter height (X.XX m): "))


bmi <- round(weight/(height**2), digits = 2)

underweight <- FALSE
normal <- FALSE
overweight <- FALSE
obese <- FALSE

if (bmi <= 18.4){
  underweight <- TRUE
} 
if (bmi >= 18.5 && bmi<= 24.9){
  normal <- TRUE 
} 
if (bmi >= 25.0 && bmi <= 39.9){
  overweight <- TRUE
} 
if (bmi > 40.0) {
  obese <- TRUE
}

print(paste("Underweight:", underweight))
print(paste("Normal:", normal))
print(paste("Overweight:", overweight))
print(paste("Obese:", obese))