### practice_1_answer
### BMI Caculator

# Craete variable called my.height.cm with your actual height in cm 
my.height.cm <- 178

# Craete variable called my.weight.cm with your actual weight in kg
my.weight.kg <- 72

# Create my.height.m transfered by my.height.cm  
my.height.m <- 1.78

# Create my.bmi with BMI(Body Mass Index) formula
my.bmi <-  my.weight.kg / (my.height.m * my.height.m)

# Use if-else to print matched information
# Reference: http://www.tpech.gov.taipei/ct.asp?xItem=1794336&CtNode=30678&mp=109171
if (my.bmi >= 35) {
  print(paste("Your bmi: ", my.bmi))
  print("??çÂ∫¶?Ç•???!")
} else if ( my.bmi<35 ) {
  print(paste("Your bmi: ", my.bmi))
  print("•ø±`!")
}
