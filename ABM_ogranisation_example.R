library(tidyverse)

rm(list=ls())

# Create Agent characteristics

agents <- c(1,2,3,4,5)
base <- c(25,28,24,35,32)
pay <- c(35,40,32,48,42)
beta <- c(0.5,0.6,0.7,1.1,1.1)
gamma <- c(0.9, 1.1, 0.9, 1, 1.5)
sd = c(4,6,4,7,3)
productivity <- base + pay*beta

sum(productivity)

pay_raise <- pay + 3

productivity2 <- base + pay_raise*beta
sum(productivity2) - 3*5

table = data.frame(agents,
                   base,
                   pay,
                   beta,
                   gamma,
                   sd)


# logarithmic funciton for effort increase after being off target
plot(1:100, log2(1:100), type = 'l', col = 'blue',
     main = 'Low performance aversion',
     xlab = 'Distance from target',
     ylab = 'Increase in effort')


# different aversion parameters for each employee
aversion_param <- c(2,1,2,2.5,3)


# Step by step

# initialize
productivity <- base + pay*beta
prod <- productivity
total_productivity <- 0
projected_productivity <- 0

av_prod <- sum(productivity)/length(productivity)

for (k in 1:10000){
  productivity <- base + pay*beta
for (j in 1:36){
for (i in 1:length(agents)){
  if (productivity[i] < gamma[i]*av_prod){
    off_target <- gamma[i]*av_prod - productivity[i]
    productivity[i] <- base[i] + pay[i]*beta[i] + log(off_target, 2)
    productivity[i] <- rnorm(1, productivity[i], sd[i])
  } else{
    productivity[i] <- rnorm(1, productivity[i], sd[i])
  }
}
  total_productivity[j] <- sum(productivity) # team productivity each month
}
  projected_productivity[k] <- sum(total_productivity)/length(total_productivity) # projected average productivity from each simulation 
}

sum(projected_productivity)/10000 # 378.67

# Simulation after pay raise
for (k in 1:10000){
  productivity <- base + pay_raise*beta
  for (j in 1:36){
    for (i in 1:length(agents)){
      if (productivity[i] < gamma[i]*av_prod){
        off_target <- gamma[i]*av_prod - productivity[i]
        productivity[i] <- base[i] + pay_raise[i]*beta[i] + log(off_target, 2)
        productivity[i] <- rnorm(1, productivity[i], sd[i])
      } else{
        productivity[i] <- rnorm(1, productivity[i], sd[i])
      }
    }
    total_productivity[j] <- sum(productivity) # team productivity each month
  }
  projected_productivity[k] <- sum(total_productivity)/length(total_productivity) # projected average productivity from each simulation 
}

sum(projected_productivity)/10000 - 3*5 # 391.4



plot(1:36, total_productivity, type = 'l', col = "seagreen")
#





if (productivity[1] < gamma[1]*av_prod){
  off_target <- gamma[1]*av_prod - productivity[1]
  productivity[1] <- base[1] + pay[1]*beta[1] + aversion_param[1]*log(off_target, 2)
  productivity[1] <- rnorm(1, productivity[1], sd[1])
} else{
  productivity[1] <- rnorm(1, productivity[1], sd[1])
}




# Define the sigmoid function
sigmoid <- function(a, x, c) {
  a / (1 + exp(-(x - c)))
}

# Set the center of the sigmoid function
c <- 5
a <- 6

# Generate a sequence of x values from 0 to 10
x_values <- seq(0, 10, by = 0.1)

# Calculate the corresponding sigmoid values
y_values <- sigmoid(a, x_values, c)

# Plot the shifted sigmoid function
plot(x_values, y_values, type = "l", col = "blue",
     main = paste("Sigmoid Function Centered at", c),
     xlab = "x", ylab = "sigmoid(x)",
     ylim = c(0, a))
