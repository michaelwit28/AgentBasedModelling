#Agent Based Modelling - Loss aversion, Patience & Gambling
# Supplementary code with different functions for loss aversion

rm(list = ls())
#Loss aversion

x <- seq(-50,50,1)

y <- ifelse(x < 0,
            -log(abs(x), 2),   # Handle negative x
            log(x, 2)) 

plot(x, y, type = "l", col = "blue", lwd = 2,
     main = "Loss Aversion Function",
     xlab = "x", ylab = "y")


# Loss aversion

x <- seq(-50, 50, 1)
# Define the parameters for the loss aversion function
lambda <- 2 # Loss aversion coefficient, typically > 1
alpha <- 0.9 # Parameter for gains
beta <- 0.9 # Parameter for losses
# Define the loss aversion function
y <- ifelse(x >= 0,
            x^alpha, # For gains
            -lambda * (-x)^beta) # For losses
# Plot the function
plot(x, y, type = "l", col = "blue", lwd = 2,
     main = "Loss Aversion Function",
     xlab = "x", ylab = "y",
     ylim = c(min(y), max(y)))
# Add grid for better visualization
grid()


# Define the function
loss_aversion <- function(x){
  lambda <- 2
  alpha <- 0.9
  # Define the loss aversion function
  y <- ifelse(x >= 0,
              x^alpha, # For gains
              -lambda * (-x)^alpha) # For losses
  return(y)
}

y <- loss_aversion(x)
# Plot the function
plot(x, y, type = "l", col = "blue", lwd = 2,
     main = "Loss Aversion Function",
     xlab = "x", ylab = "y",
     ylim = c(min(y), max(y)))
# Add grid for better visualization
grid()


# Define the loss aversion function with parameters
LA <- function(x, lambda, alpha){
  y <- ifelse(x >= 0,
              x^alpha, # For gains
              -lambda * (-x)^alpha) # For losses
  return(y)
}
