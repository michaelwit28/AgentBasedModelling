## Simple gamble between two agents
rm(list=ls())
agents <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
money <- rep(100, 20)

# Two random agents engage in a gamble in which each bets a random amount between [1] and [5] and has a random chance
# to win the other player's bet. The gamble can be declined which requires paying [1] to the other player. If both players
# decline there is no change in their respective wealth. The gambles are evaluated individually by each agent against the decline fee
# according to different strategy profiles.


# Define different strategy profiles 1-loss averse strong, 2-loss averse mild, 3-rational, 4-risk seeking
strat <- c(rep(1,5), rep(2,5), rep(3,5), rep(4,5))

# Loss aversion function
loss_aversion <- function(x, lambda, alpha){
  y <- ifelse(x >= 0,
              x^alpha, # For gains
              -lambda * (-x)^alpha) # For losses
  return(y)
}

# Visualize individual preferences
x <- seq(-50, 50, 1)

# Strong loss aversion
y <- ifelse(x >= 0,
            x^0.9, # For gains
            -2.5 * (-x)^0.9) # For losses
# Plot the function
plot(x, y, type = "l", col = "blue", lwd = 2,
     main = "Strong Loss Aversion",
     xlab = "Expected Returns", ylab = "Subjective Utility",
     ylim = c(min(y), max(y)))
# Add grid for better visualization
grid()

# Mild loss aversion
y <- ifelse(x >= 0,
            x^0.9, # For gains
            -1.5 * (-x)^0.9) # For losses
# Plot the function
plot(x, y, type = "l", col = "skyblue", lwd = 2,
     main = "Mild Loss Aversion",
     xlab = "Expected Returns", ylab = "Subjective Utility",
     ylim = c(min(y), max(y)))
# Add grid for better visualization
grid()

# Optimal preferences
y <- x
# Plot the function
plot(x, y, type = "l", col = "darkgreen", lwd = 2,
     main = "Optimal ",
     xlab = "Expected Returns", ylab = "Subjective Utility",
     ylim = c(min(y), max(y)))
# Add grid for better visualization
grid()

# Risk seeking
y <- ifelse(x >= 0,
            x^1.1, # For gains
            -0.8 * (-x)^1.1) # For losses
# Plot the function
plot(x, y, type = "l", col = "brown", lwd = 2,
     main = "Strong Loss Aversion",
     xlab = "Expected Returns", ylab = "Subjective Utility",
     ylim = c(min(y), max(y)))
# Add grid for better visualization
grid()


# Define the gamble function
GambleGamble <- function(x, y, z) {
  agents <- x
  wealth <- y
  profile <- z
  players <- sample(agents, 2, replace = FALSE)
  chance_win <- sample(seq(0.1, 0.9, 0.1), 1)
  chance_lose <- 1 - chance_win
  bet1 <- sample(seq(1, 5, 1), 1)
  bet2 <- sample(seq(1, 5, 1), 1)
  pot <- bet1 + bet2
  
  # Define expected value
  ev1 <- chance_win * pot - bet1 # EV for player 1
  ev2 <- chance_lose * pot - bet2 # EV for player 2
  
  # Define utility (subjective expected value) based on strategy profile
  if (profile[players[1]] == 1) {
    ut1 <- loss_aversion(bet2, 2.5, 0.9) * chance_win + loss_aversion(-bet1, 2.5, 0.9) * chance_lose
  } else if (profile[players[1]] == 2) {
    ut1 <- loss_aversion(bet2, 1.5, 0.9) * chance_win + loss_aversion(-bet1, 1.5, 0.9) * chance_lose
  } else if (profile[players[1]] == 3) {
    ut1 <- ev1
  } else if (profile[players[1]] == 4) {
    ut1 <- loss_aversion(bet2, 0.8, 1.1) * chance_win + loss_aversion(-bet1, 0.8, 1.1) * chance_lose
  }
  
  if (profile[players[2]] == 1) {
    ut2 <- loss_aversion(-bet2, 2.5, 0.9) * chance_win + loss_aversion(bet1, 2.5, 0.9) * chance_lose
  } else if (profile[players[2]] == 2) {
    ut2 <- loss_aversion(-bet2, 1.5, 0.9) * chance_win + loss_aversion(bet1, 1.5, 0.9) * chance_lose
  } else if (profile[players[2]] == 3) {
    ut2 <- ev2
  } else if (profile[players[2]] == 4) {
    ut2 <- loss_aversion(-bet2, 0.8, 1.1) * chance_win + loss_aversion(bet1, 0.8, 1.1) * chance_lose
  }  
  # Define opportunity cost
  op_cost <- 1
  
  # Make the decision
  dec1 <- ifelse(ut1 >= -op_cost, 'gamble', 'avoid')
  dec2 <- ifelse(ut2 >= -op_cost, 'gamble', 'avoid')
  
  # Game
  if (dec1 == "gamble" & dec2 == "gamble") {
    # Both players gamble
    result <- sample(c("win", "lose"), size = 1, prob = c(chance_win, chance_lose))
    if (result == "win") {
      # Player 1 wins
      wealth[players[1]] <- wealth[players[1]] + bet2
      wealth[players[2]] <- wealth[players[2]] - bet2
    } else {
      # Player 2 wins
      wealth[players[1]] <- wealth[players[1]] - bet1
      wealth[players[2]] <- wealth[players[2]] + bet1
    }
  } else if (dec1 == "gamble" & dec2 == "avoid") {
    # Player 1 wants to gamble, Player 2 avoids
    wealth[players[1]] <- wealth[players[1]] + op_cost
    wealth[players[2]] <- wealth[players[2]] - op_cost
  } else if (dec1 == "avoid" & dec2 == "gamble") {
    # Player 1 avoids, Player 2 wants to gamble
    wealth[players[1]] <- wealth[players[1]] - op_cost
    wealth[players[2]] <- wealth[players[2]] + op_cost
  }
  
  # Print the updated wealth
  print(wealth)
  
  # Return the updated wealth
  return(wealth)
}

# We want to see how different strategy profiles perform against each other over the long run

num_steps <- 10000
# Initialize a data frame to store the wealth at each step
wealth_history <- data.frame(step = 1:num_steps, matrix(0, nrow = num_steps, ncol = length(agents)))
names(wealth_history)[2:(length(agents) + 1)] <- agents

# run the simulation
for (i in 1:num_steps){
  money <- GambleGamble(agents, money, strat)
  # Record wealth at each step
  wealth_history[i, 2:(length(agents) + 1)] <- money
}

library(ggplot2)

# Map different strategies
# Melt the wealth_history data frame to long format
wealth_long <- melt(wealth_history, id.vars = "step", variable.name = "agent", value.name = "wealth")

# Ensure that 'agent' and 'strat' correspond correctly
wealth_long$agent <- as.numeric(wealth_long$agent)
wealth_long$profile <- strat[wealth_long$agent]

# Plot using ggplot2 with lines colored by profile
ggplot(wealth_long, aes(x = step, y = wealth, color = as.factor(profile), group = agent)) +
  geom_line() +
  labs(title = "Wealth Changes Over Time", x = "Step", y = "Wealth", color = "Profile") +
  theme_minimal()

# As expected optimal play generates the best expected outcomes while highly loss averse players lose most.
# We can play around by changing parameters or proportions of different players in the population and inspect
# how it affects the expected outcomes.

