## Simple gamble between two agents
rm(list=ls())
agents <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
money <- rep(100, 20)


# Define different strategy profiles 1-loss averse strong, 2-loss averse mild, 3-rational
strat <- c(rep(1,5), rep(2,5), rep(3,5), rep(4,5))

# Loss averse
loss_aversion <- function(x, lambda, alpha){
  y <- ifelse(x >= 0,
              x^alpha, # For gains
              -lambda * (-x)^alpha) # For losses
  return(y)
}



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

# Plot individual players
wealth_long <- reshape2::melt(wealth_history, id.vars = "step", variable.name = "agent", value.name = "wealth")
ggplot(wealth_long, aes(x = step, y = wealth, color = agent), legend = FALSE) +
  geom_line() +
  labs(title = "Wealth Changes Over Time", x = "Step", y = "Wealth") +
  theme_minimal()

