## Simple gamble between agents - no profiles included
rm(list=ls())
agents <- c(1,2,3,4,5,6,7)
money <- c(100,100,100,100,100,100,100)

# Function for loss aversion
loss_aversion <- function(x){
  lambda <- 2
  alpha <- 0.9
  # Define the loss aversion function
  y <- ifelse(x >= 0,
              x^alpha, # For gains
              -lambda * (-x)^alpha) # For losses
  return(y)
}

# Two random agents engage in a gamble in which each bets a random amount between 1 and 5 and
# has a random chance to win the other's bet. The gamble can be declined by paying 2 to the other
# player. If both players decline there is no change in respective wealth. The gambles are evaluated
# according to the loss aversion function.


# Function for gamble
GambleGamble <- function(x,y){
  agents <- x
  wealth <- y
players <- sample(agents, 2, replace = FALSE)
chance_win <- sample(seq(0.1,0.9,0.1), 1)
chance_lose <- 1 - chance_win
bet1 <- sample(seq(1,5, 1), 1)
bet2 <- sample(seq(1,5, 1), 1)
pot <- bet1+bet2

# define expected value
ev1 <- chance_win * pot - bet1 # ev for p1
ev2 <- chance_lose * pot - bet2 # ev for p2

# define utility (subjective expected value)
ut1 <- loss_aversion(bet2) * chance_win + loss_aversion(-bet1) * chance_lose
ut2 <- loss_aversion(bet1) * chance_lose + loss_aversion(-bet2) * chance_win

# Define opportunity cost
op_cost <- 2

# Make the decision
dec1 <- ifelse(ut1 > op_cost, 'gamble', 'avoid')
dec2 <- ifelse(ut2 > op_cost, 'gamble', 'avoid')

# game
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
} else {
  # Both players avoid, no change
}

# Print the updated wealth
print(wealth)

}

num_steps <- 1000
# Initialize a data frame to store the wealth at each step
wealth_history <- data.frame(step = 1:num_steps, matrix(0, nrow = num_steps, ncol = length(agents)))
names(wealth_history)[2:(length(agents) + 1)] <- agents

for (i in 1:num_steps){
money <- GambleGamble(agents, money)
# Record wealth at each step
wealth_history[i, 2:(length(agents) + 1)] <- money
}

library(ggplot2)
wealth_long <- reshape2::melt(wealth_history, id.vars = "step", variable.name = "agent", value.name = "wealth")
ggplot(wealth_long, aes(x = step, y = wealth, color = agent)) +
  geom_line() +
  labs(title = "Wealth Changes Over Time", x = "Step", y = "Wealth") +
  theme_minimal()

