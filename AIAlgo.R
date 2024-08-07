rm(list=ls())

# Define lists
list1 = c(-5,5,10,0,-25,-30,100,0,-10,-15,0,10)
sum(list1)/length(list1) # expected value = 3.33
list2 = c(-5,10,5,5,15,20,-10,10,30,-40,15,25)
sum(list2)/length(list2) # expected value = 6.67


# Reinforcement learning algorithm - we want it to converge to always sample from list 2
learning_rate <- 0.005
n_steps <- 10000
ex1 <- 10
ex2 <- 10
ratio <- ex1/ex2
record <- rep(0, 10000)

# Running the algorithm
for (i in 1:n_steps){
  # define probabilities, starting from expected value of 1 for both lists
  p1 <- exp(ex1)/(exp(ex1) + exp(ex2))
  p2 <- exp(ex2)/(exp(ex1) + exp(ex2))  
 # keep track of probability changes over time
  record[i] <- p1
  
  # randomly choose one list based on the probabilities
  key <- sample(c('list_one', 'list_two'), 1, prob = c(p1,p2))
  
  # sample a value from the chosen list and update the expected value
  if (key == 'list_one'){
    outc <- sample(list1, 1)
    ex1 <- ex1 + (outc/ex2)*learning_rate
  } else {
    outc <-sample(list2, 1)
    ex2 <- ex2 + (outc/ex1)*learning_rate
  }
}

# Visualize the learning process
plot(record, type = 'l', col = 'red', ylab = "Steps",
     xlab = "Index", main = "Changes in probabilities over time")

# Works in most cases however sometimes it erroneously learns to prefer list 1 over list 2.

