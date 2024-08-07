library(tidyverse)

rm(list=ls())

# How can behavioural science be leveraged in agent based modelling - ogranizational study simple example.

# NOTE: All agent based modelling is extremely reliant on assumptions, some of which are not realistic
# in this example as it serves mostly demonstration purposes.

# We assume that productivity of an agent (which directly translates to firm's returns) can be expressed as:
# Pi = (base i) + (pay i) * (beta i) + (extra effort i) + (error i)

# These terms will be explained step by step

# Create Agent characteristics

# agents - ID
# base - baseline parameter for productivity
# pay - individual pay in hundreds (per month)
# beta - pay parameter (how much it impacts the productivity)
# gamma - ambition threshold (e.g. if it is 0.9 an agent only increases her efforts if they perform worse
# than [average*0.9])
# sd - consistency parameter (we assume that performance has some noise and is normally distributed)

agents <- c(1,2,3,4,5)
base <- c(25,28,24,35,32)
pay <- c(35,40,32,48,42)
beta <- c(0.5,0.6,0.7,1.1,1.1)
gamma <- c(0.9, 1.1, 0.9, 1, 1.5)
sd = c(4,6,4,7,3)
productivity <- base + pay*beta

# Lets say we have psychological profiles of the employees as specified above with individual productivity functions
# that closely reflect reality. We can see right away that increasing pay for all employees is not an effective strategy
# since most of beta parameters are smaller than 1. However, we see that they are quite ambitious and care to perform
# better than average in most cases. The firm launches a programme in which performance is evaluated monthly and compared
# between all employees. If an employee is performing below their expected level they will increase their effort in the next
# month (the extra effort term). Since agents interact through monthly feedback increasing pay can lead to overall greater
# efforts over time.

# Will increasing the pay of all employees by 3 hundred be worth it after 3 years of running the programme?

# Save updated pay
pay_raise <- pay + 3

# calculate current expected returns
sum(productivity) # 306.9

# calculate expected returns after pay raise
productivity2 <- base + pay_raise*beta
sum(productivity2) - 3*5 # 303.9
# Right away the expected return of increasing each employee pay by 300 is -300 in total

# save data as table for overlook
table = data.frame(agents,
                   base,
                   pay,
                   beta,
                   gamma,
                   sd)


# Lets assume employees increase their effors according to logarithmic function
plot(1:100, log2(1:100), type = 'l', col = 'blue',
     main = 'Low performance aversion',
     xlab = 'Distance from target',
     ylab = 'Increase in effort')


# Say some tests are conducted and different aversion parameters are obtained for each employee.
# These will go into the log function. The higher the value the more driven the employee to increase
# their efforts after underperforming.
aversion_param <- c(2,1,2,2.5,3)

# Mind that there is no upper limit for total effort increase, employees could technically
# increase their level of efforts infinitely if they are consistently off target because only
# the previous month serves as the reference point. This may create some unrealistic results for 
# longer simulations but it can be ignored here. An alternative would be to bound the total efforts
# of each employee or even introduce a burnout parameter. Again, depends on assumptions which are crucial
# in real scenarios.

# initialize variables of interests
productivity <- base + pay*beta
prod <- productivity
total_productivity <- 0
projected_productivity <- 0

av_prod <- sum(productivity)/length(productivity)

# Run the model 10,000 times (can set seed for reproducibility here)
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

# get expected monthly productivity
sum(projected_productivity)/10000 # 338.03

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

sum(projected_productivity)/10000 - 3*5 # 339.34


# additional plot for visualization
plot(1:36, total_productivity, type = 'l', col = "seagreen")

# In this case we find that the expected returns after 36 months are greater by approximately 100 per month after the pay rise.




