setwd("C:/Users/Zattri/Desktop/modern_opt/coursework")
source("objective_functions.R")
source("nsga2.R")

my_ga = run_NSGA(2, 11, 20, 100)
plot(my_ga[[1]])
print_best(my_ga, 1)
plot(my_ga[[50]])
print_best(my_ga, 50)
plot(my_ga[[100]])
print_best(my_ga, 100)



# ---------------------------------------------------------------
# TESTING FUNCTIONS 

generateWeighting = function(groups = 11) {
  V = 100000 # The sum total of all numbers generated
  G = groups #The amount of numbers to generate
  x = c(rmultinom(1, V, rep.int(1 / G, G))/V)  # Dividing by V to get back to 0-1 range, vectorising for easier output
  return(x)
}

#weightings = c(0,0,0.3,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0) - Example weighting

lowest_price = 10000000000
for (i in 1:100) {
  weightings = generateWeighting()
  cost = sum_price_per_earnings(data=df, weightings=weightings)
  VaR = sum_value_at_risk(data=df, weightings=weightings)
  profits = cost + VaR
  if (profits <= lowest_price) {
    best_weighting = weightings
    lowest_price = profits
  }
}

print(cat("Best weights:", best_weighting, "|", lowest_price))

# ---------------------------------------------------------------