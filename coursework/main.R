setwd("C:/Users/Zattri/Desktop/modern_opt/coursework")
source("objective_functions.R")
source("nsga2.R")
source("plotting.R")
source("mopsocd.R")



# MOPSO Results ---------------------------------------------------------------
mopso = runMOPSO(2, 11, 200, 200)
plotMOPSO(mopso, mopso$numsols, "Inverse P/E Ratio", "Value at Risk")
print(mopso$paramvalues[155,])
print(mopso$numsols)



# NSGA Results ----------------------------------------------------------------
ga = run_NSGA(2, 11, 200, 200)
plotNSGA(ga, 200)

plot(ga[[200]]) # This might work better for showing pareto fronts
print_best(ga, 200)





# TESTING AREA ----------------------------------------------------------------
ga1 = run_NSGA(2, 11, 20, 100)
ga2 = run_NSGA(2, 11, 8, 100)
ga3 = run_NSGA(2, 11, 100, 10)

plotbars(ga1, ga2, ga3)


# ---------------------------------------------------------------
# BRUTE FORCE METHOD 

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