setwd("C:/Users/Zattri/Desktop/modern_opt/coursework")
source("objective_functions.R")
source("nsga2.R")
source("plotting.R")
source("mopsocd.R")


# MOPSO Results ---------------------------------------------------------------
mopso_a = runMOPSO(2, 11, 100, 100, mprob=0.05)
mopso_b = runMOPSO(2, 11, 100, 100, mprob=0.2)
plotMOPSO(mopso, mopso$numsols, "Inverse P/E Ratio", "Value at Risk")
print(mopso$paramvalues[155,])
print(mopso$numsols)



# NSGA Results ----------------------------------------------------------------
nsga_a = run_NSGA(2, 11, 100, 100, cprob=0.8, mprob=0.05)
nsga_b = run_NSGA(2, 11, 100, 100, cprob=0.95, mprob=0.2)

plotNSGA(nsga_a, 100, colour=TRUE)
plot(-nsga[[100]]$value[,1], nsga[[100]]$value[,2])
plot(paretoFront(nsga[[100]]))


plot(nsga_a[[100]]) # This might work better for showing pareto fronts
print_best(nsga, 200)



# Multi-plot ------------------------------------------------------------------

plot(-mopso_a$objfnvalues[,1],mopso_a$objfnvalues[,2], col="red", main="", xlab="F1 - Inverse P/E Ratio", ylab="F2 - Value at Risk")
points(-mopso_b$objfnvalues[,1],mopso_b$objfnvalues[,2], col="black")
points(-nsga_a[[100]]$value[,1], nsga_a[[100]]$value[,2], col="green")
points(-nsga_b[[100]]$value[,1], nsga_b[[100]]$value[,2], col="blue")
legend("topleft", legend=c("MOPSO_a", "MOPSO_b", "NSGA2_a", "NSGA2_b"), 
       col=c("red", "black", "green", "blue"), fill=c("red", "black", "green", "blue"), cex=0.8)

# NSGA only
plot(-nsga_a[[100]]$value[,1], nsga_a[[100]]$value[,2], col="green", xlab="F1 - Inverse P/E Ratio", ylab="F2 - Value at Risk")
points(-nsga_b[[100]]$value[,1], nsga_b[[100]]$value[,2], col="blue")
legend("topleft", legend=c("NSGA2_a", "NSGA2_b"), 
       col=c("green", "blue"), fill=c("green", "blue"), cex=0.8)



# Invert for variance ---------------------------------------------------------
nsga_a[[100]]$value[,1] = -1 * nsga_a[[100]]$value[,1]
nsga_b[[100]]$value[,1] = -1 * nsga_b[[100]]$value[,1]
mopso_a$objfnvalues[,1] = -1 * mopso_a$objfnvalues[,1]
mopso_b$objfnvalues[,1] = -1 * mopso_b$objfnvalues[,1]

# Variance calculations
var(nsga_a[[100]]$value)
var(nsga_b[[100]]$value)
var(mopso_a$objfnvalues)
var(mopso_b$objfnvalues)



# TESTING AREA ----------------------------------------------------------------
# SINGLE PLOT
singlePlot(convertNSGAdata(nsga_a, objectiveF=1, maximise=TRUE))
singlePlot(convertMOPSOdata(mopso_a, 1, TRUE), mopso = TRUE)


# COMPARISON PLOT
comparisonPlot(convertNSGAdata(nsga_a, 1, TRUE), convertMOPSOdata(mopso_a, 1, TRUE))
comparisonPlot(convertNSGAdata(nsga_a, 2, FALSE), convertMOPSOdata(mopso_a, 2, FALSE))


# MULTI PLOT
ga1 = run_NSGA(2, 11, 20, 100)
ga2 = run_NSGA(2, 11, 20, 100)
ga3 = run_NSGA(2, 11, 20, 100)

ga1_obj1 = convertNSGAdata(ga1, objectiveF=1, maximise=TRUE)
ga2_obj1 = convertNSGAdata(ga2, objectiveF=1, maximise=TRUE)
ga3_obj1 = convertNSGAdata(ga3, objectiveF=1, maximise=TRUE)

plotbars(ga1_obj1, ga2_obj1, ga3_obj1)







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