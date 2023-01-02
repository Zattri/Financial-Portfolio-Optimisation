library(GA)
source("feature-selection.R")
source("TSP.R")
source("parseData.R")
source("plotbars.R")
source("runGA.R")
source("class-feature-selection.R")

# Travelling sales person problem
tspResults = runGA(noRuns = 30, problem = "tsp")
plotTSPSolution(bestSolution)

# Feature selection using GAs - reload source with different pcrossover values for varied results
results1 = runGA(noRuns = 30, problem = "feature", generations = 10, crossover = 0.8, mutation = 0.1)
results2 = runGA(noRuns = 30, problem = "feature", generations = 10, crossover = 0.4, mutation = 0.4)
results3 = runGA(noRuns = 30, problem = "feature", generations = 10, crossover = 0.2, mutation = 0.2)

# Parsing and plotting the results 
p1 = parseData(data = results1, firstcolumn = 2, noRuns = 30)
p2 = parseData(data = results2, firstcolumn = 2, noRuns = 30)
p3 = parseData(data = results3, firstcolumn = 2, noRuns = 30)
plotbars(p1,p2,p3)

# Class feature selection
results4 = runGA(noRuns = 30, problem = "class-feature", generations = 5, crossover = 0.8, mutation = 0.1)
results5 = runGA(noRuns = 30, problem = "class-feature", generations = 5, crossover = 0.8, mutation = 0.2)
results6 = runGA(noRuns = 30, problem = "class-feature", generations = 5, crossover = 0.8, mutation = 0.4)

# Parsing and plotting the results 
p4 = parseData(data = results4, firstcolumn = 2, noRuns = 30)
p5 = parseData(data = results5, firstcolumn = 2, noRuns = 30)
p6 = parseData(data = results6, firstcolumn = 2, noRuns = 30)
plotbars(p4,p5,p6)

