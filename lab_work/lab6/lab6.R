source("sphere-psoptim.R")

pso = runPSO()

#exportToPDF = plotResults(pso, 1)
plotFitScores(pso,1)
plotSwarmBest(pso)

maxRuns = 30
results = c()

for (i in 1:maxRuns) {
  pso = runPSO()
  results = append(results, tail(pso$stats$error, 1))
}

plot(results, xlab="Run index", ylab="Error score")
