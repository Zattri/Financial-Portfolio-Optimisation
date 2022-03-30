library("gramEvol")
source("symbreg.R")
#This function is used by GE to compute or report the statistics of your interest after every generation.
monitor <- function(results) {
  print(results)
  print("Best Genome: ")
  print(results$best$genome)

# The following code accesses individual aspects of results. 
# To understand what is available in results: print(str(results))
# For example, following information is available from results  
#  print(results$best$genome) 
#  print(results$best$expression)
#  print(chromToExprList(ga.result$best$genome))
#  print(results$best$cost)

# When reporting results with GE, you must collect best cost value from each generation for every run. much like you did in runGA.R
}


runGE<-function(noRuns = 30, problem = "symbreg"){
  optimizer = "ga"
  monitorFunc = monitor
  iterations = 10
  popSize = 50
  if (problem == "symbreg") {
    setupRegData()
    grammarDef = getRegGrammar()
    evalFunc = getRegFitness
    #monitorFunc = regMonitor
  }
  else if (problem == "class"){
    setupClassData()
    grammarDef = getClassGrammar()
    evalFunc = getClassFitness
  } 
  else if (problem == "multi"){
    setupMultiData()
    grammarDef = getMultiGrammar2()
    evalFunc = getMultiFitness
    popSize = 500
    iterations = 30
  }
  else {
    cat("invalid problem specified. Exiting ... \n")
    return()
  }
    
  for (i in 1:noRuns){
    cat(paste("Starting Run ", i, "\n"))
    set.seed(i)  #note the seed has to be set up externally; GrammaticalEvolution() function does not accept it as an argument
    GrammaticalEvolution(grammarDef = grammarDef,evalFunc = evalFunc,optimizer = optimizer, iterations = iterations, popSize = popSize, monitorFunc = monitorFunc)
  }
    
}  
runGE()
GrammarMap(result$best$genome, getRegGrammar())
