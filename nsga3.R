library("rmoo")

run_nsga3 = function(obj = 2, dim = 11, pop = 20, gen = 100, cprob=0.7, mprob=0.2, summarise=FALSE) {
  cat("Running NSGA-3 \n")
  ga = rmoo::nsga3(
    type="real-valued",
    fitness = eval,
    lower = rep(1,dim),
    upper = rep(100,dim),
    popSize = pop,
    pcrossover = cprob,
    pmutation = mprob,
    maxiter = gen,
    nObj = obj,
    n_partitions = 10,
    summary = summarise
  )
  return(ga)
}