library("mopsocd")
# Documentation link https://cran.r-project.org/web/packages/mopsocd/mopsocd.pdf

runMOPSO = function(obj = 2, dim = 11, pop = 20, gen = 100, maximise=0) {
  cat("Running MOPSO")
  return(
    mopsocd(
      fn = eval,
      gn = function(x) {
        g1 = 100 - x[1] - x[2] - x[3] - x[4] - x[5] - x[6] - x[7] - x[8] - x[9] - x[10] - x[11] >= 0.0
        g2 = sum(x) >= 0.0
        return(c(g1,g2))
        # Previous constraints
        # g1 = 100 - sum(x) > 0.0
        # g2 = sum(x) > 0.0
      },
      varcnt = dim,
      fncnt = obj,
      lowerbound = rep(1,dim),
      upperbound = rep(100,dim),
      opt = maximise,
      popsize = pop,
      maxgen = gen
    )
  )
}

plotMOPSO = function(pso, title="Multi-Object PSO", xlab="f1", ylab="f2") {
  plot(-pso$objfnvalues[,1],pso$objfnvalues[,2], main=title, xlab=xlab, ylab=ylab)
}
