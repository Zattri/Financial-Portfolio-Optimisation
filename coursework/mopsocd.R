library("mopsocd")
# Documentation link https://cran.r-project.org/web/packages/mopsocd/mopsocd.pdf

runMOPSO = function(obj = 2, dim = 11, pop = 20, gen = 100, maximise=0) {
  cat("Purchasing gamestop stock")
  return(
    mopsocd(
      fn = eval,
      gn = function(x) {
        g1 = sum(x) - 100 <= 0.0
        g2 = sum(x) > 0.0
        return(c(g1,g2))
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

plotMOPSO = function(pso, xlab="f1", ylab="f2") {
  plot(-pso$objfnvalues[,1],pso$objfnvalues[,2], xlab=xlab, ylab=ylab)
}
