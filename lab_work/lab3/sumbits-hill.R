### sumbits-hill.R file ###

source("hill.R") # load the hill climbing methods

# sum the number of '1' bits in a given solution (evaluation function):
sumbin=function(x) {
  return (sum(x))
}

#define the tweak function that makes the change into a given solution
tweak=function(par,lower,upper) # integer change
{ 
  hchange(par,lower,upper,rnorm,mean=0,sd=1) 
}

maxRuns = 50
bestI = ""
for (i in 1:maxRuns)
{
  # set up the hill climbing for a single run:
  D=8 # dimension
  s=rep(0,D) # c(0,set0,0,0,...)
  C=list(maxit=30,REPORT=1) # maximum of 30 iterations
  
  
  #Call the hill climber.
  bestI = hclimbing(s,sumbin,lower=rep(0,D),upper=rep(1,D),
            control=C,type="max")
  cat(bestI)
}
