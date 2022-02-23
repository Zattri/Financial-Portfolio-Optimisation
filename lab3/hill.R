### hill.R file ###

# pure hill climbing:
#    par - initial solution
#    fn - evaluation function or objective function
#    lower - vector with lowest values for each dimension
#    upper - vector with highest values for each dimension
#    control - list with stopping and monitoring method:
#       $maxit - maximum number of iterations
#       $REPORT - frequency of monitoring information
#    type - "min" or "max"
#    ... - extra parameters for FUN
hclimbing=function(par,fn,lower,upper,control,type="min",...)
{ 
  
  fpar=fn(par,...)                      #evaluate the initial solution
  for(i in 1:control$maxit)             #iterate upto a maximum value specified
  { 
      par1=tweak(par,lower,upper)      #tweak and get a new solution
      fpar1=fn(par1,...)                #evaluate the new solution
      
      #This code below simply prints out messages on screen
      if(control$REPORT>2 && (i==1||i%%control$REPORT==0)) 
         cat("i:",i,"s:",par,"f:",fpar,"s'",par1,"f:",fpar1,"\n")

      #Check if the new solution is better than the old solution   
      if(   (type=="min" && fpar1<fpar) || (type=="max" && fpar1>fpar)) 
      { 
        par=par1
        fpar=fpar1 
        bestI = i
      }
  }
  
  if(control$REPORT>=1) 
    cat("best:",par,"f:",fpar,"BestI:",bestI,"\n")
    #return(list(sol=par,eval=fpar))
    return(bestI)
}

# slight random change of vector par:
#    par - initial solution
#    lower - vector with lowest values for each dimension
#    upper - vector with highest values for each dimension
#    dist - random distribution function
#    round - use integer (TRUE) or continuous (FALSE) search
#    ... - extra parameters for dist
#    examples: dist=rnorm, mean=0, sd=1; dist=runif, min=0,max=1
hchange=function(par,lower,upper,dist,round=TRUE,...)
{ 
  D=length(par)           # dimension
  step=dist(D,...)        # slight step
  if(round) step=round(step) 
    par1=par+step
  # return par1 within [lower,upper]:
  return(ifelse(par1<lower,lower,ifelse(par1>upper,upper,par1)))
}
