library("UsingR")
library("mco")
getBenchmark <- function(){
  #if the "UsingR" has not been installed on your system, install it. 
  data("fat", package = "UsingR")
  
  #The dataset is described here: https://rdrr.io/cran/UsingR/man/fat.html. 
  #str(fat)  #Uncomment/Run this command to check the structure of the dataset
  
  #Fit a linear model. Dependent/outcome variable is 'body.fat.siri'; independent variables are all those listed after '~'.
  mod <- lm(body.fat.siri ~ age + weight + height + neck + chest + abdomen +
              + hip + thigh + knee + ankle + bicep + forearm + wrist, data = fat)
  return (mod)
}

getData <-function(){
  data("fat", package = "UsingR")
  
  #The dataset is described here: https://rdrr.io/cran/UsingR/man/fat.html. 
  #str(fat)  #Uncomment/Run this command to check the structure of the dataset
  
  #Fit a linear model. Dependent/outcome variable is 'body.fat.siri'; independent variables are all those listed after '~'.
  mod <- getBenchmark()
  
  #Extract the input data from the fitted model. You can extract the data directly from the variable 'fat' but you 
  #will have to explicitly mention all the variables used in the fitting above. 
  xx <- model.matrix(mod)[, -1]   
  yy <- fat$body.fat.siri          #the response variable
  data <- cbind(xx,yy)
  return (data)
}

error <- function(x){
  inc <- which(x == 1)              #'inc' includes those features/variables for which 'string' contains 1
  if (length(inc)==0) 
    return (10E20)     #if  no feature is selected then give a terrible fitness to this solution
  X <- cbind(1, xx[,inc])                #create a matrix of values for all the variables contained in 'inc'
  
  mod <- lm.fit(X, yy)                  #lm.fit computes faster than the 'lm'; because we have to fit thousands of models, use something efficient. 
  class(mod) <- "lm"
  return(mean((mod$residuals)^2))       #return mean-squared error
}

# eval: transform objectives into minimization goal
eval <- function(realx) {
   x <- round(realx)     #NSGA-II evolves real numbers; turn them into binary values
   return (c(sum(x),error(x)))
}  

runNSGA <- function(){
  m = 2 #2 objectives: minimise features, and  error
  data <- getData()
  xx <<- data[,-ncol(data)]    #turn xx and yy into global variables with "<<-";
  yy <<- data[,ncol(data)]
  D=ncol(xx) # 5 bag prices
  maxGen <<- 20
  popsize <- 40
  
  
  cat("NSGA-II begins:\n")
  
  #minimum value of prices is 1,1,1,1,1, and max is 1000,1000,1000,1000,1000
  # Think above comment incorrect, should be values from 0,0,0,0,0 to 1,1,1,1,1
  G=nsga2(fn=eval,idim=D,odim=m,lower.bounds=rep(0,D),upper.bounds=rep(1,D),
          popsize=popsize,generations=1:maxGen)
  
  # show best individuals:
  I=which(G[[maxGen]]$pareto.optimal)
  for(i in I)
  {
    x=round(G[[maxGen]]$par[i,])
    cat(x," f=(",sum(x),",",error(x),")","\n",sep=" ")
  }
  
  return (G)
}

plotNSGA <- function(G){
  I=1:maxGen
  for(i in I)
  { 
    P=G[[i]]$value # objectives f1 and f2
    # color from light gray (75) to dark (1):
    COL=paste("gray",round(76-i*0.75),sep="")
    if(i==1) 
      plot(P,xlim=c(0,13),ylim=c(0,50),xlab="Features",ylab="MSE",cex=0.5,col=COL)
    Pareto=P[G[[i]]$pareto.optimal,]
    # sort Pareto according to x axis:
    I=sort.int(Pareto[,1],index.return=TRUE)
    Pareto=Pareto[I$ix,]
    points(P,type="p",pch=1,cex=0.5,col=COL)
    lines(Pareto,type="l",cex=0.5,col=COL)
  }
  
}

# Test Runs
my_ga = runNSGA()
plotNSGA(my_ga)