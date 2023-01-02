source("classification.R")
library(gramEvol)
getRegGrammar <- function(){
  ruleDef <- list(expr = gsrule("<expr><op><expr>", "<func>(<expr>)", "<var>"),
                  func = gsrule("sin", "cos", "log", "sqrt"),
                  op = gsrule("+", "-", "*"),     
                  var = gsrule("X[,<index>]","<const>"),
                  index = gsrule("1"),
                  const = gsrule("1.0","0.0")    #constants that you wish to use in your expressions
  )               #note index has only a single expansion i.e. "1"; add more expansions if your problem needs more variables
   
  grammarDef <- CreateGrammar(ruleDef)
  return (grammarDef)
}

setupRegData <- function(){
  #Define the data as variables used in the grammar, so that they are available when evolving expressions are evaluated later. 
  
  X1 <- seq(-1,1,by=0.1)                   #single input variable; ranges in [-1:1]; 
  X <<- matrix(nrow = length(X1), ncol=1)  #ncol = 1 because there is only one variable; if you define more variables 
                                           #in the grammar, also add code here to include those variables here. 
  X[,1] <<- X1                             #load the values for the first variable into the data matrix
  
  Y  <<- X[,1]^4 + X[,1]^3 + X[,1]^2 + X[,1]   #the outcome variable. (ideal/target values)  
}

setupRegData()

getRegFitness <- function(expr){
  #gramEvol package minimises the objective value; return a score such that the lower the better. 
  result <- eval(expr)
  if (any(is.nan(result)))   #if expression generates an illegal numerical value, penalise it by generating infinity
    return (Inf)      
  
  return (mean( (Y-result)^2))  #mean squared error
}

#sample call
result <- GrammaticalEvolution(getRegGrammar(), optimizer = "ga", iterations = 10, evalFunc = getRegFitness)
#with generational stats
#result <- GrammaticalEvolution(getRegGrammar(), optimizer = "ga", iterations = 10, evalFunc = getRegFitness,monitorFunc = print)