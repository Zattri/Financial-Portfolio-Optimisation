getClassGrammar <- function(){
  ruleDef <- list(expr = gsrule("<expr><op><expr>", "<func>(<expr>)", "<var>"),
                  func = gsrule("sin", "cos"),
                  op = gsrule("+", "-", "*"),     
                  var = gsrule("X[,<index>]","<const>"),
                  index = gsrule("1","2","3","4","5","6","7","8","9"),
                  const = gsrule("1.0","0.0")    #constants that you wish to use in your expressions
  )               #note index has only a single expansion i.e. "1"; add more expansions if your problem needs more variables
  
  grammarDef <- CreateGrammar(ruleDef)
  return (grammarDef)
}

setupClassData <- function(){
  #Define the data as variables used in the grammar, so that they are available when evolving expressions are evaluated later. 
  
  X <- read.csv(file="breast-cancer-wisconsin.csv", header=FALSE, sep=",")
  Y <<- X[,ncol(X)]
  X <<- X[,-c(1,ncol(X))]
}

getClassFitness <- function(expr){
  #gramEvol package minimises the objective value; return a score such that the lower the better. 
    result <- eval(expr)
    if (length(result) == 1) result = rep(result,length(Y))  #if a constant expression is created e.g. 1.0
                                                             #it will not automatically create a vector of results. 
      
  if (any(is.nan(result)))   #if expression generates an illegal numerical value, penalise it by generating infinity
    return (Inf) 

  error = 0
  for (i in 1:length(Y)){
    if (result[i]>0 && Y[i]==2) {error = error + 1}   #f(inputs) > 0, if not class 4, error
    else if (result[i]<=0 && Y[i]==4)     {error = error + 1}         #else if not class 2, error. 
   
  }
  
  return (error)
  #return (error/length(Y) * 100)  #return error percentage. 
}