library("gramEvol")
getMultiGrammar <- function(){
  ruleDef <- list(expr = gsrule("<expr><op><expr>", "(<expr><op><expr>)", "<pre_op>", "<var>"),
                  op = gsrule("&&", "||"),
                  pre_op = gsrule("!(<expr>)", "if (<expr>) { <expr> } else { <expr> }"), 
                  var = gsrule("X[<index>]"),
                  index = gsrule("1", "2", "3", "4", "5", "6")
  )           
  
  grammarDef <- CreateGrammar(ruleDef)
  return (grammarDef)
}

getMultiGrammar2 <- function(){
  ruleDef <- list(expr = gsrule("if (<condition>) { <action> } else { <action> }"),
                  condition = gsrule("(<condition><op><condition>)", "!(<condition>)","<addrBits>"),
                  action = gsrule("(<action><op><action>)","!(<action>)", "<dataBits>", "<expr>"),
                  op = gsrule("&&", "||"),
                  addrBits = gsrule("X[1]","X[2]"),
                  dataBits = gsrule("X[3]","X[4]","X[5]","X[6]")
  )           
  
  grammarDef <- CreateGrammar(ruleDef)
  return (grammarDef)
}


setupMultiData <- function(){
  XX <<- matrix(nrow = 64, ncol = 6)
  Y <<- logical(length = 64)
  for (r in 1:64){
      k = 5
      for (c in 1:6){
        mask = bitwShiftL(1,k)
        ZeroOne = bitwShiftR(bitwAnd((r - 1), mask), k) 
        XX[r,c] <<- (ZeroOne == 1)
        k = k-1  
      }       
  }
  
  for (r in 1:64){
    A0 = 0; A1 = 0;
    if (XX[r,1]) A0 = 1;  if (XX[r,2]) A1 = 1; 
    address = 2*A0+A1
    Y[r] <<- XX[r,3+address]
  }
  
  #print(cbind(X,Y))
}

getMultiFitness <- function(expr){
  #gramEvol package minimises the objective value; return a score such that the lower the better. 
  result <- logical(length = 64)
  for (r in 1:64){
    X <- XX[r,]
    result[r] <- eval(expr)
  }
  
  if (any(is.nan(result)) || any(is.infinite(result)))   #if expression generates an illegal numerical value, penalise it by generating infinity
    return (Inf)      
  
  return ( sum(result != Y))  #mean squared error
}

setupMultiData()
result <- GrammaticalEvolution(getMultiGrammar(), optimizer = "ga", iterations = 10, evalFunc = getMultiFitness,monitorFunc = print)

