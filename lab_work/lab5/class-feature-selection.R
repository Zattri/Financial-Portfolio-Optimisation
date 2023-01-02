library(rpart)

getData <- function(){
  data(iris)
  dataDT <<- iris
 # DTdata <-cbind(iris[,-ncol(iris)],iris[,-ncol(iris)],  iris)
#  names(DTdata) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "a", "b", "c", "d", "e", "f", "g", "h", "Species" )
  return (dataDT)
  
}

classfeatureFitness <- function(string,xx,yy) {
 
  inc <- which(string == 1)  #'inc' includes those features/variables for which 'string' contains 1
 
  if(sum(inc)==0)                          
    return(0)                          
  
   
  outcome <-"Species"
  inputs <- paste(names(xx)[inc], collapse =" + ")
  
  fRpart <- as.formula(paste(outcome, inputs, sep=" ~ "))
  
  DT <- rpart(formula = fRpart, method="class", control = rpart.control(minsplit = 3),
               data = dataDT)
  
  t_pred = predict(DT,dataDT, type='class')
  
  #Maximise accuracy
  return( mean(dataDT$Species == t_pred))
  
  #Maximise accuracy and minimise the number of features
  #return( mean(dataDT$Species == t_pred) * (1 - sum(string == 1)/length(string) ) )
}

