
getX = function(){
  #data set: sample input values for 'x'
  x <- seq(0.4,2.6, by=0.2)
  return(x)
}

#target or ideal function
getIdeal = function(){
  x<- getX()
  ideal <- (1.2 * (x-2)^2 + 3.2)
  return (ideal)
}

#approximation/guess by our optimisation function
getApprox = function(b){
  x <- getX()
  approx <- (1.2 * (x-b)^2 + 3.2)
  return (approx)
}

# define the objective function 
objFun = function(b){
  x <- getX()
  ideal <- getIdeal()
  approx <- getApprox(b)
  #mean squared error
  mse <- sum((ideal - approx)^2)/length(x)
  return(mse)
}  


# define the gradient of objFun i.e. the derivative of the function encoded in objFun()
gradient = function(b){ 
  x <- getX()
  ideal <- getIdeal()
  approx <- getApprox(b)
  #compute the derivative of 'mse' in objFun with respect to 'b'
  grad <- (1.0/length(x)) * sum (
    2 * (ideal-approx) * 2.4 * (x - b)
  )
  return (grad)
}

# Get default parameters
getParams = function(){
  gd_params <- c(1.2, 0.1, 10E-13, 1000, 0, 5)
  names(gd_params) <- c("x", "alpha", "epsilon", "iter", "from", "to")
  return(gd_params)
}

#Plot the objective function
plot_obj <- function(title, from=getParams()["from"], to=getParams()["to"]){
  #Plot the objective function
  h <- Vectorize(objFun)
  curve(h, from=from, to=to, main=title, xlab='b', ylab="MSE")
  #Plot crossing lines so they intersect at the minimum
  abline( h = 0, v = 2, col = "red")
}

plot_iterations <- function(b){
  h <- Vectorize(objFun)
  points(b, h(b), type="b", col="green")
}

#How to run this script. 

#source("error_function.R")
source("grad-descent.R")

#Below are sample runs of this script

gd_plot(expression(Default~alpha),x=1.6)
gd_plot(expression(Default~alpha),x=1.5)
gd_plot(expression(Default~alpha),x=1.0)
gd_plot(expression(Default~alpha),x=0.3)
gd_plot(expression(Default~alpha),x=3)
gd_plot(expression(Default~alpha),x=-1)

#The above commands all run ok. Now try this below: 

gd_plot(expression(Default~alpha),x=4)
#Errors arise; Why? Big jumps around?
#How can you make them smaller?
gd_plot(expression(Default~alpha),x=4, alpha=0.01)
#Runs ok?
