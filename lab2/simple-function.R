
#0.5x^3+2x^2 - 2

# define the objective function 
objFun = function(x){
  # return(1.2 * (x-2)^2 + 3.2) 
  return((0.5*x)^3 + (2*x)^2 - 2) 
}  

# define the gradient of objFun i.e. the derivative of the function encoded in objFun()
gradient = function(x){ 
  #return (1.2 * 2 * (x-2))
  return ((0.125*x)^2 + (4*x))
}

# Get default parameters
getParams = function(){
  gd_params <- c(0.1, 0.6, 10E-13, 1000, 0, 4)
  names(gd_params) <- c("x", "alpha", "epsilon", "iter", "from", "to")
  return(gd_params)
}

#Plot the objective function
plot_obj <- function(title, from=0, to=4){
  #Plot the objective function
  curve(objFun(x), from=from, to=to, main=title)
  #Plot crossing lines so they intersect at the minimum
  abline( h = 0, v = 0, col = "red")
}

plot_iterations <- function(x){
  points(x, objFun(x), type="b", col="green")
}

#How to run this script. 

#source("simple-function.R")
#source("grad-descent.R")

#Below are sample runs of this script

#Use Default parameters for the best performance. 
#gd_plot(expression(Default~alpha))
#Alpha too low; slow convergence
#gd_plot(expression(Low~alpha), alpha=0.1)
#Alpha high; convergence even slower
#gd_plot(expression(High~alpha), alpha=0.8)