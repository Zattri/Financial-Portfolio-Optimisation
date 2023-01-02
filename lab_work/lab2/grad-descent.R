################################
#### NOTE: This file needs helper functions which are defined in other files, that is: 
####        simple-function.R
####  OR    error_function.R
####  The above are two alternative files that provide identically named functions but with 
####  different functionality; therefore, 'sourcing' any one of them will override the functionality
####  of the other.
####  Start off by sourcing as below: 
####      source("simple-function.R")  
####  OR  source("error_function.R")
####  Then
####      source("grad-descent.R")   # this file
####  See comments in simple-function.R and error_function.R. 

# gradient descent method to find the minimum
gdescent<- function(x, alpha, epsilon, iter){  
  # create a vector to contain all xs for all the steps
  x.All = numeric(iter)
  x.All[1] = x
  
  g = 1
  i=1
  while(i<iter && abs(g) > epsilon){
    g = gradient(x)
    #To check the gradients developing. Comment it if you do not wish to see this ouput.  
    print(paste("solution = ",x, "; gradient =", g))
    
    i = i + 1
    x = x - alpha*g # Subtracting the gradient from the x vector
    x.All[i] = x
    
  }
  
  # print result and plot all xs for every iteration
  print(paste("The minimum of f(x) = ", objFun(x), " at position x = ", x, "; found in ",i-1," iterations.",sep = ""))
  return (x.All[1:i-1])
}


#Plot the objective function; conduct gradient descent; and plot the points visited during the gradient descent. 
gd_plot <- function(title, x=getParams()["x"], alpha=getParams()["alpha"], epsilon=getParams()["epsilon"], 
                    iter=getParams()["iter"], from=getParams()["from"], to=getParams()["to"]){
  plot_obj(title, from, to)
  x.All <-gdescent(x,alpha,epsilon,iter)
  plot_iterations(x.All)
  #return (x.All)
}


