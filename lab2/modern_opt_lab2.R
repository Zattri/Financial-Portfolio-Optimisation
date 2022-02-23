# Testing 2nd lesson

# Importing from other R files
source("simple-function.R")
source("grad-descent.R")

# Basic examples
#gd_plot(expression(Default~alpha))
#Alpha too low; slow convergence
#gd_plot(expression(Low~alpha), alpha=0.1)
#Alpha high; convergence even slower
#gd_plot(expression(High~alpha), alpha=0.8)

my_params <- c(0.1, 0.6, 10E-13, 1000, -10, 11)
names(my_params) <- c("x", "alpha", "epsilon", "iter", "from", "to")

# Making my own graph and plotting it
graph_title = expression(F(x) ~0.5~x^3 + 2~x^2 - 2)
gd_plot(graph_title, x=-20, alpha=0.4, my_params["epsilon"], iter=1000, 
        from=-25, to=10)