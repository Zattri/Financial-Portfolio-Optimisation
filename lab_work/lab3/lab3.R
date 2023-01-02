library()
x = seq(0,15)
plot(x, exp(x))

expFunc = function(x) exp(x)

curve(expFunc, from=-5, to=0)

newSolution = 0
oldSolution = 4
time = 20

random = runif(n=1)

acceptanceProb = exp((newSolution-oldSolution)/time)
accept = random < acceptanceProb

