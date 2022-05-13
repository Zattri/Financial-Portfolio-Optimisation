#Library information - https://cran.r-project.org/web/packages/GPareto/GPareto.pdf
library("GPareto")


runGPareto = function(gen=100) {
  return(
    easyGParetoptim(
      fn=eval,
      budget=gen,
      lower = rep(1,11),
      upper = rep(100,11)
    )
  )
}