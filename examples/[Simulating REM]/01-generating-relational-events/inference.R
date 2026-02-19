source("event-simulator.R")

n_sim = 100
p = 20
n = 1000
b1 = 1
remdats <- sapply(1:n_sim, function(x)
  event_simulator(p = p, n = n,
                  b1 = b1, seed=x),
  simplify =FALSE)

gam.fits <- lapply(remdats, function(x){
  gam(one ~ delta.x - 1,
      family="binomial",
      data=x)
})

boxplot(unlist(lapply(gam.fits, function(x) 
  x$coefficients[1])))
abline(h=b1, col=2, lwd=2)