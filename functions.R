DisPoisson<- function(lambda=5){
    vmax = qpois(0.999,lambda = lambda)
    vmin = 0
    x = seq(from=vmin, to=vmax, by=1)
    y = dpois(x,lambda = lambda)
    return(list(x,y))
}