library(ggplot2)

DisGamma<- function(x=list(1,0.5,TRUE)){
    shape = x[[1]]
    sca = x[[2]]
    isscale = x[[3]]
    if((shape<0)|(sca<0)){
        return('NA')
    }
    if(!isscale) {
        sca = 1/sca
    }
    vmax = qgamma(0.999,shape=shape,scale = sca)
    vmin = min(0,qgamma(0.01,shape=shape,scale = sca))
    x = seq(from=vmin, to=vmax, by=(vmax-vmin)/200)
    y = dgamma(x,shape=shape,scale = sca)
    return(list(x,y))
}


DisBeta<- function(x=list(2,3)){
    shape1 = x[[1]]
    shape2 = x[[2]]
    if((shape1<0)|(shape2<0)){
        return('NA')
    }
    vmax = 1
    vmin = 0
    x = seq(from=vmin, to=vmax, by=(vmax-vmin)/200)
    y = dbeta(x,shape1,shape2)
    return(list(x,y))
}


DisCauchy<- function(x=list(5,1)){
    location = x[[1]]
    scale = x[[2]]
    if(scale<0){
        return('NA')
    }
    vmax = qcauchy(0.99,location = location, scale = scale)
    vmin = qcauchy(0.01,location = location, scale = scale)
    x = seq(from=vmin, to=vmax, by=(vmax-vmin)/200)
    y = dcauchy(x,location = location, scale = scale)
    return(list(x,y))
}


DisChisquare<- function(x=list(3)){
    df=x[[1]]
    if(df<0){
        return('NA')
    }
    vmax = qchisq(0.99,df = df)
    vmin = 0
    x = seq(from=vmin, to=vmax, by=(vmax-vmin)/200)
    y = dchisq(x,df = df)
    return(list(x,y))
}


DisExponential<- function(x=list(3)){
    rate = x[[1]]
    if(rate<0){
        return('NA')
    }
    vmax = qexp(0.99,rate = rate)
    vmin = 0
    x = seq(from=vmin, to=vmax, by=(vmax-vmin)/200)
    y = dexp(x,rate = rate)
    return(list(x,y))
}

DisBinomial<- function(x=list(100,0.1)){
    shape1 = x[[1]]
    shape2 = x[[2]]
    if((shape1%%1!=0)|(shape1<0)|(shape2<0)|(shape2>1)){
        return('NA')
    }
    vmax = shape1
    vmin = 0
    x = seq(from=vmin, to=vmax, by=1)
    y = dbinom(x,shape1,shape2)
    return(list(x,y))
}


DisGeometric<- function(x=list(0.5)){
    shape1 = x[[1]]
    if((shape1<0)|(shape1>1)){
        return('NA')
    }
    vmax = qgeom(0.999,prob=shape1)
    vmin = 1
    x = seq(from=vmin, to=vmax, by=1)
    y = dgeom(x,shape1)
    return(list(x,y))
}

DisNegBinomial<- function(x=list(10,0.5)){
    size = x[[1]]
    prob = x[[2]]
    if((size%%1!=0)|(size<0)|(prob<0)|(prob>1)){
        return('NA')
    }
    vmax = qnbinom(0.999,size = size, prob = prob)
    vmin = 0
    x = seq(from=vmin, to=vmax, by=1)
    y = dnbinom(x,size = size, prob = prob)
    return(list(x,y))
}

DisPoisson<- function(x=list(5)){
    lambda=x[[1]]
    if((lambda%%1!=0)){
        return('NA')
    }
    vmax = qpois(0.999,lambda = lambda)
    vmin = 0
    x = seq(from=vmin, to=vmax, by=1)
    y = dpois(x,lambda = lambda)
    return(list(x,y))
}

DisNormal<- function(x=list(3,1)){
    mean = x[[1]]
    sd = x[[2]]
    vmax = qnorm(0.99,mean = mean, sd = sd)
    vmin = qnorm(0.01,mean = mean, sd = sd)
    x = seq(from=vmin, to=vmax, by=(vmax-vmin)/200)
    y = dnorm(x,mean = mean, sd = sd)
    return(list(x,y))
}

DisT <- function(x=list(3)){
    df=x[[1]]
    if(df<0){
        return('NA')
    }
    vmax = qt(0.99,df = df)
    vmin = qt(0.01,df = df)
    x = seq(from=vmin, to=vmax, by=(vmax-vmin)/200)
    y = dt(x,df = df)
    return(list(x,y))
}


LinePlot = function(input){
    x = input[[1]]
    y = input[[2]]
    w = input[[3]]
    d = input[[4]]
    distribution = input[[5]]
    options(repr.plot.width = w, repr.plot.height = d)
    dt = data.frame(x = x, y = y)
    gg = (ggplot(dt) + geom_line(aes(x = x, y = y))
          + theme_bw() + theme(panel.background = element_blank(), panel.border = element_blank())
          + theme(axis.line = element_line(size = 1))
          + theme(axis.text = element_text(size = 20, face = "italic"), axis.title = element_text(size = 25,  face = "italic"))
          + labs(title = distribution, x = 'x', y = 'f(x)\n' )
          + theme(plot.title = element_text(size = 30, hjust = 0.5,  face = "italic"))
          # + geom_vline(aes(xintercept = mean_x), linetype = 5)
         )
    return(gg)
}

DotPlot = function(input){
    x = input[[1]]
    y = input[[2]]
    w = input[[3]]
    d = input[[4]]
    distribution = input[[5]]
    options(repr.plot.width = w, repr.plot.height = d)
    dt = data.frame(x = x, y = y)
    gg = (ggplot(dt) + geom_point(aes(x = x, y = y))
          + theme_bw() + theme(panel.background = element_blank(), panel.border = element_blank())
          + theme(axis.line = element_line(size = 1))
          + theme(axis.text = element_text(size = 20, face = "italic"), axis.title = element_text(size = 25,  face = "italic"))
          + labs(title = distribution, x = 'x', y = 'f(x)\n' )
          + theme(plot.title = element_text(size = 30, hjust = 0.5,  face = "italic"))
          # + geom_vline(aes(xintercept = mean_x), linetype = 5)
         )
    return(gg)
}


# 输入参数顺序为 分布名，图的长宽，分布参数。 输出为ggplot元素
PlotDistribution<- function(disname='Gamma',figparam=list(10,5),param=NULL){
    if (!(disname %in% c('Gamma','Beta','Cauchy','Chisquare','Exponential','Normal','StudentT','Binomial','Geometric','NegBinomial','Poisson'))){
        return('Not Implemented')
    }
    if (length(param)==0){
        data = switch(disname,'Gamma'=DisGamma(), 
                   'Beta'=DisBeta(),
                   'Cauchy'=DisCauchy(),
                   'Chisquare'=DisChisquare(),
                   'Exponential'=DisExponential(),
                   'Normal'=DisNormal(),
                   'StudentT'=DisT(),
                   'Binomial'=DisBinomial(),
                   'Geometric'=DisGeometric(),
                   'NegBinomial'=DisNegBinomial(),
                   'Poisson'=DisPoisson())
    }
    else{
        data = switch(disname,'Gamma'=DisGamma(param), 
               'Beta'=DisBeta(param),
               'Cauchy'=DisCauchy(param),
               'Chisquare'=DisChisquare(param),
               'Exponential'=DisExponential(param),
               'Normal'=DisNormal(param),
               'StudentT'=DisT(param),
               'Binomial'=DisBinomial(param),
               'Geometric'=DisGeometric(param),
               'NegBinomial'=DisNegBinomial(param),
               'Poisson'=DisPoisson(param))
    }
    plotparam = list(data[[1]],data[[2]],figparam[[1]],figparam[[2]],disname)
    if (disname %in% c('Gamma','Beta','Cauchy','Chisquare','Exponential','Normal','StudentT')){
        figure = LinePlot(plotparam)
    }
    else if (disname %in% c('Binomial','Geometric','NegBinomial','Poisson')){
        figure = DotPlot(plotparam)
    }
    return(figure)
}