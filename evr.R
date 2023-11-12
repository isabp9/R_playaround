library(extRemes)
library(ggplot2)

data("Flood")
head(Flood)
summary(Flood)
hist(Flood$LOSSPW, 30, col = 2)  #col = 2 is red

#plot values by year 
ggplot(data = Flood, aes( x= HYEAR, y = USDMG)) +
  geom_line()

#fit GEV 
fit <- fevd(USDMG, Flood)   #to estimate how often extreme quantiles occur with a certain return level
fit #location(mu): intercept, scale (alpha): slope at x=0, shape(omega): curvature
distill(fit)
plot(fit) #Quantile-quantile plot (top left), quantiles from a sample drawn from the fitted GEV df against 
#the empirical data quantiles with 95% confidence bands (top right),
#density plots of empirical data and fitted GEV df (bottom left), 
#and return level plot with 95% pointwise normal approximation confidence intervals (bottom right).
return.level(fit) #return level is the value that is expcted to be exceeded once every t with a probabilty t 
# once evrey 2 years with probabilty 1/2 a flood will be >= 1.4, every 10 years with prob 1/10 >= 10 
return.level(fit, do.ci=TRUE)
return.level(fit, make.plot=TRUE)
ci(fit, return.period=c(2,20,100))
