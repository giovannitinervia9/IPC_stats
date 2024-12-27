library(stopp)
library(mgcv)
library(spatstat)
library(stpp)
library(colormap)

source("funzioni nuove e internals.R")

set.seed(22)
lgcp2 <- rlgcp(npoints=200, nx=50, ny=50, nt=50, separable=TRUE, 
               model="exponential", param=c(1,1,1,1,1,2), var.grf=2, mean.grf=-0.5*2)
df <- lgcp2$xyt[, 1:3]
stp1 <- stp(df)
plot(stp1)
W <- owin(poly=list(x=c(0.5,1.5,0.5,-0.5),y=c(-0.5,0.5,1.5,0.5)))
l1 <- stppm(stp1, ~x + y, W  = W )
summary(l1$mod_global)

plot.stppm(l1, W)

lgcp1 <- stlgcppm(stp1, ~x + y, W  = W, seed = 2)
lgcp1

plot.stppm(lgcp1, W)



