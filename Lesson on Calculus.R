#Calculus
#y = x^2

g <- expression(x^2)
D(g,"x")

D(expression(x^2),"x")

f <- expression((2*x^2+5*X-1)/(sqrt(x^2+3)))
D(f,"x")
D(D(f,"x"),"x")
D(D(D(f,"x"),"x"),"x")

library(Deriv)
Deriv(f,"x")

y <- expression(3*cos(x)-(sin(x))^2)
D(y,"x")

#Integration
y <- function(x){
  1/((x+1)*sqrt(x))
}
ans <- integrate(y,lower = 0,upper = Inf)
ans$value
ans$abs.error

#Multidimension
library(cubature)

h <- function(x){
  (3/4)*(2*x[1]+x[2]+4*x[3])
}

ans2 <- adaptIntegrate(h, lowerLimit = c(0,0,0),upperLimit = c(1,0.5,0.25))
ans2$integral
ans2$error
