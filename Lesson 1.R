cat("Lesson One: Version Controls in Rstudio")
print("Hello world")
cat("Hello World")
print("I love programming")


mysample <- c(1:10)
xbar <- c(102,94.8,98.3,98.4,102,98.5,99,97.7,100,98.1)
mydata <- as.data.frame(cbind(mysample,xbar))
mydata
xbar_mu0 <- xbar-99
xbar_mu0
mydata <- cbind(mydata,xbar_mu0)
mydata

s <- NULL
mu0 <- 99
s[1] <- xbar[1]-mu0
for (i in 2:length(mysample)) {
  s[i] <- (xbar[i]-mu0)+s[i-1]
}
s

mydata <- cbind(mydata,s)
mydata
