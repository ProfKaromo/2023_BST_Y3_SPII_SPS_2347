#QCM/SQCM
sample <- c(1:20)
x1 <- c(33,33,35,30,33,38,30,29,28,38,28,31,27,33,35,33,35,32,25,35)
x2 <- c(29,31,37,31,34,37,31,39,33,33,30,35,32,33,37,33,34,33,27,35)
x3 <- c(31,35,33,33,35,39,32,38,35,32,28,35,34,35,32,27,34,30,34,36)
x4 <- c(32,37,34,34,33,40,34,39,36,35,32,35,35,37,35,31,30,30,27,33)
x5 <- c(33,31,36,33,34,38,31,39,43,32,31,34,37,36,39,30,32,33,28,30)
mydata <- cbind(sample,x1,x2,x3,x4,x5)
mydata <- as.matrix(mydata)
mydata

mydata1 <- mydata[,-1]
mydata1

xbari <- NULL
rangei <- NULL
sdis <- NULL

for (i in sample) {
  xbi <- mean(mydata1[i,])
  ri <- max(mydata1[i,])-min(mydata1[i,])
  sdi <- round(sd(mydata1[i,]),4)#sqrt(var(mydata1[i,]))
  
  xbari <- rbind(xbari,xbi)
  rangei <- rbind(rangei,ri)
  sdis <- rbind(sdis,sdi)
}

final_data <- cbind(mydata,xbari,rangei,sdis)
colnames(final_data) <- c("Sample_No.","x1","x2","x3","x4","x5","xbar","r","s")

final_data <- as.data.frame(final_data)
mu <- mean(final_data$xbar)
rbar <-mean(final_data$r) 
sbar <- mean(final_data$s)

k <-c("-","-","-","-","-","-",mu,rbar,sbar)
final_data <- rbind(final_data,k)
View(final_data)

library(fpp3)
final_data <- final_data |>
  as_tsibble(key=c(x1,x2,x3,x4,x5), index=Sample_No.)

plot(NULL,xlim=c(1,20),ylim=c(25,41),xlab="Samples",ylab ="Xbaris",main="X Bar Chart")
UCL <- mu+3*sbar
LCL <- mu-3*sbar
abline(h=mu)
abline(h=UCL)
abline(h=LCL)
points(xbari)
lines(xy.coords(sample,xbari),type="l")

#OC for single sampling plan
n <- 50
c <- 1
p <- seq(0.00,0.1,0.01)
pa <- 0
for (d in 0:c) {
  pr <- (factorial(n))/((factorial(d))*factorial(n-d))*(p^d)*(1-p)^(n-d)
  pa<- pa + pr
}

PA <- NULL
p <- seq(0.00,0.1,0.01)
for (j in p) {
  pa <- 0
  for (d in 0:c) {
    pr <- (factorial(n))/((factorial(d))*factorial(n-d))*(j^d)*(1-j)^(n-d)
    pa<- pa + pr
  }
  PA <- c(PA,pa)
}

cat("The acceptance probability is",pa)

plot(p,PA)
lines(plot(p,PA))


pa_vals <- as.data.frame(cbind(p,PA))

View(pa_vals)

#Example
library(qcc)
##
##  Continuous data 
##
data(pistonrings)
attach(pistonrings)
diameter <- qcc.groups(diameter, sample)

qcc(diameter[1:25,], type="xbar")
qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,])
q <- qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,], plot=FALSE)
plot(q, chart.all=FALSE)
qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,], nsigmas=2)
qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,], confidence.level=0.99)

qcc(diameter[1:25,], type="R")
qcc(diameter[1:25,], type="R", newdata=diameter[26:40,])

qcc(diameter[1:25,], type="S")
qcc(diameter[1:25,], type="S", newdata=diameter[26:40,])

# add warning limits at 2 std. deviations
q <- qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,], plot=FALSE)
(warn.limits <- limits.xbar(q$center, q$std.dev, q$sizes, 2))
plot(q, restore.par = FALSE)
abline(h = warn.limits, lty = 3, col = "chocolate")

# variable control limits
out <- c(9, 10, 30, 35, 45, 64, 65, 74, 75, 85, 99, 100)
diameter <- qcc.groups(pistonrings$diameter[-out], sample[-out])
qcc(diameter[1:25,], type="xbar")
qcc(diameter[1:25,], type="R")
qcc(diameter[1:25,], type="S")
qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,])
qcc(diameter[1:25,], type="R", newdata=diameter[26:40,])
qcc(diameter[1:25,], type="S", newdata=diameter[26:40,])

detach(pistonrings)

##
##  Attribute data 
##

data(orangejuice)
attach(orangejuice)
qcc(D[trial], sizes=size[trial], type="p")

# remove out-of-control points (see help(orangejuice) for the reasons)
inc <- setdiff(which(trial), c(15,23))
q1 <- qcc(D[inc], sizes=size[inc], type="p")
qcc(D[inc], sizes=size[inc], type="p", newdata=D[!trial], newsizes=size[!trial]) 
detach(orangejuice)

data(orangejuice2)
attach(orangejuice2)
names(D) <- sample
qcc(D[trial], sizes=size[trial], type="p")
q2 <- qcc(D[trial], sizes=size[trial], type="p", newdata=D[!trial], newsizes=size[!trial])
detach(orangejuice2)

# put on the same graph the two orange juice samples
oldpar <- par(no.readonly = TRUE)
par(mfrow=c(1,2), mar=c(5,5,3,0))
plot(q1, title="First samples", ylim=c(0,0.5), add.stats=FALSE, restore.par=FALSE)
par("mar"=c(5,0,3,3), yaxt="n")
plot(q2, title="Second samples", add.stats=FALSE, ylim=c(0,0.5))
par(oldpar)

data(circuit)
attach(circuit)
qcc(x[trial], sizes=size[trial], type="c")
# remove out-of-control points (see help(circuit) for the reasons)
inc <- setdiff(which(trial), c(6,20))
qcc(x[inc], sizes=size[inc], type="c", labels=inc)
qcc(x[inc], sizes=size[inc], type="c", labels=inc, 
    newdata=x[!trial], newsizes=size[!trial], newlabels=which(!trial))
qcc(x[inc], sizes=size[inc], type="u", labels=inc, 
    newdata=x[!trial], newsizes=size[!trial], newlabels=which(!trial))
detach(circuit)

data(pcmanufact)
attach(pcmanufact)
qcc(x, sizes=size, type="u")
detach(pcmanufact)

data(dyedcloth)
attach(dyedcloth)
qcc(x, sizes=size, type="u")
# standardized control chart
q <- qcc(x, sizes=size, type="u", plot=FALSE)
z <- (q$statistics - q$center)/sqrt(q$center/q$size)
plot(z,  type="o", ylim=range(z,3,-3), pch=16)
abline(h=0, lty=2)
abline(h=c(-3,3), lty=2)
detach(dyedcloth)

##
##  Continuous one-at-time data 
##

# viscosity data (Montgomery, pag. 242)
x <- c(33.75, 33.05, 34, 33.81, 33.46, 34.02, 33.68, 33.27, 33.49, 33.20,
       33.62, 33.00, 33.54, 33.12, 33.84)
qcc(x, type="xbar.one")
qcc(x, type="xbar.one", std.dev = "SD")