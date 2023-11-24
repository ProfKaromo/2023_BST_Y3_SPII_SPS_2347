#Linear Regression
#yi <- b0+bixi+e
View(women)
attach(women)

model1 <- lm(weight~height)
summary(model1)

#height = -87.51667 + 3.45*weight + e# fitted model.

plot(height, weight)
abline(model1)

model1$coefficients[1]
model1$coefficients[2]

coef(model1)[1]
coef(model1)[2]

model1$coeff[1]
model1$coeff[1]

model1$fitted.values
e <- weight-model1$fitted.values
e
model1$residuals

confint(model1)

View(mtcars)
attach(mtcars)

model2 <- lm(disp~.,data = mtcars)
summary(model2)

confint(model2)

#Using matrix method
x <- as.matrix(cbind(rep(1,length(women$height)),women$height))
y <- as.matrix(women$weight)

xtx <- t(x)%*%x
xty <- t(x)%*%y

xtx.in <-solve(xtx)

betas <- xtx.in%*%xty
betas

cor(weight,height)
