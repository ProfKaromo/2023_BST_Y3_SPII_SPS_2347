#DEALING WITH MATRICES
# A matrix of order 5*5 whose elements are random normal 
# values with mean 6 and s.d of 2

# Solution
A <- matrix(round(rnorm(25,6,2),0),nrow = 5, byrow = T)
A

# Matrix Operations
X <- matrix(c(1:9), nrow = 3, byrow = T)
Y <- matrix(c(1:9), nrow = 3, byrow = F)
X;Y

X+Y
A+X#not posible

#Scalar with a matrix
3*X

Y-X

# Extracting of values

Y[2,3]
Y[3,]
Y[,2]

Z <- X%*%Y
Z

tX <- t(X)
tX
X

X[2,1] <- 5
  
solve(X)

#2X+Y-2XY
Y[2,1] <- 0
K <- 2*X+t(Y)-2*X%*%solve(Y)
K

x1 <- 2*X
x2 <- t(Y)
x3 <- 2*X%*%solve(Y)

K1<- x1+x2-x3
K1

X;Y
kronecker(X,Y)

#Identity matrix
diag(1,5)

X
diag(diag(X))

X;Y

X[,2]%*%Y
(as.matrix(X[,2]))%*%Y

X1 <- matrix(round(rnorm(36,3,1),0),nrow=6, byrow=T)
X1

X1_row_mean <- rowMeans(X1)
X1_row_mean
X1_col_mean <- colMeans(X1)
X1_col_mean

K1 <- as.data.frame(X1)
k2 <- sapply(K1,K1$V1, max)
k2

#logs
log10(5)
log(5,10)
log(5,base = 10)
log(5,base = exp(1))
log(8,16)

#Exponents
exp(5)
plot(exp(-(5:50)))

     