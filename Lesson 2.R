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
