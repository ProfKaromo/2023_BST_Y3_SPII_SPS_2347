a <- c(2,3,-1,4,-5,1,-2,3,-1,6,4,1,-2,-2,1,-1,5,1,3,2,3,-4,2,5,-7)
A <- matrix(a,nrow = 5, byrow = T)
b <- c(10,-5,3,8,2)

sol <- solve(A)%*%b
sol


#Liner Programming 
library(lpSolve)
obj.in <- c(5,8)
const.mat <- matrix(c(1,1,1,2),nrow = 2,byrow = T)
const.dir <- c(rep(">=",2))
const.rhs <- c(2,3)
so1 <- lp("min",obj.in,const.mat,const.dir,const.rhs)
so1
so1$solution

sol2 <- lp(direction = "min",
           objective.in = c(5,8),
           const.mat = matrix(c(1,1,1,2),nrow = 2,byrow = T),
           const.dir = c(rep(">=",2)),
           const.rhs = c(2,3)
           )
sol2

#Example 2
#
# Set up problem: maximize
#   x1 + 9 x2 +   x3 subject to
#   x1 + 2 x2 + 3 x3  <= 9
# 3 x1 + 2 x2 + 2 x3 <= 15
#
f.obj <- c(1, 9, 1)
f.con <- matrix (c(1, 2, 3, 3, 2, 2), nrow=2, byrow=TRUE)
f.dir <- c("<=", "<=")
f.rhs <- c(9, 15)
#
# Now run.
#
lp ("max", f.obj, f.con, f.dir, f.rhs)
## Not run: Success: the objective function is 40.5
lp ("max", f.obj, f.con, f.dir, f.rhs)$solution
## Not run: [1] 0.0 4.5 0.0
#
# The same problem using the dense constraint approach:
#
f.con.d <- matrix (c(rep (1:2,each=3), rep (1:3, 2), t(f.con)), ncol=3)
lp ("max", f.obj, , f.dir, f.rhs, dense.const=f.con.d)
## Not run: Success: the objective function is 40.5
#
# Get sensitivities
#
lp ("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$sens.coef.from
## Not run: [1] -1e+30  2e+00 -1e+30
lp ("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$sens.coef.to  
## Not run: [1] 4.50e+00 1.00e+30 1.35e+01
#
# Right now the dual values for the constraints and the variables are
# combined, constraints coming first. So in this example...
#
lp ("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$duals     
## Not run: [1]   4.5   0.0  -3.5   0.0 -10.5
#
# ...the duals of the constraints are 4.5 and 0, and of the variables,
# -3.5, 0.0, -10.5. Here are the lower and upper limits on these:
#
lp ("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$duals.from
## Not run: [1]  0e+00 -1e+30 -1e+30 -1e+30 -6e+00
lp ("max", f.obj, f.con, f.dir, f.rhs, compute.sens=TRUE)$duals.to  
## Not run: [1] 1.5e+01 1.0e+30 3.0e+00 1.0e+30 3.0e+00
#
# Run again, this time requiring that all three variables be integer
#
lp ("max", f.obj, f.con, f.dir, f.rhs, int.vec=1:3)
## Not run: Success: the objective function is 37
lp ("max", f.obj, f.con, f.dir, f.rhs, int.vec=1:3)$solution
## Not run: [1] 1 4 0
#
# You can get sensitivities in the integer case, but they're harder to
# interpret.
#
lp ("max", f.obj, f.con, f.dir, f.rhs, int.vec=1:3, compute.sens=TRUE)$duals
## Not run: [1] 1 0 0 7 0
#
# Here's an example in which we want more than one solution to a problem
# in which all variables are binary: the 8-queens problem, 
# with dense constraints.
#
chess.obj <- rep (1, 64)
q8 <- make.q8 ()
chess.dir <- rep (c("=", "<"), c(16, 26))
chess.rhs <- rep (1, 42)
lp ('max', chess.obj, , chess.dir, chess.rhs, dense.const = q8, 
    all.bin=TRUE, num.bin.solns=3)