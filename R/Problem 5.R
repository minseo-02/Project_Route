# (a)

A = matrix(c(4,2,2,2,5,1,2,1,3), 3)
A

U <- chol(A)
L <- t(U)

L %*% t(L)

all.equal(A, L %*% t(L))

# (b)

forward <- function(L, b)
{
  n <- length(b)
  z <- numeric(n)
  
  for (i in 1:n) {
    if (i == 1) {
      s <- 0
    } else {
      s <- sum(L[i, 1:(i-1)] * z[1:(i-1)])
    }
    z[i] <- (b[i] - s) / L[i, i]
  }
  return(z)
}

?forwardsolve

b <- c(1, 2, 3)

z <- forward(L, b)
z

forwardsolve(L, b)

all.equal(z, forwardsolve(L, b))

# (c)
## t(L) = U

backward <- function(U, z)
{
  n <- length(z)
  x <- numeric(n)
  
  for (i in n:1) {
    if (i == n) {
      s <- 0
    } else {
      s <- sum(U[i, (i+1):n] * x[(i+1):n])
    }
    x[i] <- (z[i] - s) / U[i, i]
  }
  return(x)
}

backward_result_x <- backward(t(L), z)
backward_result_x

backsolve(t(L), z)

all.equal(backward_result_x, backsolve(t(L), z))

# (d)

b_pro5_d <- c(1, -2, 3)

z_d <- forward(L, b_pro5_d)
x_d <- backward(t(L), z_d)
x_d

x_solve <- solve(A, b_pro5_d)
x_solve

all.equal(x_d, x_solve)






