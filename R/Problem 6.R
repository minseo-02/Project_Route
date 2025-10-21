# (a)

gaussian_kernel <- function(x, x_prime, rho = 1)
{
  diff <- x - x_prime
  kernel_val <- exp(-rho * sum(diff^2))
  return(kernel_val)
}

gaussian_kernel_matrix <- function(X, Y = NULL, rho = 1)
{
  X <- as.matrix(X)
  
  if (is.null(Y))
    Y <- X
  else Y <- as.matrix(Y)
  
  
  n <- nrow(X); m <- nrow(Y)
  K <- matrix(0, n, m)
  
  for (i in 1:n)
  {
    for (j in 1:m)
    {
      diff <- X[i,] - Y[j,]
      K[i,j] <- exp(-rho * sum(diff^2))
    }
  }
  
  return(K)
}

# (b)

KRR <- function(X, y, lambda = 1e-4, rho = 1)
{
  X <- as.matrix(X)
  y <- as.numeric(y)
  n <- nrow(X)
  
  K <- gaussian_kernel_matrix(X, rho = rho)
  K_reg <- K + lambda + diag(n)
  
  alpha <- as.numeric(solve(K_reg, y))
  
  fitted <- as.numeric(K %*% alpha)
  
  result <- list(X = X,
                 y = y,
                 alpha = alpha,
                 lambda = lambda,
                 rho = rho,
                 fitted = fitted)
  
  class(result) <- "krr"
  
  return(result)
  
}

# (c)

predict.krr <- function(object, newdata = NULL, ...)
{
  if (is.null(newdata)) {
    return(object$fitted)
  }
  
  newX <- as.matrix(newdata)
  
  K_new <- gaussian_kernel_matrix(newX, object$X, rho = object$rho)
  
  as.numeric(K_new %*% object$alpha)
}

# (d)

plot_krr <- function(x, xlim = NULL, ngrid = 400,
                                 col_points = "gray40", pch = 16, cex = 0.9,
                                 col_line = "steelblue", lty = 1, lwd = 2, ...)
{
  X <- x$X
  
  xr <- if (is.null(xlim)) range(X[,1]) else xlim
  grid <- seq(xr[1], xr[2], length.out = ngrid)
  yhat <- predict(x, newdata = matrix(grid, ncol = 1))
  
  plot(X[,1], x$y, col = col_points, pch = pch, cex = cex,
       xlab = "x", ylab = "y / f̂(x)", ...)
  
  matlines(x = grid, y = cbind(yhat),
           lty = lty, lwd = lwd, col = col_line)
}


# (e)

set.seed(1)
n = 150
X = matrix(runif(n,-1, 1), ncol = 1)
ftrue = function(x) sin(2*pi*x) + 0.5*cos(4*pi*x)
y = ftrue(X[,1]) + rnorm(n, sd = 0.1)

fit <- KRR(X = X, y = y, lambda = 1e-4, rho = 1)

x_grid <- matrix(seq(-1, 1, length.out = 400), ncol = 1)
y_hat  <- predict(fit, newdata = x_grid)
y_true <- ftrue(x_grid[,1])

plot_krr(fit, xlim = c(-1, 1), ngrid = 400,
        col_points = "gray55", pch = 16, cex = 0.9,
        col_line = "steelblue", lty = 1, lwd = 2,
        main = "KRR fit (Gaussian)")
