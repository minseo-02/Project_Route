# 3-(a)

numerical_derivative <- function(f, x, h = 1e-6, method = c("forward", "backward", "central"))
  {
  method <- match.arg(method)
  
  if (method == "forward") {
    (f(x+h) - f(x)) / h
  } else if (method == "backward") {
    (f(x) - f(x-h)) / h
  } else {
    (f(x+h) - f(x-h)) / (2*h)
  }
}

f <- function(x) cos(x) - x
f_prime <- function(x) -sin(x) - 1

x_problem3 <- seq(0, 2*pi, length.out = 100)

f_num <- numerical_derivative(f, x_problem3, h = 1e-6, method = "central")
f_prime_num <- f_prime(x_problem3)

problem3_df <- data.frame(x = x_problem3,
                          analysis = f_prime_num,
                          num = f_num)

library(ggplot2)

problem3_a_plot <- ggplot(problem3_df, aes(x)) +
  geom_line(aes(y = analysis), linewidth = 1, color = "blue") +
  geom_line(aes(y = num), linetype = 3, linewidth = 2, color = "orange") +
  labs(y = "f'(x)")


print(problem3_a_plot)


# 3-(b)

newton_raphson <- function(f, fprime = NULL, x0, maxiter = 100,
                           h = 1e-6, epsilon = 1e-10)
{
  x <- x0
  
  for (t in 1:maxiter) {
    g <- if(is.null(fprime)) {
      (f(x+h) - f(x-h)) / (2*h)
    } else {
      fprime(x)
    }
    
    x_new <- x - f(x) / g
    
    if (abs(x_new - x) < epsilon) {
      return(list(root = x_new, iter = t, converged = TRUE))
    }
    
    x <- x_new
    
  }
  
  return(list(root = x, iter = maxiter, converged = FALSE))

}

# (c)

f <- function(x) cos(x) - x
f_prime <- function(x) -sin(x) - 1
x0 = 0.5

cos_analysis <- newton_raphson(f, fprime = f_prime, x0 = x0,
                               maxiter = 100, h = 1e-6, epsilon = 1e-10)

cos_num <- newton_raphson(f, fprime = NULL, x0 = x0,
                               maxiter = 100, h = 1e-6, epsilon = 1e-10)

print(cos_analysis)
print(cos_num)

# 두 개의 결과가 동일하게 나옴




