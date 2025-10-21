# (a)

left_rectangle <- function(f, a, b, n)
{
  h <- (b-a) / n
  x_i <- a + h * (0:(n-1))
  area <- h * sum(f(x_i))
  return(area)
}


# (b)

trapezoid <- function(f, a, b, n)
{
  h <- (b-a) / n
  x_i <- a + h * (0:n)
  f_i <- f(x_i)
  area <- h * (0.5 * f_i[1] + sum(f_i[2:n]) + 0.5* f_i[n+1])
  return(area)
}

# (c)

simpson <- function(f, a, b, n)
{
  if (n %% 2 == 1) {
    n <- n+1
  }
  
  h <- (b-a) / n
  x_i <- a + h * (0:n)
  f_i <- f(x_i)
  
  odd_idx <- seq(2, n, by=2)
  even_idx <- seq(3, n-1, by=2)
  area <- (h/3) * (f_i[1] + 4*sum(f_i[odd_idx]) + 2*sum(f_i[even_idx]) + f_i[n+1])
  return(area)
}

# (d)

f_problem4 <- function(x) sin(x)
a <- 0; b <- pi; n <- 100

left_val <- left_rectangle(f = f_problem4, a, b, n)
trap_val <- trapezoid(f = f_problem4, a, b, n)
simp_val <- simpson(f = f_problem4, a, b, n)

left_val
trap_val
simp_val

# (e)

num_problem4 <- c(10, 30, 60, 100, 150, 200)

true_val <- -cos(b) + cos(a)
true_val

left <- numeric(length(num_problem4))
trap <- numeric(length(num_problem4))
simp <- numeric(length(num_problem4))

for (i in 1:length(num_problem4)) {
  nn <- num_problem4[i]
  left[i] <- left_rectangle(f_problem4, a, b, nn)
  trap[i] <- trapezoid(f_problem4, a, b, nn)
  simp[i] <- simpson(f_problem4, a, b, nn)
}

err_left <- abs(left - true_val)
err_trap <- abs(trap - true_val)
err_simp <- abs(simp - true_val)

df_problem4 <- data.frame(n = num_problem4,
                          Left = left, Trap = trap, Simp = simp,
                          Left_err = err_left, Trap_err = err_trap, Simp_err = err_simp)

df_problem4

install.packages("patchwork")
library(patchwork)

left_plot <- ggplot(df_problem4, aes(x = n, y = err_left)) +
  geom_point(color = "red", size = 3) +
  geom_line(color = "blue", linewidth = 1)

trap_plot <- ggplot(df_problem4, aes(x = n, y = err_trap)) +
  geom_point(color = "red", size = 3) +
  geom_line(color = "blue", linewidth = 1)

simp_plot <- ggplot(df_problem4, aes(x = n, y = err_simp)) +
  geom_point(color = "red", size = 3) +
  geom_line(color = "blue", linewidth = 1)

left_plot + trap_plot + simp_plot









