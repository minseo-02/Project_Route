set.seed(1)
n = 200
x = seq(0, 1, length.out = n)
y = sin(2*pi*x) + rnorm(n, sd = 0.15)

example_data <- data.frame(x = x, y = y)

write.csv(example_data, file = file.path("data", "example_data.csv"), row.names = FALSE)