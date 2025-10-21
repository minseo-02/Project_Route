install.packages("ggplot2")

library(ggplot2)

?ggplot2

df <- read.csv(file.path("data", "example_data.csv"))

df_plot <- ggplot(df, aes(x, y)) +
  geom_point() + geom_smooth(method='loess')

print(df_plot)

ggsave(filename = file.path("plots", "Problem_1_c.png"))
