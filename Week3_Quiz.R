library(dplyr)

d = data(iris)

mean(iris$Sepal.Length)

iris %>%
  filter(Species == "virginica") %>%
  summarise(mn = round(mean(Sepal.Length)))

rowMeans(iris[, 1:4])

apply(iris, 2, mean)

colMeans(iris)

apply(iris[, 1:4], 2, mean)

data(mtcars)

View(mtcars)

round(abs((mtcars %>%
  filter(mtcars$cyl == 4) %>%
  summarise(avg_4 = mean(hp))) -
(mtcars %>%
  filter(mtcars$cyl == 8) %>%
  summarise(avg_4 = mean(hp)))))