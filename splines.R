# Ejemplo en Medium
library(tidyverse)
library(plotly)
library(splines)

quantity <- c(25, 39, 45, 57, 70, 85, 89, 100, 110, 124, 137, 150, 177)
sales <- c(1000, 1250, 2600, 3000, 3500, 4500, 5000, 4700, 4405, 4000, 3730, 3400, 3300)
data <- data_frame(quantity, sales)

plot_ly(
  data,
  x = ~quantity,
  y = ~sales,
  type = 'scatter'
        )

fit <- lm(sales ~ quantity, data = data)
summary(fit)

plot_ly(
  data,
  x = ~quantity,
  y = ~sales,
  type = 'scatter'
) %>% add_lines(x = ~quantity, y = fitted(fit))

fit2 <- lm(sales ~ poly(quantity, 2), data = data)
summary(fit2)

plot_ly(
  data,
  x = ~quantity,
  y = ~sales,
  type = 'scatter'
) %>% add_lines(x = ~quantity, y = fitted(fit2))

data <- data %>% mutate(
  xi_t = ifelse(quantity <= 89, 0, 1),
  quantity_trans = (quantity - 89) * xi_t
)

fit <- lm(sales ~ quantity + quantity_trans, data = data)
summary(fit)

plot_ly(
  data,
  x = ~quantity,
  y = ~sales,
  type = 'scatter'
) %>% add_lines(x = ~quantity, y = fitted(fit))

set.seed(123)
x = runif(500)
mu = sin(2*(4*x-2)) + 2*exp(-(16^2)*((x-.5)^2))
y = rnorm(500, mu, .3)
d = data.frame(x,y) 
p <- ggplot(d, aes(x , y)) + geom_point(size = 3, alpha = 0.7)

knots <- seq(0, 1, by = 0.1)
knots <- knots[-length(knots)]  # don't need the last value
l = 3
bs = as.data.frame(sapply(1:length(knots), function(k) ifelse(x >= knots[k], (x - knots[k]) ^ l, 0)))
lmMod = lm(y ~ .-1, data = bs)

d$fitted_l <- fitted(lmMod)
d$fitted_cubic <- fitted(lmMod)
ggplot(d, aes(x = x, y = y)) + geom_point(size = 3, alpha = 0.6) +
  geom_line(aes(x = x, y = fitted_l), col = 'orange', size = 1.5) +
  geom_line(aes(x = x, y = fitted_cubic), col = 'steelblue', size = 1.5)
