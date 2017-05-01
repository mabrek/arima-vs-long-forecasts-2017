source('data.R')
source('theme.R')

shinyplot(nn5, 150)

## NN5.001 trend
## 110 year and outliers

## NN5.026 mean shift
## 89 broken year trend

## half-hourly

multiplot(nn5[, c(1, 26, 89, 110)], vline = last(nn5_train_range))


pattern <- c(5, 4.7, 4, 4.5, 6, 0, 0)
long <- rep(pattern, 100)
long <- long + rnorm(length(long), 0, 0.5)
long[seq_along(long) %% 7 == 6] <- 0
long[seq_along(long) %% 7 == 0] <- 0

clean <- rep(pattern, 4)
short <- long[1:28]
x <- 1:28
ggplot() + geom_path(aes(x = x, y = clean, color = "red")) +
  geom_path(aes(x = x, y = short, color = "black")) +
  scale_colour_identity() + xlab(NULL) + ylab(NULL) + tm

auto.fit <- auto.arima(long, max.p = 28, max.q = 5, max.order = 40, start.p = 1, start.q = 1)

x <- 1:70

ggplot() + geom_path(aes(x = x, y = rep(pattern, 10), color = "green")) +
  geom_path(aes(x = x, y = forecast(auto.fit, h = 70)$mean, color = "blue")) +
  scale_colour_identity() + ggtitle(as.character(auto.fit)) + xlab(NULL) + ylab(NULL) + tm

manual.fit <- Arima(long, c(7, 0, 0))

ggplot() + geom_path(aes(x = x, y = rep(pattern, 10), color = "green")) +
  geom_path(aes(x = x, y = forecast(manual.fit, h = 70)$mean, color = "blue")) +
  scale_colour_identity() + ggtitle(as.character(manual.fit)) + xlab(NULL) + ylab(NULL) + tm
