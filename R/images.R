source('data.R')
source('theme.R')

## TODO drop y text
image_device("daily1")
m <- nn5[, c(1, 110)]
r <- nrow(m)
labels <- make.unique(colnames(m))
df <- data.frame(
  index(m)[rep.int(1:r, ncol(m))],
  factor(rep(1:ncol(m), each = r), levels = 1:ncol(m), labels = labels),
  as.vector(coredata(m)))
names(df) <- c("Index", "Series", "Value")
df[, "Color"] <- "black"
df[df$Index > nn5_last_train_date, "Color"] <- "blue"
p <- ggplot(data = df) + geom_path(aes(x = Index, y = Value, color = Color), na.rm = TRUE) + scale_colour_identity() +
  xlab(NULL) + ylab(NULL) + facet_grid(Series ~ ., scales = "free_y") + tm +
  theme(strip.text.y = element_blank())
print(p)
dev.off()

image_device("daily2")
m <- nn5[, c(26, 89)]
r <- nrow(m)
labels <- make.unique(colnames(m))
df <- data.frame(
  index(m)[rep.int(1:r, ncol(m))],
  factor(rep(1:ncol(m), each = r), levels = 1:ncol(m), labels = labels),
  as.vector(coredata(m)))
names(df) <- c("Index", "Series", "Value")
df[, "Color"] <- "black"
df[df$Index > nn5_last_train_date, "Color"] <- "blue"
p <- ggplot(data = df) + geom_path(aes(x = Index, y = Value, color = Color), na.rm = TRUE) + scale_colour_identity() +
  xlab(NULL) + ylab(NULL) + facet_grid(Series ~ ., scales = "free_y") + tm +
  theme(strip.text.y = element_blank())
print(p)
dev.off()

image_device("half-hourly")
data <- data.frame(x = 1:384, y = taylor[1:384])
data["color"] <- "black"
data[336:384, "color"] <- "blue"
p <- ggplot(data) + geom_path(aes(x = x, y = y, color = color)) + scale_colour_identity() + xlab(NULL) + ylab(NULL) + tm + theme_no_labels
print(p)
dev.off()

image_device("dirac-delta")
delta <- data.frame(x = (-500):500, y = 0)
delta[501, "y"] <- 1
p <- ggplot(delta) + geom_path(aes(x = x, y = y)) + xlab(NULL) + ylab(NULL) + tm
print(p)
dev.off()

delta <- rep.int(0, 200)
delta[1] <- 1

image_device("ar_1")
start.innov <- c(0)
sim <- as.numeric(arima.sim(list(ar = (0.9)), length(delta), innov = delta, n.start = length(start.innov), start.innov = start.innov))[1:100]
p <- ggplot() + geom_path(aes(x = seq_along(sim) - 1, y = sim)) + xlab(NULL) + ylab(NULL) + tm
print(p)
dev.off()

image_device("ar_2_1")
start.innov <- c(0, 0)
sim <- as.numeric(arima.sim(list(ar = c(0.3, 0.3)), length(delta), innov = delta, n.start = length(start.innov), start.innov = start.innov))[1:50]
p <- ggplot() + geom_path(aes(x = seq_along(sim) - 1, y = sim)) + xlab(NULL) + ylab(NULL) + tm
print(p)
dev.off()


image_device("ar_2_2")
start.innov <- c(0, 0)
sim <- as.numeric(arima.sim(list(ar = c(0.9, -0.85)), length(delta), innov = delta, n.start = length(start.innov), start.innov = start.innov))[1:100]
ggplot() + geom_path(aes(x = seq_along(sim) - 1, y = sim)) + xlab(NULL) + ylab(NULL) + tm
p <- ggplot() + geom_path(aes(x = seq_along(sim) - 1, y = sim)) + xlab(NULL) + ylab(NULL) + tm
print(p)
dev.off()

image_device("ar_2_3")
start.innov <- c(0, 0)
sim <- as.numeric(arima.sim(list(ar = c(0.08, 0.9)), length(delta), innov = delta, n.start = length(start.innov), start.innov = start.innov))
p <- ggplot() + geom_path(aes(x = seq_along(sim) - 1, y = sim)) + xlab(NULL) + ylab(NULL) + tm
print(p)
dev.off()


image_device("ma_5")
start.innov <- rep.int(0, 5)
sim <- as.numeric(arima.sim(list(ma = c(0.8, 0.6, 0.4, 0.2, 0.1)), length(delta), innov = delta, n.start = length(start.innov), start.innov = start.innov))[1:15]
p <- ggplot() + geom_path(aes(x = seq_along(sim) - 1, y = sim)) + xlab(NULL) + ylab(NULL) + tm
print(p)
dev.off()


noise <- rnorm(100)
image_device("i_0")
p <- ggplot() + geom_path(aes(x = seq_along(noise), y = noise)) + xlab(NULL) + ylab(NULL) + tm
print(p)
dev.off()

noise <- cumsum(noise)
image_device("i_1")
p <- ggplot() + geom_path(aes(x = seq_along(noise), y = noise)) + xlab(NULL) + ylab(NULL) + tm
print(p)
dev.off()


noise <- cumsum(noise)
image_device("i_2")
p <- ggplot() + geom_path(aes(x = seq_along(noise), y = noise)) + xlab(NULL) + ylab(NULL) + tm
print(p)
dev.off()
