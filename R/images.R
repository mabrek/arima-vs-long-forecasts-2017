source('data.R')
source('theme.R')

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

