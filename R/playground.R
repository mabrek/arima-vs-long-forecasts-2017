source('data.R')

shinyplot(nn5, 150)

## NN5.001 trend
## 110 year and outliers

## NN5.026 mean shift
## 89 broken year trend

## half-hourly

multiplot(nn5[, c(1, 26, 89, 110)], vline = last(nn5_train_range))

