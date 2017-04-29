source('functions.R')

data(NN5.A)
data(NN5.A.cont)

nn5.data <- rbind(NN5.A, NN5.A.cont)
nn5.index <- ymd("1996-03-18") + days(seq_len(nrow(nn5.data)))
nn5 <- xts(nn5.data, order.by = nn5.index)

shinyplot(nn5, 150)

## NN5.026 mean shift
## NN5.001 trend
## 110 year and outliers
