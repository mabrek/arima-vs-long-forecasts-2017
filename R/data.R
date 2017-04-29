source('functions.R')

data(NN5.A)
data(NN5.A.cont)

nn5.data <- rbind(NN5.A, NN5.A.cont)
nn5.index <- ymd("1996-03-18") + days(seq_len(nrow(nn5.data)))
nn5 <- xts(nn5.data, order.by = nn5.index)

nn5_last_train_date <- ymd("1996-03-18") + days(nrow(NN5.A) - 1)
nn5_train_range <- seq_len(nrow(NN5.A))
