library(readr)
library(dplyr)
library(lubridate)
library(bsts)
library(Metrics)

# Feb -> 2, Apr -> 4
numerizer <- function(abb) {
  lut <- data.frame(
    abb = month.abb,
    num = seq(1, 12)
  )
  lut[lut$abb == abb, 2]
}
numerizer <- Vectorize(numerizer, 'abb')

mape <- function(act, pred) {
  t <- abs(act - pred) / act
  mape <- mean(t[!is.infinite(t)])
  mape
} 

# Takes complainttype input file, runs BSTS, and returns MAPE and RMSE
do_bsts <- function(f) {
  df <- read_csv(f) %>% 
    filter(Year != 2016) %>% 
    mutate(Month = numerizer(Month)) %>% 
    mutate(Date = ymd(paste(Year, Month, '01', sep = '-')))
  train <- df %>% filter(Year <= 2014)
  test <- df %>% filter(Year >= 2015)
  
  y <- train$Complaints
  y[y == 0] <- 1
  y <- log10(y)
  
  ss <- AddLocalLinearTrend(list(), y)
  ss <- AddSeasonal(ss, y, nseasons = 12)
  bsts.model <- bsts(y, state.specification = ss, niter = 1000, 
                     ping = 0, seed = 1337)
  
  burn <- SuggestBurn(0.1, bsts.model)
  p <- predict.bsts(bsts.model, horizon = nrow(test), burn = burn)
  
  d2 <- data.frame(
    c(10 ^ as.numeric(-colMeans(bsts.model$one.step.prediction.errors[-(1:burn),])
                      + y),
      10 ^ as.numeric(p$mean)),
    df$Complaints,
    df$Year)
  names(d2) <- c("Fitted", "Actual", 'Year')
  
  df_for_metrics <- d2 %>% filter(Year >= 2015)
  act <- df_for_metrics$Actual
  pred <- df_for_metrics$Fitted
  
  list(mape = mape(act, pred),
       rmse = rmsle(act, pred))
}

complaint_types <- list.files('data/topNComplaints') %>% gsub('.csv', '', .)
data_files <- list.files('data/topNComplaints', full.names = T)

results <- data.frame(
  complaint.type = character(),
  mape = numeric(),
  rmse = numeric()
)

for (ix in seq_along(complaint_types)) {
  bsts_results <- do_bsts(data_files[ix])
  result_row <- data.frame(
    complaint.type = complaint_types[ix],
    mape = round(bsts_results$mape *100, 2),
    rmse = round(bsts_results$rmse * 100, 2) 
  )
  results <- bind_rows(results, result_row)
}

write_csv(results, 'results_seed_1337.csv')

