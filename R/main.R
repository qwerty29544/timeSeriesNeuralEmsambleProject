
# setup -------------------------------------------------------------------
from <- Sys.Date() - 365
future_days <- 7
past_days <- 30

libraries = c("tidyverse",
              "openxlsx",
              "dplyr",
              "tidyr",
              "stringr",
              "ggplot2",
              "forecast",
              "openxlsx",
              "quantreg",
              "data.table",
              "stringr",
              "pracma",
              "elastic",
              "rjson",
              "jsonlite")
source(file = "R/functions.R", encoding = "UTF-8")

download_libs(libs = libraries)

symbols_array = list("Thai Airways International Public Company Limited" = "TAWNF",
                     "S&P 500" = "^GSPC",
                     "Dow Jones Industrial Average" = "^DJI",
                     "NASDAQ Composite" = "^IXIC",
                     "Russell 2000" = "^RUT",
                     "Crude Oil Jul 21" = "CL=F",
                     "Gold Jun 21" = "GC=F",
                     "Sundial Growers Inc")


series_NASDAQ <- download_series(symbol = symbols_array[[2]], from = from)

# @TODO: автодобавление в ES

scaled_set <- log_scaler(series_NASDAQ)

preds_matrix <- matrix(data = 0, nrow = 10, ncol = 14)
for (row in 1:nrow(preds_matrix)) {
  ts <- predict_autoregression(dataset = scaled_set$dataset,
                               autoreg_past = 50 + 4 * (row - 1))
  ts <- ts$y_pred
  preds_matrix[row, ] <- log_unscaler(ts = ts,
                                      min = scaled_set$min_Close,
                                      max = scaled_set$max_Close)
}
unscaled_preds <- colMeans(preds_matrix)


dataset <- predictions_to_dataset(series_NASDAQ, unscaled_preds)


