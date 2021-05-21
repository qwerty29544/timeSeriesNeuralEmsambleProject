#' Download Yahoo time series by symbols
#'
#' @param symbol
#' @param from
#' @param to
#'
#' @return
#' @export quantmod
#' @export stringr
#' @export dplyr
#' @export data.table
#' @export zoo
#'
#' @examples
download_series <- function(symbol, from = Sys.Date() - 90, to = Sys.Date()) {
  # Загрузка данных из Yahoo по symbol
  quantmod::getSymbols(Symbols = symbol, src = "yahoo", from = from, to = to)

  # Переопределение строки для того, чтобы взять переменную датасета по её имени
  symbol <- stringr::str_remove(symbol, "[:punct:\\^]")

  ts_df <- get(symbol)
  date_ts <- as.Date(zoo::index(ts_df), format = "%Y-%d-%m")

  years <- as.numeric(format(date_ts,"%Y"))
  months <- as.numeric(format(date_ts, "%m"))
  days <- as.numeric(format(date_ts, "%d"))
  quarter <- as.numeric(gsub(pattern = "Q", replacement = "", x = quarters(date_ts)))
  weekdays_numeric_vector <- 1:5
  names(weekdays_numeric_vector) <- unique(weekdays(date_ts))

  ts_df <- data.table::data.table(Date = date_ts,
                                  Year = years,
                                  Month = months,
                                  Day = days,
                                  Quarter = quarters,
                                  Weekday = weekdays_numeric_vector,
                                  Date_days = as.numeric(date_ts),
                                  data.frame(ts_df))

  return(ts_df)
}
