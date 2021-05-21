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

  ts_df <- data.table::data.table(Date = date_ts,
                                  Date_days = as.numeric(date_ts),
                                  data.frame(ts_df))

  return(ts_df)
}
