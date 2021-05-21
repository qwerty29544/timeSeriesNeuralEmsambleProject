# Libraries download script v1.01 -----------------------------------------

#' Downloading and installing packages in R session
#'
#' @param libs - vector of character libraries that you want to download
#'
#' @return NULL
#' @export
#'
#' @examples
#' install_new_package("ggplot2")
#' install_new_package(c("dplyr", "forecast"))
download_libs <- function(libs = libraries) {
  for (libI in libs) {
    if (libI %in% installed.packages() == FALSE) {
      install.packages(libI)
      library(libI)
    }
  }
}

# -------------------------------------------------------------------------

