source(file = "R/download_libs.R", encoding = "UTF-8")
libraries = c("tidyverse",
              "openxlsx",
              "dplyr",
              "tidyr",
              "stringr",
              "ggplot2",
              "forecast",
              "openxlsx",
              "quantreg",
              "data.table")

download_libs(libs = libraries)
