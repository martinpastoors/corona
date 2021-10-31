library(tidyverse)
# library(zoo)
library(readxl)
library(patchwork)

# https://github.com/OxCGRT/covid-policy-tracker

download.file("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_withnotes_2020.csv", 
              "C:/TEMP/OxCGRT_withnotes_2020.csv", mode = "w", cacheOK = TRUE)
download.file("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_withnotes_2021.csv", 
              "C:/TEMP/OxCGRT_withnotes_2021.csv", mode = "w", cacheOK = TRUE)

policy_track <-
  read.csv("C:/TEMP/OxCGRT_withnotes_2020.csv") %>% 
  bind_rows( read.csv("C:/TEMP/OxCGRT_withnotes_2021.csv"))
