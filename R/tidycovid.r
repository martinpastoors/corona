# devtools::install_github("joachim-gassen/tidycovid19")

library(tidyverse)
library(tidycovid19)
library(zoo)
library(patchwork)

df <- 
  tidycovid19::download_merged_data(cached = TRUE, silent = TRUE) %>% 
  mutate(
    new_cases_per_million = 100000*(confirmed - lag(confirmed))/population,
    new_cases_smoothed_per_million = zoo::rollmean(new_cases_per_million, 7, na.pad=TRUE, align="right")
  )
  
df %>%
  filter(iso3c == "NLD") %>%
  ggplot(aes(x = date)) +
  geom_bar(aes(y = new_cases_per_million), stat = "identity", fill = "lightblue") +
  geom_line(aes(y = new_cases_smoothed_per_million), color ="red") +
  theme_minimal()

