# devtools::install_github("joachim-gassen/tidycovid19")

library(tidyverse)
library(tidycovid19)
library(zoo)
library(patchwork)

df <- tidycovid19::download_merged_data(cached = TRUE, silent = TRUE) 
  
t <-
  df %>% 
  group_by(iso3c) %>% 
  mutate(new_cases            = confirmed - lag(confirmed)) %>% 
  mutate(new_cases_per_million = 1000000*(confirmed - lag(confirmed))/population) %>% 
  mutate(new_cases_per_million = ifelse(new_cases_per_million<0, 0, new_cases_per_million)) %>% 
  
  mutate(new_cases_smoothed             = zoo::rollmean(new_cases, 7, na.pad=TRUE, align="right")) %>% 
  mutate(new_cases_smoothed_per_million = zoo::rollmean(new_cases_per_million, 7, na.pad=TRUE, align="right")) %>% 
  filter(iso3c %in% c("NLD","IRL","DEU","FRA","GBR","AUT","DNK")) %>%
  filter(date >= lubridate::ymd("2021-01-01")) 

# per million  
t %>%
  ggplot(aes(x = date)) +
  theme_bw() +
  geom_bar(aes(y = new_cases_per_million), stat = "identity", fill = "lightblue") +
  geom_line(aes(y = new_cases_smoothed_per_million), color ="red") +
  ggrepel::geom_label_repel(data=t %>% group_by(iso3c) %>% filter(date==max(date, na.rm=TRUE)),
                            aes(y=new_cases_smoothed_per_million, label=round(new_cases_smoothed_per_million)),
                            nudge_y = 500,
                            segment.color = "red",
                            segment.size = 0.2,
                            segment.linetype = "dashed",
                            colour="red") +
  facet_wrap(~iso3c, nrow=1)

# absolute
# t %>%
#   filter(new_cases_smoothed >= 0) %>% 
#   ggplot(aes(x = date)) +
#   theme_bw() +
#   geom_bar(aes(y = new_cases), stat = "identity", fill = "lightblue") +
#   geom_line(aes(y = new_cases_smoothed), color ="red") +
#   ggrepel::geom_label_repel(data=t %>% group_by(iso3c) %>% filter(date==max(date, na.rm=TRUE)),
#                             aes(y=new_cases_smoothed, label=round(new_cases_smoothed)),
#                             nudge_y = 500,
#                             segment.color = "red",
#                             segment.size = 0.2,
#                             segment.linetype = "dashed",
#                             colour="red") +
#   facet_wrap(~iso3c, nrow=1)

# df %>% group_by(iso3c) %>% filter(iso3c=="NLD", date == max(date, na.rm=TRUE)) %>% dplyr::select(iso3c, date)
