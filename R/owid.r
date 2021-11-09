# devtools::install_github("joachim-gassen/tidycovid19")

library(tidyverse)
library(zoo)
library(readxl)
library(patchwork)

download.file("https://covid.ourworldindata.org/data/owid-covid-data.csv", 
              "C:/TEMP/owid-covid-data.csv", mode = "w", cacheOK = TRUE)

owid <- 
  read.csv("C:/TEMP/owid-covid-data.csv") %>% 
  mutate(date = lubridate::ymd(date))

# OxCGRT <- 
#   read.csv("https://github.com/OxCGRT/covid-policy-tracker/blob/master/data/OxCGRT_withnotes_2021.csv")



# patchwork plot with cases, people vaccinated and stringency index
t <-
  owid %>%
  filter(iso_code %in% c("DEU","NLD", "BEL", "FRA", "ESP","DNK", "IRL", "GBR")) %>% 
  filter(date >= lubridate::ymd("2021-01-01")) %>% 
  dplyr::select(iso_code, date, 
                new_cases_smoothed_per_million, new_cases_per_million, stringency_index, people_fully_vaccinated_per_hundred) %>% 
  pivot_longer(names_to = "variable", values_to = "value", 
               stringency_index:people_fully_vaccinated_per_hundred) %>% 
  filter(value >= 0, (new_cases_per_million > 0 | new_cases_smoothed_per_million > 0)) 

t %>% group_by(iso_code) %>% filter(iso_code == "NLD", date == max(date)) %>% dplyr::select(iso_code, date) %>% View()

p1 <-
  t %>% 
  ggplot(aes(x = date)) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_blank()) +
  
  geom_point(aes(y =new_cases_per_million), colour="black", size=0.2) +

  geom_line(aes(y =new_cases_smoothed_per_million), colour="red") +
  ggrepel::geom_label_repel(data=t %>% group_by(iso_code) %>% filter(date==max(date, na.rm=TRUE)),
                            aes(y=new_cases_smoothed_per_million, label=round(new_cases_smoothed_per_million)),
                            nudge_y = 500,
                            segment.color = "red",
                            segment.size = 0.2,
                            segment.linetype = "dashed",
                            colour="red") +
  labs(y="", x="", title = "new_cases_smoothed_per_million") +
  
  expand_limits(y=0) +
  facet_wrap(~iso_code, nrow=1)

p2 <-
  t %>% 
  ggplot(aes(x = date)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(strip.background = element_blank(), strip.text = element_blank()) + 
  geom_line(aes(y =value, colour=variable)) +
  ggrepel::geom_label_repel(data=t %>% group_by(iso_code, variable) %>% filter(date==max(date, na.rm=TRUE)), 
                            aes(y=value, label=round(value), colour=variable)) +
  expand_limits(y=0) +
  labs(x="", y="index") +
  facet_wrap(~iso_code, nrow=1)

p1 / p2



# facet grid with cases, people vaccinated and stringency index
# owid %>%
#   filter(iso_code %in% c("DEU","NLD", "BEL", "FRA", "ESP","DNK")) %>% 
#   filter(date >= lubridate::ymd("2021-01-01")) %>% 
#   dplyr::select(iso_code, date, 
#                 new_cases_smoothed_per_million, stringency_index, people_fully_vaccinated_per_hundred) %>% 
#   pivot_longer(names_to = "variable", values_to = "value", 
#                new_cases_smoothed_per_million:people_fully_vaccinated_per_hundred) %>% 
#   filter(value >= 0) %>% 
#   
#   ggplot(aes(x = date)) +
#   theme_bw() +
#   theme(legend.position = "none") +
#   geom_line(aes(y =value, colour=variable)) +
#   # geom_point(aes(y=value, colour=variable), size=0.3) +
#   expand_limits(y=0) +
#   facet_grid(variable~iso_code, scales="free_y")
