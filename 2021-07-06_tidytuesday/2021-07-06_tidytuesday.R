library(tidyverse)
library(lubridate)
library(ggflags)
library(ggthemes)
library(ggpubr)

days <- tidytuesdayR::tt_load(2021,week = 28)
days <- days[[1]]
days <- days %>%
  filter(grepl("^Independence", name_of_holiday)) %>% 
  mutate(year_of_event = ifelse(is.na(year_of_event),
                                year(date_parsed),
                                year_of_event),
         latest = rank(date_parsed)) %>% 
  arrange(latest)
days <- days[1:14,]
days <- days %>% 
  mutate(country_code = c("us",
                          "ht",
                          "ec",
                          "co",
                          "cl",
                          "pa",
                          "ve",
                          "ar",
                          "cr",
                          "sv",
                          "gt",
                          "hn",
                          "ni",
                          "br") )

ggplot(days, aes(x= year_of_event, y=reorder(country, latest), country=country_code)) + 
  geom_flag() + 
  scale_country() +
  scale_size(range = c(0, 15)) +
  theme_fivethirtyeight() +
  rremove("legend") +
  labs(title = "Los 10 primeros países en conseguir la independencia")