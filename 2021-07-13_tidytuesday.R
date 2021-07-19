library(tidytuesdayR)
library(ggplot2)
library(scales)
library(ggthemes)
library(dplyr)

scooby_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/Scooby-Doo%20Completed.csv')
scooby <- scooby_raw %>%
  select(index, network, season) %>%
  filter(season %in% c("1", "2", "3", "4")) %>%
  group_by (season, network) %>%
  summarise(count_episodes = n_distinct(index))

ggplot(scooby, aes(x = season, y = count_episodes)) +
  geom_bar(stat = "identity") +
  facet_wrap(network ~ .) +
  labs(x = "Season", 
       y = "Number of Episodes",
       title =  "Number of episodes of each season televised on each network") +
  theme_solarized()



  