library(tidyverse)
library(tidytuesdayR)
library(ggthemr)
library(firatheme)

tuesdata <- tidytuesdayR::tt_load(2021, week = 22)
data <- tuesdata$records %>% select(track, type, shortcut, system_played, time) %>%
  filter(type == "Single Lap", shortcut == "No") %>%
  group_by(track) %>%
  summarise(best_time = min(time)) %>%
  arrange(desc(best_time))

ggplot(data, aes(x =reorder(track, best_time), y = best_time)) +
  geom_col(position = position_dodge(), fill = "darkblue") +
  geom_text(aes(label = best_time), size = 3, hjust = 1.5, color = "#ffffff")+
  coord_flip() +
  theme_fira() +
  labs(
    title = "Tiempo record por pista de Mario Kart (Single Lap)"
  ) +
  ylab("Tiempo en segundos") +
  xlab("Pista")
