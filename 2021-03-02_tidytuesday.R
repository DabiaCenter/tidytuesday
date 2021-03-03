library(tidyverse)
library(tidytuesdayR)
library(scales)
library(ggthemes)
library(RColorBrewer)

# install.packages("tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load('2021-03-02')

youtube <- tuesdata[[1]] %>%
  mutate(brand = recode(brand, "Hynudai" = "Hyundai"))

youtube_by_brand <- youtube %>%
  select(-favorite_count, -kind, -etag) %>%
  group_by(brand) %>%
  summarize(Funny = mean(funny),
            Quick_Product = mean(show_product_quickly),
            Patriotic = mean(patriotic),
            Celebrity = mean(celebrity),
            Danger = mean(danger),
            Animals = mean(animals),
            Sex = mean(use_sex),
            Count = n()) %>%
  slice_max(n = 6, order_by = Count, with_ties = FALSE)

youtube_by_brand %>% 
  pivot_longer(2:8, names_to = 'Property') %>%
  mutate(Property = fct_reorder(Property, desc(value))) %>%
  ggplot(aes(Property, value, fill = Property)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  coord_polar() +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = 'Set1') + 
  facet_wrap(~brand) +
  theme_pander() + 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 9),
        legend.position = 'right',
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 11)
  ) +
  labs(x = NULL,
       y = NULL, 
       fill = 'Property',
       title = 'Suberbowl Ads from 2000-2020',
       subtitle = 'How often do different ad properties get used?')