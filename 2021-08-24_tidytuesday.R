library(tidytuesdayR)
library(ggplot2)
library(stringr)
library(ggthemes)
library(plyr)
library(dplyr)



lemurs_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')

lemurs <- lemurs_raw %>%
  select(taxon, sex, weight_g) %>%
  mutate(weight_g = weight_g/1000 ) %>% 
  filter(sex %in% c("F", "M"))


ggplot(lemurs, aes(x = weight_g, fill = sex)) +
  geom_histogram(aes(y=..density..), alpha=0.5, position="identity") +
  geom_density(alpha=.2) +
  facet_wrap(sex ~ .) +
  scale_x_continuous(breaks = c(0:10)) +
  labs(x = "Masa (Kg)", 
       y = "Densidad",
       fill = "Género",
       title =  "Curva de densidad de la masa los Lémures por género",
       caption = "Fuente: Duke Lemur Center")
  theme_wsj()

ggsave(
    filename = "2021-08-24_tidytuesday.png",
    device = "png",
    width = 13,
    height = 9)
  



