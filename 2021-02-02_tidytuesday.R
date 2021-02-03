
# dependencias ------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(janitor)
library(scales)


# Cargando los datos ------------------------------------------------------

hbcu_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-02/hbcu_all.csv') %>% clean_names()


# Preparando los datos ----------------------------------------------------


hbcu_all_tidy <- hbcu_all %>%
  select(year, males, females) %>%
  pivot_longer(!year,
               names_to = "gender",
               values_to = "cantidad") %>%
  group_by(year, gender) %>%
  summarise(n = sum(cantidad)) %>%
  mutate(percentage = n / sum(n))



# Graficando los datos ----------------------------------------------------

hbcu_all_tidy %>%
  ggplot(aes(x=year, y=percentage * 100, fill=gender)) +
  geom_area(alpha=0.6 , size=1, colour="black") +
  scale_fill_discrete(labels = c("Femenino", "Masculino")) +
  labs(y = "Porcentaje %",
       x = "Año",
       fill = "Género",
       title = "High school completion and bachelor's degree attainment ",
       subtitle = "among persons age 25 and over by gender 1910-2016 in USA") +
  ggthemes::theme_economist()
