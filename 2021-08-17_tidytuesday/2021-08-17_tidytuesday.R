library(readr)
library(tidyverse)
library(stopwords)
library(echarts4r)

# Gettting the data
computer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-17/computer.csv')

# Stopwords
stopwords <- data.frame(stopwords('english'))
colnames(stopwords) <- c('sw')

others <- tibble(sw = c('zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten', 'twenty', 'thirty', 'fourty', 'fifty', 'seconds', 'minutes', 'can'))

the_stopwords <- bind_rows(stopwords, 
                           others)

# Most frequent words: people vs computers
table(computer$char_type)

freq <- tibble(phrase = computer$interaction, 
               clasif = computer$char_type) %>%
  unnest_tokens(output = word, 
                input = phrase, 
                strip_numeric = TRUE) %>%
  anti_join(the_stopwords, by = c('word' = 'sw')) %>%
  group_by(clasif) %>%
  count(word, sort = TRUE) 

comp <- freq %>% 
  filter(clasif == "Computer") %>% 
  arrange(-n)

comp[1:100,] %>%
  e_color_range(n, color) %>% 
  e_charts() %>% 
  e_cloud(word, n, color, shape = "circle", sizeRange = c(10, 30)) %>%
  e_title("Top palabras mencionadas por una computadora en Star Trek",
          "Hecho por Grupo Dabia")