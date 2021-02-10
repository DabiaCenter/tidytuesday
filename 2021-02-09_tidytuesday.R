library(tidytuesdayR)
library(ggplot2)
library(scales)
library(ggthemes)
library(dplyr)


student_debt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/student_debt.csv')

student_debt <- student_debt %>%
  mutate(race = if_else(race == "Black", "Afro-descendants", race))

student_debt %>%
  ggplot(aes(x = year, y = loan_debt, color = race)) +
  geom_line() +
  scale_y_continuous(labels = dollar) +
  expand_limits(y = 0) +
  labs(title = "Average family student loan debt for aged 25-55",
       subtitle = "By race and year normalized to 2016 dollars.",
       x = "Year",
       y = "Loan debt",
       color = "Race") +
  ggthemes::theme_clean()
  