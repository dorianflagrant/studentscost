Unemployment <- read.csv("Unemployment.csv", sep = ",")

Unemployment2 <- Unemployment %>%  filter(MEASURE == "VALUE")

library(tidyverse)
Unemployment %>% 
  filter(MEASURE == "VALUE") %>% 
  View()

names(Unemployment)

Unemploymentsimple <- Unemployment[c(2,23)]

Unemployment2 %>% 
  filter(Value>0) %>%
    ggplot() +
    geom_bar(aes(x = reorder(Country, Value), y = Value, fill = Value), stat = "identity", position = position_stack(reverse = TRUE)) +  
    coord_flip() +
    theme()



