library(tidyverse)

load("data/Zimai_ms_raw.Rdata")

# check the age difference in suitable and unsuitable males
conceptions_full %>% 
  filter(!is.na(maleID)) %>% 
  group_by(male.type) %>% 
  ggplot(data = ., aes(x = male.type, y = male.age)) +
  geom_violin() +
  geom_point()
  
# check male characteristics and mating success
conceptions_full %>% 
  filter(!is.na(maleID)) %>% 
  group_by(maleID, male.type, male.age, male.lifstg, male.)) %>% 
  summarise(number_sires = n())