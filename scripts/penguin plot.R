library(tidyverse)
library(here)
library(janitor)
library(ggeasy)

penguins <- read_csv(here("data", "penguins.csv")) %>%
  clean_names() %>%
  mutate(species = case_when(species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie", 
                             species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap", 
                             species == "Gentoo penguin (Pygoscelis papua)"   ~ "Gentoo"))

penguins %>%
  ggplot(aes(x = species, y = body_mass_g, colour = species)) +
  geom_jitter() +
  theme_classic() + 
  easy_remove_legend() +
  labs(title = "Jitter plot of penguin body mass by species")
