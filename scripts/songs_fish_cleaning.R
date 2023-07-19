#Cleaning of SONGS visual survey fish data into smaller data sizes
#created by: Jonathan Huang 2023-02-11
#updated 7/7/2023


#load libraries 
library(tidyverse)
library(here)


#import data - phase 2/3 now uploaded via url, 
# so only cleaning for phase 1 that is not public yet

#load data
# phase_2_3 <- read.csv(here("data", "songs_fish_leng_abd","reef_ts_fish_size_and_abundance-2022-11-29_08-33-37.csv"))
phase_1 <- read.csv(here("data", "reef_ts_fish_size_and_abundance_2000-to-2008_20230412.csv")) 
#-99999 denotes not recorded or not avaliable
#count can be 0 but still have a total_length, need clarification

#filter to have target species - PACL, OXCA, PANE, CHPU,EMJA, SEPU
songs_p1_pacl <- phase_1 %>% 
  # bind_rows(phase_1,phase_2_3) %>% 
  filter(species_code %in% c("PACL")) %>% 
write.csv(here("data","songs_clean_pacl.csv"))
  
songs_p1_pane <- phase_1 %>% 
  filter(species_code %in% c("PANE")) %>% 
  write.csv(here("data","songs_clean_pane.csv"))
  
songs_p1_sepu <- phase_1 %>% 
  filter(species_code %in% c("SEPU")) %>% 
  write.csv(here("data","songs_clean_sepu.csv"))

songs_p1_emja <- phase_1 %>% 
  filter(species_code %in% c("EMJA")) %>% 
  write.csv(here("data","songs_clean_emja.csv"))

songs_p1_chpu <- phase_1 %>% 
  filter(species_code %in% c("CHPU")) %>% 
  write.csv(here("data","songs_clean_chpu.csv"))

songs_p1_oxca <- phase_1 %>% 
  filter(species_code %in% c("OXCA")) %>% 
  write.csv(here("data","songs_clean_oxca.csv"))


#Analysis

view(songs)
glimpse(songs)
unique(songs$phase_built_code)

