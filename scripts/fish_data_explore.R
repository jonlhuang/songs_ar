##########################################################
#Data Exploration for Mark Steele's SONGS fish fecundity data 
#Jonathan Huang - Dec 2022
##########################################################
#Fish and Fecundity Data from Mark Steele's 

#clear environment
# rm(list=ls())

#install library
library(tidyverse)
library(beepr)
library(lubridate) #for time and date formatting
library(here)
getwd()

############----------RecFIN Fish Catch data (2003 - 2022)------------#############
#reading in all csv files in a folder
recfin<- map_df(list.files(here::here("Data/Fish/fisheries_length"),
                          pattern = '.csv', full.names = TRUE),
                         ~read.csv(.x) %>% 
                  mutate(across(.fns = as.character)) 
                %>% type.convert) 

#SEPARATE BY SPECIES
CHPU <- recfin[which(recfin$SPECIES_NAME == "CALIFORNIA SHEEPHEAD"),]
PACL <-recfin[which(recfin$SPECIES_NAME == "KELP BASS"),] 
PANE <-recfin[which(recfin$SPECIES_NAME == "BARRED SANDBASS"),] 
PANE <- PANE %>%  #couldn't drop NAs with numberic
  mutate(RECFIN_LENGTH_MM = as.character(RECFIN_LENGTH_MM)) %>% 
  drop_na(RECFIN_LENGTH_MM) %>% 
  mutate(RECFIN_LENGTH_MM = as.numeric(RECFIN_LENGTH_MM))

#Years after size regulation change
PACL2013 <- PACL %>% filter(RECFIN_YEAR %in% c(2013:2022))
PANE2013 <- PANE %>% filter(RECFIN_YEAR %in% c(2013:2022))
PANEall <- PANE %>% filter(RECFIN_YEAR %in% c(2003:2022)) 

#GET SUMMARY CATCH SIZE
mean(CHPU$RECFIN_LENGTH_MM)
sd(CHPU$RECFIN_LENGTH_MM)

mean(PACL$RECFIN_LENGTH_MM, na.rm = TRUE)
mean(PACL2013$RECFIN_LENGTH_MM, na.rm = TRUE)
sd(PACL$RECFIN_LENGTH_MM, na.rm = TRUE)

mean(PANE$RECFIN_LENGTH_MM)
sd(PANE$RECFIN_LENGTH_MM)



############----------MARK's FISH DATA-------------#############
fish <- read.csv(here::here("Data/Fish/steele_fecundity_data_2009_2020.csv"), 
                 header = TRUE, stringsAsFactors =  FALSE) %>% 
  filter(species %in% c("CHPU", "EMJA", "OXCA", "PACL","SEPU","PANE")) %>% 
  mutate(year = as.character(year),
         date = mdy(date), standard_length_mm = as.numeric(standard_length_mm),
         fork_length_mm = as.numeric(fork_length_mm),
         total_length_mm = as.numeric(total_length_mm),
         mass_grams = as.numeric(mass_grams),
         gonad_mass_grams = as.numeric(gonad_mass_grams),
         batch_fecundity = as.numeric(batch_fecundity),
         fish_growth_grams = as.numeric(fish_growth_grams)) %>% 
  select(year, species,reef_code, standard_length_mm, fork_length_mm,
         total_length_mm, mass_grams, gonad_mass_grams, fish_sex_maturity_code, 
         digestive_tract_mass_grams, gut_content_mass_grams, batch_fecundity, fish_growth_grams)%>% 
  mutate("growth/fecundity" = fish_growth_grams/batch_fecundity,
         "somatic/gonad" = fish_growth_grams/gonad_mass_grams)

#getting count of fish per species per year
fishtally <- fish %>% 
  group_by(species, year) %>% tally()

#Summarize by specie by year (NOTE: the NAs should be fixed in the final dataset - hopefully)
fish [fish == "."] <- NA         #replace . with NA
fish [fish == "#VALUE!"] <- NA   # what is #VALUE!(?)
fish [fish == "0"] <- NA         #is 0 no fecundity or NA
fish [fish == "NaN"] <- NA    
fishsum <- fish %>% filter(species %in% c("CHPU", "EMJA", "OXCA", "PACL","SEPU")) %>%  #SHOULD NOT HAVE TO FILTER IN FINAL DATA IF ERROR ARE FIXED
  group_by(species , year) %>% 
  #Get means of  values 
  summarise(sl = mean(standard_length_mm,na.rm = TRUE),
            fl = mean(fork_length_mm,na.rm = TRUE), 
            tl = mean(total_length_mm,na.rm = TRUE),
            mass = mean(mass_grams,na.rm = TRUE),
            gonadmass = mean(gonad_mass_grams,na.rm = TRUE),
            batch_fecundity = mean(batch_fecundity,na.rm = TRUE),
            growth = mean(fish_growth_grams,na.rm = TRUE)) 



#####plot data
length<- ggplot(data = fishsum ,aes(x = year, y = tl, 
                                 color = species,
                                 group = factor(species)))+
  geom_line()+
  theme_classic()
length

##### avg tl vs time plots by species
p <- ggplot(fishsum, aes(year, tl, color = species,
                         group = factor(species))) +
  labs(x = "year", y = "tl (mm)") +
  theme_classic()+
  theme(plot.title = element_text(size = 10))
p + facet_wrap(~fishsum$species) + geom_line() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Length (TL) vs Time")+
  theme(plot.title = element_text( hjust=0.5, size = 20))

#####avg fish growth over time
p <- ggplot(fishsum, aes(year, growth, color = species,
                         group = factor(species))) +
  geom_line()+
  labs(x = "year", y = "growth (g)") +
  theme_classic()+
  theme(plot.title = element_text(size = 10))
p + facet_wrap(~fishsum$species) + geom_line() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Growth (g) vs Time")+
  theme(plot.title = element_text( hjust=0.5, size = 20))


#by reef
p <- ggplot(fish, aes(year, fish_growth_grams, color = species,
                         group = factor(species))) +
  geom_bar(stat="identity", position="dodge", size=0.6) +
  # geom_point()+
  # geom_jitter(stat = "identity")+
  labs(x = "year", y = "growth (g)") +
  theme_classic()+
  theme(plot.title = element_text(size = 10))
p + facet_wrap(~fish$reef_code) +  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Growth (g) vs Time by reef")+
  theme(plot.title = element_text( hjust=0.5, size = 20))


#####length vs  growth
p <- ggplot(fish, aes(total_length_mm, fish_growth_grams, color = species,
                         group = factor(species))) +
  geom_point( size = 0.5)+ 
  labs(x = "TL (mm)", y = "growth (g)") +
  theme_classic()+
  theme(plot.title = element_text(size = 10))

# p + facet_wrap(~fish$species) +  stat_smooth(method = "lm",
#                                              formula =  y~x,  
#                                              geom = "smooth",
#                                              color = "black", linewidth = .75)+
#   labs(xlim(0, max(fish$fish_growth_grams)))+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   ggtitle("Growth (g) vs length")+
#   theme(plot.title = element_text( hjust=0.5, size = 20))
# 

#power function
p + facet_wrap(~fish$species) +  stat_smooth(method = "nls",
                                             formula =  y~a*x^b, start = list(a = 1, b = 1), se = FALSE,  
                                             geom = "smooth",
                                             color = "black", linewidth = .75)+
  labs(xlim(0, max(fish$fish_growth_grams)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Growth (g) vs length")+
  theme(plot.title = element_text( hjust=0.5, size = 20))




#####lenth vs fecundity
fishp <-fish %>% select(species, total_length_mm, batch_fecundity) 
fishp[sapply(fishp, is.infinite)]<- NA
fishp<- fishp %>% na.omit()
p <- ggplot(fishp, 
            aes(x=total_length_mm,y= batch_fecundity, color = species,
            group = factor(species))) +
  geom_point( size = 0.5)+ 
  labs(x = "TL (mm)", y = "batch_fecundity") +
  theme_classic()+
  theme(plot.title = element_text(size = 10))+
  ggtitle("fecundity vs length")+
  theme(plot.title = element_text( hjust=0.5, size = 20))+
  labs(xlim(0, max(fish$batch_fecundity)))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p

p + facet_wrap(~species, scales = "free") +
  stat_smooth(
    method = "nls",
    formula = y~a*x^b,
    method.args = list(start= list(a = 7, b = 5)), #need to find right a & b
    se = FALSE,
    geom = "smooth",
    color = "black", linewidth = .5)

