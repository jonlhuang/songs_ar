#### Data exploration of SONGS size structured data
#### Jonathan Huang

####-----load libraries-----
library(tidyverse)
library(here)
library(patchwork)
library(ggpubr)
library(ggh4x) #additional ggplot axis 
library(lme4)
library(lmerTest)
library(performance) #for model checking
library(permuco) #permutation (randomization) test
library(car)

####------------import data ----------####

#add songs data
{
#upload phase 2/3 data from online repository
{inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/668/3/60afbd5190f50ba5271dacdf74e549f3" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


phase2_3 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "year",     
                 "date",     
                 "reef_code",     
                 "polygon",     
                 "phase_built_code",     
                 "transect_code",     
                 "visibility",     
                 "species_code",     
                 "genus_name",     
                 "species_name",     
                 "count",     
                 "total_length",     
                 "total_area_sampled"    ), check.names=TRUE)
unlink(infile1)

phase2_3 <- phase2_3 %>% 
filter(species_code %in% c("PACL", "OXCA", "PANE",
                           "CHPU", "EMJA", "SEPU")) 
}

#add phase 1 data
  condpath <- here("data","phase1_survey_clean") 
  files <- dir(path = condpath,
               pattern = ".csv",
               full.names = TRUE)
  songs_1 <- files %>% #iterating over files
    set_names() %>% # set the id for everything in "files"
    map_df(read_csv, .id = "filename")
  
  
# songs_1 <- read.csv(here("data","songs_clean.csv")) %>% 
#   filter(transtype_strata %in% c("BOTTOM", NA))   #Filter so only bottom transect in phase 1 matches phase 2 & 3 
  
}


songs <- bind_rows(songs_1,phase2_3) %>% 
  select(3:18)

#Data wrangle to have individual fish per row
songs_count <- songs %>% 
  mutate(species_code = as_factor(factor(species_code,   #reorder the factors
                                         levels = c( "SEPU","PACL","PANE","EMJA","CHPU","OXCA"))),
         total_length = as.numeric(total_length), #change length to numeric
         reef_code = as_factor(reef_code),
         year = as.factor(year)) %>%   
  filter(count!=0) %>% 
  summarise(count = sum(count), 
            .by = c(year,total_length, species_code, reef_code, transect_code,total_area_sampled)) %>%     #sum the count through the years
  uncount(count) %>%                   #reorder so each row = 1 count
  relocate(year, species_code,reef_code,transect_code, total_length, total_area_sampled) %>%  #reorder df order
  mutate(count = 1) #make a column so each row has one value

#summarizing the data by year, length, species, reef, and transect
songs_count_sum <- songs_count %>%  
  summarise(count = sum(count), #sum the count through the years
            .by = c(year, species_code, reef_code,transect_code,total_length)) 

#yoy density
 yoyden1 <- songs %>% #isolate fish categorized yoy ~8cm
  filter(species_code%in% c("PACL","CHPU","SEPU","PANE","OXCA", "EMJA"),
    total_length <=13) %>% 
  filter(species_code %in% c("PACL","CHPU","SEPU","PANE","OXCA") & total_length<=8)
  yoyden2 <- songs %>% #isolate fish categorized yoy ~13cm
    filter(species_code%in% c("PACL","CHPU","SEPU","PANE","OXCA", "EMJA"),
           total_length <=13) %>% 
    filter(species_code =="EMJA" & total_length<=13)
songs_yoy_fishdensity <- bind_rows(yoyden1,yoyden2) %>%  #combine into one df
  summarise(count =  sum(count),
            .by = c(year,species_code,reef_code,transect_code, total_area_sampled)) %>% 
  mutate(fish_density = count/total_area_sampled,
         species_code = as.factor(species_code),
         year = as.factor(year))
glimpse(songs_yoy_fishdensity)
  

  

#####------------------------ Add environmental data for correleation-------
#kelp density
kelp <- read.csv(url("https://portal.edirepository.org/nis/dataviewer?packageid=edi.667.3&entityid=6f44e7f393c932e0f72ccbf3b361e0fc")) %>% 
  summarise(ann_kelp = sum(number_of_plants),
            .by = c(year, reef_code,quadrat_area))
glimpse(kelp)

# kelp %>%
#   ggplot(aes(x = year, y = ann_kelp, group = reef_code,color = reef_code))+
#   geom_line()

#MOCI
moci <- read.csv(url("http://www.faralloninstitute.org/s/CaliforniaMOCI.csv")) %>% 
  select(c(time,Year,Season,Southern.California..32.34.5N.)) %>% 
  rename("socal_moci" = Southern.California..32.34.5N.) %>% 
  mutate(time = ymd(time)) %>% 
  separate(col = time,
           into = c("year","month","date"),
           sep = "-") %>% 
  select(c("year","month","date","Season","socal_moci")) %>% 
  summarise(ann_moci = mean(socal_moci),   #average 
            .by = year) %>% 
  mutate(year = as.character(year))
colnames(moci)
glimpse(moci)

#SST
oceanside_sst <- read.csv(url("https://erddap.sensors.axds.co/erddap/tabledap/edu_ucsd_cdip_045.csv?time%2Csea_water_temperature%2Csea_water_temperature_qc_agg%2Cz&time%3E%3D1997-05-19T15%3A38%3A34Z&time%3C%3D2023-06-28T20%3A28%3A20Z"))
oceanside_sst [oceanside_sst == "NaN"] <- NA         #replace NaN with NA
names(oceanside_sst) <- oceanside_sst[1,]            #make the first row the header 
oceanside_sst <- oceanside_sst[-1,]                  #add the first row to the header
oceanside_sstspread <- oceanside_sst %>%             #transform data to separate date and time
  mutate( time = as.character("time"),
          "degree_Celsius" = as.numeric(degree_Celsius)) %>% 
  na.omit(degree_Celsius) %>% 
  rename(sstqaqa= "NA") %>% 
  separate(col = UTC, sep = c("T"), into = c("date","time"), remove =  FALSE) %>%  #separate UTC into time and date
  mutate("time" = as.character(time),
         date = as.Date(date))
oceanside_ssttime <-                              #remove "Z" from the end of time string so only hour-min-sec format
  str_sub(oceanside_sstspread$time,1,str_length(oceanside_sstspread$time)-1)    
oceanside_sstf<-cbind(oceanside_sstspread,oceanside_ssttime) %>%      #combine vector into dataframe
  mutate("location" = "oceanside",
         year = year(date)) %>% 
  select("location","date","year","oceanside_ssttime","degree_Celsius", "m") %>% #select relevant columns
  rename("time" = oceanside_ssttime) %>% 
  summarise(ann_temp = mean(degree_Celsius),
            .by = year)

#BUTI - Nutrient transport
beuti <- read.csv(url("https://www.mjacox.com/wp-content/uploads/2023/07/BEUTI_monthly.csv")) %>% 
  select(c(year, month, X33N)) %>% 
  summarise(ann_beuti = mean(X33N),
            .by = year)

mei <- read.csv(here("data","Oceanographic","mei2023.csv")) %>% 
  rename("year" = X) %>% 
  pivot_longer(cols = c(DJ:ND),
               names_to = "month",
               values_to = "mei") %>% 
  summarise(ann_mei = mean(mei),
            .by = year) %>% 
  filter(year != 2023) #remove 2023 (incomplete year thus far)
  
### bind columns so one df
annual_env <- bind_cols(c(moci %>% filter(year %in% c(2000:2022)),
                          oceanside_sstf%>% filter(year %in% c(2000:2022)),
                          beuti%>% filter(year %in% c(2000:2022)),
                          mei%>% filter(year %in% c(2000:2022)))) %>% 
  select(c(year...1,ann_moci, ann_temp,ann_beuti,ann_mei)) %>% 
  rename("year" = year...1) %>% 
  mutate(year = as.factor(year))
glimpse(annual_env)

####-------functions-----

se <- function(x) sd(x)/sqrt(length(x))

#--- YOY data
fishYOY <- function(data,species,length) {
 df <-  data %>% #wrangle data so have each individual fish in one row
    filter(species_code == {{species}},
           total_length<=length) %>% 
    uncount(count)%>% 
    mutate(n = 1) %>% 
    group_by(year, species_code, reef_code, transect_code, total_length) %>% 
    summarise(count = sum(n)) #sum to get all fish by transect by length by year
 year <- data_frame(year = as_factor(c(2000:2021)),
                    species_code = as_factor(species))  #add so have all years avaliable
 df_all <- full_join(year, df) #combine into one dataframe with all years
 df2 <-  df_all %>% 
   mutate(count = if_else(year %in% c(2000:2006) & is.na(count), 0, 
                          if_else(
                            year %in% c(2009:2021) & is.na(count), 0,
                            if_else(
                              year %in% c(2007:2008) & is.na(count), NA,
                              count
                            )))) %>% 
   mutate(species_code = as.factor(species_code),
          transect_code = as.factor(transect_code)) %>% 
   mutate(survey_complete = case_when(year %in% c(2007,2008) ~ "no survey year", #specify years that have no or incomplete survey
                                      year == 2019 ~ "partial survey year",
                                      TRUE ~ " "))
 
} #number of fish at size at site at year at transect


fishYOY_sum <- function(data,species,length,reef) { 
  data %>% 
    filter(species_code == {{species}},
           total_length<=length,
           reef_code%in%{{reef}}) %>% 
    uncount(count) %>% 
    mutate(count = 1) %>% 
    group_by(year) %>% 
    summarize(count = sum(count))
}#summarize the abundance of YOY by size 

yoy <- function(data,species,size){
  wnr <- fishYOY_sum(data, species, size, "WNR")#take YOY aboundance from every site 
  bk <- fishYOY_sum(data, species, size, "BK")
  smk <- fishYOY_sum(data, species, size, "SMK")
  year <- data_frame(year = as_factor(c(2000:2021))) #make a list with all years to show every year
  YOY <- list(wnr,smk,bk) #make all sites into one list
  
df <-   left_join(year, #join year to have all years, with combined list of all reefs
            YOY %>% 
    reduce(full_join, #combine list using reduce with function "full_join" to have all reefs on one df
           by = "year") %>% 
    rename("WNR" = count.x, #rename columns to site names
           "SMK" = count.y ,
           "BK" = count)) 

#edit df to have 0 on counts and NA on data without data
df2 <-  df %>% 
  pivot_longer(cols = WNR:BK, #make longer to do it all at once
               names_to = "reef",
               values_to = "count") %>% 
  mutate(count = if_else(year %in% c(2000:2006) & is.na(count), 0, 
                 if_else(
                   year %in% c(2009:2021) & is.na(count), 0,
                   if_else(
                     year %in% c(2007:2008) & is.na(count), NA,
                     count
                   )))) %>% 
  pivot_wider(names_from = reef,
              values_from = count)%>% 
  mutate(species_code = as.factor(species))%>% 
  mutate(survey_complete = case_when(year %in% c(2007,2008) ~ "no survey year", #specify years that have no or incomplete survey
                                     year == 2019 ~ "partial survey year",
                                     TRUE ~ " "))

} #summarize the total abundundance of YOY by year



#--- Resident (Juvenile + Adult data)
fishRes <- function(data,species,length) {
  data %>% 
    filter(species_code == {{species}},
           total_length>=length) %>% 
    uncount(count)%>% 
    mutate(n = 1) %>% 
    group_by(year, species_code, reef_code, transect_code, total_length) %>% 
    summarise(n = sum(n))
} #number of fish at size at site at year at transect

fishRes_sum <- function(data,species,length,reef) { 
  data %>% 
    filter(species_code == {{species}},
           total_length >=length,
           reef_code%in%{{reef}}) %>% 
    uncount(count) %>% 
    mutate(count = 1) %>% 
    group_by(year) %>% 
    summarize(count = sum(count))
}#summarize the abundance of YOY by size 

Res <- function(data,species,size){
  wnr1 <- fishRes_sum(data, species, size, "WNR")#take YOY aboundance from every site 

  bk1 <- fishRes_sum(data, species, size, "BK")
  
  smk1 <- fishRes_sum(data, species, size, "SMK")
  year <- data_frame(year = as_factor(c(2000:2021))) #make a list with all years to show every year
  Res <- list(wnr1,smk1,bk1) #make all sites into one list
  
  df <-   left_join(year, #join year to have all years, with combined list of all reefs
                    Res %>% 
                      reduce(full_join, #combine list using reduce with function "full_join" to have all reefs on one df
                             by = "year") %>% 
                      rename("WNR" = count.x, #rename columns to site names
                             "SMK" = count.y ,
                             "BK" = count))
  
  df2 <-  df %>% 
    pivot_longer(cols = WNR:BK, #make longer to do it all at once
                 names_to = "reef",
                 values_to = "count") %>% 
    mutate(count = if_else(year %in% c(2000:2006) & is.na(count), 0, 
                           if_else(
                             year %in% c(2009:2021) & is.na(count), 0,
                             if_else(
                               year %in% c(2007:2008) & is.na(count), NA,
                               count
                             )))) %>% 
    pivot_wider(names_from = reef,
                values_from = count)%>% 
    mutate(species_code = species)%>% 
    mutate(survey_complete = case_when(year %in% c(2007,2008) ~ "no survey year", #specify years that have no or incomplete survey
                                       year == 2019 ~ "partial survey year",
                                       TRUE ~ " "))
}


#--- graph of all species on one
ar_nr <- function(data,reefar,xlim,reefnr, title){
  p1 <- ggplot(data %>% filter(reef_code%in% {{reefar}}) %>% 
                 mutate(year = as_factor(year)),    #data 
               aes(x = total_length, color = year))+   #specify x & solor by year
    geom_density(linewidth = 1.5)+   #use density plot
    facet_wrap(~species_code,ncol=1, scales = "free")+    #separate plots by species
    ggh4x::facetted_pos_scales(x = NULL, y = list( #custom scales for each facet
      SEPU = scale_y_continuous(limits = c(0,0.075)),
      PACL = scale_y_continuous(limits = c(0,0.15)),
      PANE = scale_y_continuous(limits = c(0,0.24)),
      EMJA = scale_y_continuous(limits = c(0,0.15)),
      CHPU = scale_y_continuous(limits = c(0,1.0)),
      OXCA = scale_y_continuous(limits = c(0,1.0))
    ))+
    scale_x_continuous(breaks = seq(0,xlim, 10),
                       limits = c(0,NA))+  #scale x to be max lenght at increment of 5cm
    labs(x = "Total Length (mm)",    #edit axis & legend labels
         y = "Density", 
         color = "Year",
         title = element_text("Size distribution on Artifical Reef (WNR)"))+
    scale_color_viridis_d()+  #color palatte
    theme_classic()+
    theme(panel.background = element_rect(fill = "grey90", color = "black"),
          plot.title = element_text(hjust = 0.5, size = 40),
          axis.text = element_text(size = 30),
          axis.title = element_text(size = 17),
          strip.text = element_text(size = 30),
          legend.text = element_text(size = 30),
          legend.key.size = unit(1.5,"cm"),
          legend.title = element_text(size = 50)) #remove lengend for ggarrange
  p1

  p2 <- data %>% filter(reef_code%in% reefnr) %>% 
    mutate(year = as_factor(year)) %>% 
    ggplot()+   #specify x & solor by year
    geom_density(
      aes(x = total_length, color = year, group = year),linewidth = 1.5)+   #use density plot
    facet_wrap(~species_code, ncol=1, scales = "free")+ #separate plots by species
    ggh4x::facetted_pos_scales(x = NULL, y = list( #custom scales for each facet
      SEPU = scale_y_continuous(limits = c(0,0.075)),
      PACL = scale_y_continuous(limits = c(0,0.15)),
      PANE = scale_y_continuous(limits = c(0,0.24)),
      EMJA = scale_y_continuous(limits = c(0,0.15)),
      CHPU = scale_y_continuous(limits = c(0,1.0)),
      OXCA = scale_y_continuous(limits = c(0,1.0))
    ))+
    scale_x_continuous(breaks = seq(0,xlim, 10),
                       limits = c(0,NA))+  #scale x to be max lenght
    labs(x = "Total Length (mm)",    #edit axis & legend labels
         y = "Density", 
         color = "Year",
         title = element_text("Size distribution on Natural Reefs (BK & SMK)"))+
    # scale_color_viridis_d()+  #color palatte
    scale_fill_manual( values = c("#440154","#481467", "#482576","#453781","#404688","#39558c", #manually fill in legend
                                           "#33638d","#2d718e","#287d8e","#238a8d","#1f968b",
                                           "#20a386","#29af7f","#3dbc74","#56c667","#75d054","#95d840",
                                           "#bade28","#dde318","#fde725"),
                                           aesthetics = c("color", "fill"),
                       name = "Year")+
    theme_classic()+ #edit theme
    theme(panel.background = element_rect(fill = "grey90", color = "black"),
          plot.title = element_text(hjust = 0.5, size = 40),
          axis.text = element_text(size = 30),
          axis.title = element_text(size = 17),
          strip.text = element_text(size = 30), #facet font size 
          legend.text = element_text(size = 30), #legend font size
          legend.key.size = unit(1.5,"cm"), #legend key size
          legend.title = element_text(size = 50))
  p2
  
  arrange <- ggarrange((p1+rremove("xlab")+rremove("ylab")+rremove("legend"))+(p2+rremove("ylab")+rremove("xlab")), #edit which axis labels to keep
                       labels = NULL, 
                       common.legend = TRUE, #have one single legend for both garphs
                       legend = "right")
  annotate_figure(arrange, 
                  left = text_grob("Density", #add common axis
                                   rot = 90, 
                                   size = 45),
                  bottom = text_grob("Total Length (mm)",
                                     size = 45))+
    theme(plot.margin = margin(1,1,1,1, "cm")) #add margin space
  ggsave(here("output", title),
         width = 25, height = 33)
}

#---- graph of individual species
species_reefs <- function(data,species, reefar,xlim,ylim, yinterval,
                          reefbk,reefsmk, reefnr, title, arnr_filename, reefs_filename){
  p1 <- ggplot(songs_count %>% filter(reef_code%in% reefar,
                                      species_code%in% {{species}})%>% 
                 mutate(year = as_factor(year)),    #data 
               aes(x = total_length, fill = year))+   #specify x & solor by year
    geom_density()+  #use density plot
    facet_wrap(~year, ncol = 1, scales = "free_y")+    #separate plots by species
    scale_x_continuous(breaks = seq(0,xlim, 10),
                       limits = c(0,NA))+  #scale x to be max lenght at increment of 5cm
    scale_y_continuous(breaks = seq(0,ylim, yinterval),
                       limits = c(0,ylim))+ #scale y tick mark
    labs(x = "Total Length (mm)",    #edit axis & legend labels
         y = "Density", 
         fill = "Year",
         title = element_text("Artificial Reef"))+
    scale_fill_viridis_d()+  #color palatte
    theme_classic()+
    theme(panel.background = element_rect(fill = "grey96", color = "black"),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none",
          panel.spacing = unit(1, "mm"),
          strip.text = element_blank())
  p1
  
  p2 <- ggplot(data %>%  #both natural reefs
                 filter(reef_code%in% c({{reefnr}}),
                        species_code%in% {{species}}) %>% 
                 mutate(year = as_factor(year)),    #data 
               aes(x = total_length, fill = year))+   #specify x & solor by year
    geom_density()+   #use densiity plot
    facet_wrap(~year, ncol = 1, scales = "free_y")+    #separate plots by species
    scale_x_continuous(breaks = seq(0,xlim, 10),
                       limits = c(0,NA))+  #scale x to be max length
    scale_y_continuous(breaks = seq(0,ylim, yinterval),
                       limits = c(0,ylim))+ #scale y tick mark
    labs(x = "Total Length (mm)",    #edit axis & legend labels
         y = "Density", 
         fill = "Year",
         title = element_text("Natural Reefs"))+
    scale_fill_viridis_d()+  #color palatte
    theme_classic()+
    theme(panel.background = element_rect(fill = "grey96", color = "black"),
          plot.title = element_text(hjust = 0.5),
          panel.spacing = unit(1, "mm"),
          strip.text = element_blank(),
          legend.position = "none")
  
  p2.1 <- ggplot(songs_count %>% 
                   filter(reef_code%in% reefbk,
                          species_code%in%species) %>% 
                   mutate(year = as_factor(year)),    #data 
                 aes(x = total_length, fill = year))+   #specify x & solor by year
    geom_density()+   #use densiity plot
    facet_wrap(~year, ncol = 1, scales = "free_y")+    #separate plots by species
    scale_x_continuous(breaks = seq(0,xlim, 10),
                       limits = c(0,NA))+  #scale x to be max length
    scale_y_continuous(breaks = seq(0,ylim, yinterval),
                       limits = c(0,ylim))+ #scale y tick mark
    labs(x = "Total Length (cm)",    #edit axis & legend labels
         y = "Density", 
         fill = "Year",
         title = element_text("BK"))+
    scale_fill_viridis_d()+  #color palatte
    theme_classic()+
    theme(panel.background = element_rect(fill = "grey96", color = "black"),
          plot.title = element_text(hjust = 0.5),
          panel.spacing = unit(1, "mm"),
          strip.text = element_blank(),
          legend.position = "none")
  
  p2.2 <- ggplot(songs_count %>% filter(reef_code%in% reefsmk,
                                        species_code%in%species)%>% 
                   mutate(year = as_factor(year)),    #data 
                 aes(x = total_length, fill = year))+   #specify x & solor by year
    geom_density()+   #use densiity plot
    facet_wrap(~year, ncol = 1, scales = "free_y")+    #separate plots by species
    scale_x_continuous(breaks = seq(0,xlim, 10),
                       limits = c(0,NA))+  #scale x to be max length
    scale_y_continuous(breaks = seq(0,ylim, yinterval),
                       limits = c(0,ylim))+ #scale y tick mark
    labs(x = "Total Length (mm)",    #edit axis & legend labels
         y = "Density", 
         fill = "Year",
         title = element_text("SMK"))+
    scale_fill_viridis_d()+  #color palatte
    theme_classic()+
    theme(panel.background = element_rect(fill = "grey96", color = "black"),
          plot.title = element_text(hjust = 0.5),
          panel.spacing = unit(1, "mm"),
          strip.text = element_blank(),
          legend.position = "none")
  
  #combine both plots - by ar/nr
  combined <- ggarrange(p1+
                          p2+ rremove("ylab"),
                        # p2.2+rremove("ylab")+
                        # p2.1+rremove("ylab"),
                        common.legend = TRUE, 
                        legend = "right")
  #add title at the top of the graph
  annotate_figure(combined,
                  top = text_grob(title, 
                                  face = "bold",
                                  size = 14))
  
  ggsave(here("output", arnr_filename),
         width = 9, height = 6)

#Individual reefs
  combined <- ggarrange(p1+
                        p2.2+rremove("ylab")+
                        p2.1+rremove("ylab"),
                        common.legend = TRUE, 
                        legend = "right")
  #add title at the top of the graph
  annotate_figure(combined,
                  top = text_grob(title, 
                                  face = "bold",
                                  size = 14))
  
  ggsave(here("output", reefs_filename),
         width = 9, height = 6)
}

###############################################
############# data exploration ################
###############################################

#check number of transects - NOTE: this only shows transects that have value fish seen - not all transects surveyed
songs_count %>%
  mutate(transect_code = as_factor(transect_code)) %>% 
  summarise(transect_num = sum(length(unique(transect_code))), #summarize by year and reef
            .by = c(year,reef_code))

#list of transect for each parts
unique(case_when(songs_count$year %in% c(2000:2004) & songs_count$reef_code == "WNR" ~ songs_count$transect_code,
                         FALSE~" "))
unique(case_when(songs_count$year %in% c(2005:2006) & songs_count$reef_code == "WNR"~ songs_count$transect_code,
                            FALSE~" "))
unique(case_when(songs_count$year %in% c(2010:2021) & songs_count$reef_code == "WNR"~ songs_count$transect_code,
                 FALSE~" "))
         
  # summarise(unique(transect_code))

#######-----------Exploration for environemnt----------##########

songs_year <- songs_count %>%  #add labels to re or post-blob by size
  drop_na() %>% 
  mutate( blob_case =  case_when(year<2013 ~"pre-blob",
                    year>=2013 & year<=2016 ~"blob",
                    year>=2017 ~"post-blob",
                    TRUE ~NA),
          blob_case = as_factor(factor(blob_case,
                                       levels = c("pre-blob","blob","post-blob")))
  )
    
size_summary <- songs_year %>%  #summarize size by category
  group_by(blob_case,species_code) %>% 
  summarize(mean = mean(total_length),
            sd = sd(total_length),
            se = se(total_length))




####-----Number of YOY-----####

#summary by year by species
yoy_emja <- yoy(songs_count_sum, "EMJA", 13) #12.7cm from SONGS MMP, rounded up to 13
yoy_pacl <- yoy(songs_count_sum, "PACL", 8) #7.62cm from SONGS MMP, rounded up to 8- same for remaining species
yoy_chpu <- yoy(songs_count_sum, "CHPU", 8)
yoy_sepu <- yoy(songs_count_sum, "SEPU", 8)
yoy_pane <- yoy(songs_count_sum, "PANE", 8)
yoy_oxca<- yoy(songs_count_sum, "OXCA", 8) 

#make yoy into one dataframe
yoy_all <- bind_rows(yoy_chpu, yoy_emja,yoy_oxca, 
                     yoy_pacl,yoy_pane,yoy_sepu)



#export to one excel sheet
# yoy_tables <- list("sepu" = yoy_sepu, "pane" = yoy_pane,  "pacl" = yoy_pacl, 
#                    "emja" = yoy_emja,"chpu" = yoy_chpu, "oxca" = yoy_oxca)
# openxlsx::write.xlsx(yoy_tables, file = here("output","yoy.xlsx"))

#abundance and size at each transect
yoy_emja_size <- fishYOY(songs_count, "EMJA", 13) #12.7cm from SONGS MMP, rounded up to 13 
yoy_pacl_size <- fishYOY(songs_count, "PACL", 8) #7.62cm from SONGS MMP, rounded up to 8 - same for remaining species
yoy_chpu_size <- fishYOY(songs_count, "CHPU", 8)
yoy_sepu_size <- fishYOY(songs_count, "SEPU", 8)
yoy_pane_size <- fishYOY(songs_count, "PANE", 8)
yoy_oxca_size <- fishYOY(songs_count, "OXCA", 8) 

#make into one dataframe
yoy_all_size <- bind_rows(yoy_emja_size,yoy_pacl_size,yoy_chpu_size,
                          yoy_sepu_size,yoy_pane_size,yoy_oxca_size) %>% 
  mutate(transect_code = as.factor(transect_code)) %>% 
  drop_na()

yoy_pacl_den <- songs_yoy_fishdensity %>% 
  filter(species_code == "PACL") %>% 
  left_join(.,annual_env)
glimpse(yoy_pacl_den)
yoy_sepu_den <- songs_yoy_fishdensity %>% 
  filter(species_code == "SEPU")
yoy_pane_den <- songs_yoy_fishdensity %>% 
  filter(species_code == "PANE")
yoy_emja_den <- songs_yoy_fishdensity %>% 
  filter(species_code == "EMJA")
yoy_chpu_den <- songs_yoy_fishdensity %>% 
  filter(species_code == "CHPU")
yoy_oxca_den <- songs_yoy_fishdensity %>% 
  filter(species_code == "OXCA")


################################################# STILL TESTING
############ Statistical Tests - YOY  ########### 
#################################################



#testing difference between each site - repeated measure anova
### Find a non-parametric way - assumptions not met -friedman test potentially - or randomization test

# chpu_anova<-lmer(count ~ reef_code + year + reef_code:year + (1|reef_code/transect_code),
#              data=yoy_emja_size)
# anova(chpu_anova)
# summary(chpu_anova)
# plot(chpu_anova)
# check_model(chpu_anova)


#### ---- Randomization test with repeated measure anova 
# Asing Questions
#info on randomization (or permutation) test://www.uvm.edu/~statdhtx/StatPages/Randomization%20Tests/RandomRepeatedMeasuresAnovaR.html
#Note: randomization test uses random assignment 
# SO, if we are abile to shuffle randomaly abundance at each year, the sites should be the same 


repmes_permu <- function(data) aovperm(fish_density ~ reef_code
                                       + Error(data$transect_code/data$reef_code),
                                       data = data)


# yoy_all_permu <- aovperm(count ~ reef_code*year*species_code + Error(yoy_all_size$transect_code/yoy_all_size$reef_code),
#                      data = yoy_all_size)
# yoy_all_permu #No effect from any of the factors

#density use yoy_den, abundance use yoy_size
sepu_permu <- repmes_permu(yoy_sepu_den) 
summary(sepu_permu) # reef significant 
pacl_permu <- repmes_permu(yoy_pacl_den) 
summary(pacl_permu) # reef significant  - density
pane_permu <- repmes_permu(yoy_pane_den) #Not enough data 
summary(pane_permu)
chpu_permu <- repmes_permu(yoy_chpu_den)  
summary(chpu_permu) # reef significant - density
emja_permu <- repmes_permu(yoy_emja_den) 
summary(emja_permu) #reef significant- density
oxca_permu <- repmes_permu(yoy_oxca_den) 
summary(oxca_permu) #reef not significant- density


#correlation test 
pacl_sum <- yoy_pacl_den %>% 
  summarise(ann_density = mean(fish_density),
            ann_moci = mean(ann_moci),
            ann_temp = mean(ann_temp),
            ann_beuti = mean(ann_beuti),
            ann_mei = mean(ann_mei),
            .by = c(year, reef_code)) %>% 
  mutate(reef_code = as.factor(reef_code), levels(c("WNR","SMK","BK")))

test <- cor.test(pacl_sum$ann_moci, pacl_sum$ann_density, method = "spearman", data = pacl_sum)
test

df <- data.frame(matrix(ncol = 6,
                        nrow = 4))
colnames(df) <- c("wnr_rho", "wnr_p-value", "smk_rho","smk_p-value", "bk_rho","bk_p-value")


# test for correlation test, no separating by sites yet
# for (i in 4:ncol(pacl_sum)) {
#   
#   df$rho [i-3] <- cor.test(pacl_sum[,3],pacl_sum[,i], method = "spearman")$estimate
#   df$`p-value` [i-3] <- cor.test(pacl_sum[,3],pacl_sum[,i], method = "spearman")$p.value
# }

df_wnr <- pacl_sum %>% filter(reef_code %in%  c("WNR")) 
df_smk <- pacl_sum %>% filter(reef_code %in%  c("SMK")) 
df_bk <- pacl_sum %>% filter(reef_code %in%  c("BK")) 

# for loop to test correltation for every site by environemtal factor

  for (j in 4:ncol(pacl_sum)) {

    df$wnr_rho[j-3] <- cor.test(df_wnr$ann_density, df_wnr [,j], method = "spearman")$estimate
    df$`wnr_p-value`[j-3] <-  cor.test(df_wnr$ann_density, df_wnr [,j], method = "spearman")$p.value
    df$smk_rho <- cor.test(df_smk$ann_density, df_smk [,j], method = "spearman")$estimate
    df$`smk_p-value`[j-3] <- cor.test(df_smk$ann_density, df_smk [,j], method = "spearman")$p.value
    df$bk_rho <- cor.test(df_bk$ann_density, df_bk [,j], method = "spearman")$estimate
    df$`bk_p-value`[j-3] <- cor.test(df_bk$ann_density, df_bk [,j], method = "spearman")$p.value

    rownames(df) <- paste(colnames(pacl_sum[3]), "on",
                          colnames(pacl_sum [4:ncol(pacl_sum)]))
  }
view(df)


glimpse(pacl_sum)

#######-----------Plot----------##########

### -- Line plot 

#fish count by year
year <- data_frame(year = as.factor(c(2000:2021)))
songs_tot_count <- songs_count_sum %>% #wrangle to have sum count of every species 
  filter(reef_code == "WNR") %>% 
  mutate(species_code = as.factor(species_code),
         year = as.factor(year)) %>% 
  uncount(count) %>% 
  mutate(count = 1) %>% 
  reframe(tot_count = sum(count),
            .by = c(year, species_code)) %>% 
  pivot_wider(names_from = species_code,
              values_from = tot_count)
left_join(year, songs_tot_count) %>% 
  pivot_longer(cols = c(CHPU:PACL),
               names_to = "species_code",
               values_to = "tot_count") %>% 
  ggplot(aes(x = year, y = tot_count, color = species_code, group = species_code))+
  geom_line()+
  geom_point()+
  facet_wrap(~species_code, scales = "free")+
  labs(y = "Total Count", x = "year",
       title = "Total individual counts by year at WNR")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = .1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")
# ggsave(here("output", "tot_count_year_WBR.png"),
#        width = 15, height = 9)


#yoy by year
yoy_all %>%
  pivot_longer(cols = c(WNR:BK),
               names_to = "reef",
               values_to = "tot_count") %>% 
  filter(reef == "WNR")%>%
  ggplot(aes(x = year, y = tot_count, color = species_code, group = reef))+
  geom_line()+
  geom_point()+
  facet_wrap(~species_code, scales = "free")+
  labs(y = "Total Count", x = "year",
       title = "Total YOY individual counts by year at WNR")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = .1),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")
# ggsave(here("output", "YOY_tot_count_year_WBR.png"),
#        width = 15, height = 9)

### -- Histogram
#graph of yoy by size by species
ggplot(yoy_oxca_size %>% 
         filter(reef_code %in% c("WNR",NA)), #select only one reef from data
         aes(x = total_length, y = count))+
  geom_col(aes(fill = reef_code))+
  facet_wrap(~year, scales = "free")+ #separate by year, with adusting axis
  scale_x_continuous(breaks = seq(0,15,2), limits = c(0,14))+
  labs(y = "count", x = "Total Length (cm)",
       title = "YOY size distribution by year at WNR",
       fill = "Reef Name")+ #legend title
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")+
  geom_text(aes(x = 4, y =2, label = survey_complete), size = 3) #add survey info - text in plots
# ggsave(here("output", "YOY_oxca_count_year_WNR.png"),
#        width = 15, height = 10)

#graph of yoys count by length
yoy_all_size %>% ungroup() %>% 
  filter(reef_code == "WNR") %>%
  summarise(tot_count = sum(count), .by = c(year,species_code, reef_code, total_length)) %>% 
  ggplot(aes(x = total_length, y = tot_count, fill = species_code))+
  geom_col()+
  facet_wrap(~species_code, scales = "free_y")+
  labs(y = "Total count", x = "Total Length (cm)",
       title = "YOY size distribution by species at WNR",
       fill = "Reef Name")+ #legend title
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")
# ggsave(here("output", "YOY_tot_count_sizedist_specie_WNR.png"),
#        width = 15, height = 10)

#raw number of YOY to resident
ggplot(songs_count %>% #add column for YOY or resident
      mutate(life_stage = case_when(species_code %in% c("EMJA") & total_length <12.7 ~ "YOY",
                                    species_code %in% c("PACL","CHPU","SEPU","PANE","OXCA") & total_length<7.62 ~ "YOY",
                                    TRUE~"resident")) %>% 
        filter(species_code == "SEPU"))+
  geom_bar(aes(x = total_length, group = factor(reef_code), fill = reef_code),
           position = "dodge")+
  facet_wrap(~year, scales = "free_y")+
  geom_vline(xintercept = 7.62, 
             linetype = "dashed",
             color = "black",
             linewidth = 0.3)+
  theme_bw()
  

###-- Denisty plot
ggplot(yoy_all_size,
       aes(x = total_length, fill = species_code))+
  geom_density(adjust = 2)+ #binwith of 1 
  facet_wrap(~species_code)


######---------byspecie and by site---------
#all species and plots on an ar_nr
ar_nr(songs_count,"WNR",75,c("BK","SMK"), "AR_NR_density_sizedistribution.pdf")


#SEPU
species_reefs(songs_count, "SEPU", "WNR", 75, 0.1, 0.08, "BK","SMK",c("BK","SMK"),
              "Size distribution of California Sheephead",
              arnr_filename =  "Test_AR_NR_density_distribution.pdf",
              reefs_filename = "Test_site_density_distribution.pdf")

#PACL
species_reefs(songs_count, "PACL", "WNR", 75, 0.2, 0.15, "BK","SMK",c("BK","SMK"),
              "Size distribution of Kelp Bass",
              arnr_filename =  "PACL_AR_NR_density_distribution.pdf",
              reefs_filename = "PACL_site_density_distribution.pdf")

#CHPU
species_reefs(songs_count, "CHPU", "WNR", 75, 1.4, 0.5, "BK","SMK",c("BK","SMK"),
              "Size distribution of Blacksmith",
              arnr_filename =  "CHPU_AR_NR_density_distribution.pdf",
              reefs_filename = "CHPU_site_density_distribution.pdf")

#EMJA
species_reefs(songs_count, "EMJA", "WNR", 75, 0.2, 0.15, "BK","SMK",c("BK","SMK"),
              "Size distribution of Black Perch",
              arnr_filename =  "EMJA_AR_NR_density_distribution.pdf",
              reefs_filename = "EMJA_site_density_distribution.pdf")

#OXCA
species_reefs(songs_count, "OXCA", "WNR", 75, 1.0, 0.4, "BK","SMK",c("BK","SMK"),
              "Size distribution of Señorita",
              arnr_filename =  "OXCA_AR_NR_density_distribution.pdf",
              reefs_filename = "OXCA_site_density_distribution.pdf")

#####-------Size simmary with before-after-heatwave-----

size_summary %>% 
  ggplot(aes(x = species_code,y = mean))+
  geom_bar(aes(group = blob_case,
               fill = blob_case),
           position = "dodge",
           stat = "identity")+
  geom_errorbar(aes(x = species_code,ymin = mean-se, ymax = mean+se,
                  group = blob_case ), 
                position = position_dodge(0.95)
)+
  labs(y = "mean Total Length (mm)",
       x = element_blank(),
       fill = "Blob stage")+
  scale_fill_manual(values = c("coral", "darkred","lightblue"))+
  theme_bw()

