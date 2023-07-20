#Data import of raw data for the SONGS Artificial reef project
#created Jul 19th, 2023 by Jonathan Huang

#import library
library(tidyverse)
library(here)

#####------------------------ Import fish survey data----------------
#import phase 2 & 3 data from public repo, and phase 1 from local .csv files
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
  
  #add phase 1 data from existing folder
  condpath <- here("data","phase1_survey_clean") 
  files <- dir(path = condpath,
               pattern = ".csv",
               full.names = TRUE)
  songs_1 <- files %>% #iterating over files
    set_names(nm = files) %>% # set the id for everything in "files"
    map_df(read_csv, .id = "filename")
  
}

songs <- bind_rows(songs_1,phase2_3) %>% 
  select(3:18)



#####------------------------ Add environmental data for correleation-------
#kelp density
kelp <- read.csv(url("https://portal.edirepository.org/nis/dataviewer?packageid=edi.667.3&entityid=6f44e7f393c932e0f72ccbf3b361e0fc"))
glimpse(kelp)

#MOCI
moci <- read.csv(url("http://www.faralloninstitute.org/s/CaliforniaMOCI.csv")) 
colnames(moci)
glimpse(moci)

#SST
oceanside_sst <- read.csv(here("data","Oceanographic","oceanside_sst.csv"))


#BUTI - Nutrient transport
beuti <- read.csv(url("https://www.mjacox.com/wp-content/uploads/2023/07/BEUTI_monthly.csv")) 

#MEI
mei <- read.csv(here("data","Oceanographic","mei2023.csv")) 

#pdo - https://www.ncei.noaa.gov/access/monitoring/pdo/
pdo <- read.csv(url("https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat")) %>% 
  separate_wider_delim(cols = ERSST.PDO.Index.,
                       delim = "\t", names = c("year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                       too_many = "debug")

pdo <- read.delim(file = url("https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat"),header = T, sep ="\t")

pdo1 <- as.data.frame(read.table(file = url("https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat"),
                                 header = TRUE, skip = 2, sep = "\t",
                                 col.names = c("year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))

pdo <- read.table(file = url("https://www.ncei.noaa.gov/access/monitoring/pdo/"), header = T, sep = "\t")
view(pdo)
