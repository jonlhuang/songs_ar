##########################################################
#Test code for comaring SST moorings from nearby SONGS Reefs
#Jonathan Huang - Nov 2022
##########################################################

library(tidyverse)
library(beepr)
library(lubridate) #for time and date formatting
library(RColorBrewer)
# install.packages("forecast")
library(forecast)

# install.packages("magrittr")
# magrittr::split_chain
# devtools::install_github('tidyverse/magrittr@v1.5')

########-------------- Import & transformation of data frame --------------###########

##import data
danapt_sst <- read.csv(here::here("data/Oceanographic/danapoint_sst.csv"))
danapt_sst [danapt_sst == "NaN"] <- NA         #replace NaN with NA
names(danapt_sst) <- danapt_sst[1,]            #make the first row the header 
danapt_sst <- danapt_sst[-1,]                  #add the first row to the header
danapt_sstspread <- danapt_sst %>%             #transform data to separate date and time
 mutate( time = as.character("time"),
         "degree_Celsius" = as.numeric(degree_Celsius)) %>% 
  na.omit(degree_Celsius) %>% 
  rename(sstqaqa= "NA") %>% 
  separate(col = UTC, sep = c("T"), into = c("date","time"), remove =  FALSE) %>%  #separate UTC into time and date
  mutate("time" = as.character(time),
         date = as.Date(date))
danapt_ssttime <-                              #remove "Z" from the end of time string so only hour-min-sec format
  str_sub(danapt_sstspread$time,1,str_length(danapt_sstspread$time)-1)    
danapt_sstf<-cbind(danapt_sstspread,danapt_ssttime) %>%                   #combine vector into dataframe
  mutate("location" = "danapt") %>% 
  select("location","date","danapt_ssttime","degree_Celsius", "m") %>%                #select relevant columns
  rename("time" = danapt_ssttime)

#import oceanside SST data
oceanside_sst <- read.csv(here::here("data/Oceanographic/oceanside_sst.csv"))
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
oceanside_sstf<-cbind(oceanside_sstspread,oceanside_ssttime) %>%                   #combine vector into dataframe
  mutate("location" = "oceanside") %>% 
  select("location","date","oceanside_ssttime","degree_Celsius", "m") %>%                #select relevant columns
  rename("time" = oceanside_ssttime)

#Scripps data
scripps_sst <- read.csv(here::here("data/Oceanographic/scripps.csv"))
scripps_sst [scripps_sst == "NaN"] <- NA         #replace NaN with NA
names(scripps_sst) <- scripps_sst[1,]            #make the first row the header 
scripps_sst <- scripps_sst[-1,]                  #add the first row to the header
scripps_sstspread <- scripps_sst %>%             #transform data to separate date and time
  mutate( time = as.character("time"),
          "degree_Celsius" = as.numeric(degree_Celsius)) %>% 
  na.omit(degree_Celsius) %>% 
  rename(sstqaqa= "NA") %>% 
  separate(col = UTC, sep = c("T"), into = c("date","time"), remove =  FALSE) %>%  #separate UTC into time and date
  mutate("time" = as.character(time),
         date = as.Date(date))
scripps_ssttime <-                              #remove "Z" from the end of time string so only hour-min-sec format
  str_sub(scripps_sstspread$time,1,str_length(scripps_sstspread$time)-1)    
scripps_sstf<-cbind(scripps_sstspread,scripps_ssttime) %>%                   #combine vector into dataframe
  mutate("location" = "scripps") %>% 
  select("location","date","scripps_ssttime","degree_Celsius", "m") %>%                #select relevant columns
  rename("time" = scripps_ssttime)


#combine datasets
sstcomp<- rbind(danapt_sstf,oceanside_sstf,scripps_sstf) %>% mutate( "location" = as.factor(location),
                                                        "date_time" = paste(date,time, sep = " "),
                                                        date_time = ymd_hms(date_time))

# #set date and time format - old, replaced with lubridate package
# compstr <- as.POSIXct(as.character(sstcomp$date_time), format = "%Y-%d-%m %H:%M:%S", tz = "America/Los_Angeles")
# #combine strings to dataset, and select
# sstcompf <- cbind(sstcomp,compstr) %>%
#   select("location","compstr","degree_Celsius", "m") %>%
#   rename("date_time" = compstr)

# bind_cols - the tidy way insead of cbind/rbind
# OlsonNames() #check for valid timezones



########-------------- Plotting time series --------------###########

#graphing time series data 

sst <- ggplot(data = sstcomp, aes(x = date_time,
                y = degree_Celsius,
                color=location,
                group = factor(location)))+
  geom_line(linewidth = 0.5,
            aes(alpha = location)) +
  scale_alpha_manual(values = c(1,0.4,0.05))+
  scale_y_continuous(breaks = seq(10,28,2), lim = c(8,28))+
  labs(x = "Date",
       y="Temperature (degrees °C)", 
       title = "Dana Point vs. Oceanside vs. Scripps SST", 
       color = "location")+
  scale_color_manual(values = c("brown4","aquamarine1", "lavenderblush2"))+
  theme_bw()+
  theme(legend.title=element_text(colour="black", size=14), 
        axis.text.x=element_text(face="bold", color="black", size=16),
        axis.text.y=element_text(face="bold", color="black", size=13),
        axis.title.x = element_text(color="black", size=18, face="bold"), 
        axis.title.y = element_text(color="black", size=18, face="bold"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())
  sst
  

  #save the plot
  # ggsave("danapt_oceanside_scripps_sst.png", plot = sst, path = "Plots")
  
  
  
###################################################################################
########--------- analysis of time series: Dana pt & Oceanside ---------###########
###################################################################################
  
  #combine both dataset for analysis & temp is sumarized by date
  sst_dana_ocean <- rbind(danapt_sstf,oceanside_sstf) %>% 
    # mutate( "location" = as.factor(location),                 #no need for date_time as we will average by date anyways
    # "date_time" = paste(date,time, sep = " "),
    mutate(date = ymd(date)) %>% 
    mutate("temp" = degree_Celsius,
           location = as.factor(location),
           date = as.factor(date)) %>% 
    select("location", "date", "temp") %>% 
    group_by(date,location) %>% summarise(temp = mean(temp)) %>%         #summarize data by averaging by location by date
    spread(location,temp) %>%                                                 #make into wide format
    na.omit()
  
  head(sst_dana_ocean)


########--------normal regression by date - does not work doesnt meet assumption as the residuals are correlated to time-------#######
# mod2 <- lm(danapt ~ oceanside, data = sst_dana_ocean)
# anova(mod2)
# summary(mod2)
# library(car)
# qqp(mod2)
# 
# par(mfrow = c(2,2))
# plot(mod2)
# 

########---------Test stationarity of the time series---------########
library(tseries)

adf.test(sst_dana_ocean$danapt)
adf.test(sst_dana_ocean$oceanside)
#both data are stationary since <0.05, can be used for arutoregressive model
#IF they were non stationary, would need to differencing to make stationary


# #####Granger Causality test ######- test to see if one timeseries can forecast another
# #------This test for if one causes another though there are no causality, so cross correlation might be better instead
# #INFO here: https://www.r-bloggers.com/2021/11/granger-causality-test-in-r-with-example/
# library(lmtest)
# grangertest(oceanside ~ danapt, order = 1, data = sst_dana_ocean)
# #p<0.0001, suggest that oceanside SST is useful in predicting future danapt SST
# #Null hypothesis for this test:
#   # x-values do not explain the variation in y

######--------Cross-Correlation - gives lag between time series -------############
#These figures since consistently high acf values and no dropoff suggest autocorrelation 
#needs prewhitening to help address this
ccf(sst_dana_ocean$date,sst_dana_ocean$danapt, type = "correlation")
ccf(sst_dana_ocean$date,sst_dana_ocean$oceanside, type = "correlation")


########--------------CROSS-CORRELATION--------------##########
#info here:   https://nicolarighetti.github.io/Time-Series-Analysis-With-R/correlations-and-arima.html#cross-correlation
# and here:   https://link.springer.com/article/10.3758/s13428-015-0611-2#Sec11
#accounting for auto correlation when prewhitening

# fit an ARIMA model - with oceanside as the x - predictor variable
ocean <- auto.arima(sst_dana_ocean$oceanside)
summary(ocean) #uses ARIMA(2,1,4) for (p,d,q)
# keep the residuals ("white noise")
ocean_residuals <- ocean$residuals

# fit the same ARIMA model to the Y-series (pre-whitening process here)
# by using the "Arima" function in forecast
dana <- Arima(sst_dana_ocean$danapt, model=ocean)
summary(dana)
# keep the residuals
dana_filtered <- residuals(dana)

# apply the ccf to the residuals 
ccf(ocean_residuals, dana_filtered)
print(ccf(ocean_residuals, dana_filtered, lag.max =30))
#CCF provided plot shows a big spike, meaning there is a small time lag (days) between two time series
#In this case, suggest the two locations are highly correlated w/lag of 5 days

#test to check prewhitened results and predictive significance of lag in predicting
library(lmtest)
grangertest(ocean_residuals, dana_filtered, order = 10)

######--------ARIMA MODEL - TEST AUTOCORRELATION (within one time series), NOT CROSS-CORRELATION------############

# library(fpp2)
#ARIMA model (Autoregressive integrated moving averages): type of regression model independent variable are lags of dependent variable and/or error
#Autoregressive model= assumption that past value has effect on current values
#essentially, this model includes correction for autocorrelated(?) errors
#ARIMA model errors are assumed to be white noise

#differencing = between time series makes non-stationary time series stationary by
#differences between consecutive observations - Look at ACF plots

# mod3 <- auto.arima(sst_dana_ocean[,"danapt"], approximation = FALSE, stepwise = FALSE)
# summary(mod3)
# 
# #dynamic regression model (oceanside as predictor) w/ ARIMA in error term
# mod4 <- auto.arima(sst_dana_ocean$danapt, xreg = sst_dana_ocean$oceanside)
# summary(mod4)
# 
# #Estimates of both regression error and ARIMA error 
# cbind("Regression Errors" = residuals(mod4, type = "regression"),
#       "ARIMA errors" = residuals(mod4, type = "innovation")) %>% 
#   autoplot(facets = TRUE)
# 
# #check residuals fit model
# checkresiduals(mod4) 
# #p<0.05, so time series residual error are not white noise and are dependent 
# #residuals not independently distributed, and are auto-correlated - may have forecasting problem
# 
# #visualize time series residuals
# ggtsdisplay(residuals(mod4, type = "regression"), main = "regression errors")
# ggtsdisplay(residuals(mod4, type = "response"), main = "ARIMA errors")
# 





