##########################################################
#von Bertalanffy Growth curve, CV, and mortality
#Jonathan Huang - Dec 2022
##########################################################

#procedure of fitting to von bertalanffy described in:
#     Ogle (2018) Introductory fisheries analyses with R
# OR here: http://derekogle.com/fishR/2019-12-31-ggplot-vonB-fitPlot-1

#Load libraries
library(tidyverse)
library(here)
library(FSA)
library(magrittr)
library(nlstools) #nls - nonlinear 
library(FSAmisc)
library(FSAdata)
library(stats)
library(car) #for boot
library(performance)

# add function: 
#----------VBGF function - two ways-------
vb <- function(Age, Linf, K, t0) Linf*(1-exp(-K*(Age-t0)))
# vb <- vbFuns(param = "vonBertalanffy") #

vb(0.1,244.7046,0.2606294,-1.961704) #test

#add label to graph
makeVBEqnLabel <- function(fit,digits=c(1,3,3)) {
  # Isolate coefficients (and control decimals)
  cfs <- coef(fit)
  Linf <- formatC(cfs[["Linf"]],format="f",digits=digits[1])
  K <- formatC(cfs[["K"]],format="f",digits=digits[2])
  # Handle t0 differently because of minus in the equation
  t0 <- cfs[["t0"]]
  sgn <- ifelse(t0<0,"+","-")
  t0 <- formatC(abs(t0),format="f",digits=digits[3])
  # Put together and return
  paste0("TL=='",Linf,"'~bgroup('[',1-e^{-'",K,"'~(age",sgn,"'",t0,"')},']')")
}

#----------Natural mortality function (Then et al 2015)------
#using von bert
mort <- function(K,Linf) (4.118*(K^0.73)*(Linf^-0.33)) #Length in cm

#using tmax (maximum age)
morttmax <- function(tmax) 4.899*(tmax^-0.916)
morttmax(28)

#----------vb coefficient------
vbcoef <- function(data,fit){
sv <- vbStarts(TL~Age, data = data)
fit <- nls(TL~vb(Age,Linf,K,t0), 
                data = data,  
                start = sv)
}

####---------load data--------
chpu <- read.csv(here::here("data","otolith_data","CHPU_otolith_data_2022.csv")) %>% 
  rename(TL = TL..mm.) %>% 
  select(c(Species, Site, Age,TL)) %>% 
  mutate(TL = TL*0.1) %>% 
  drop_na(Age)
oxca <- read.csv(here::here("data","otolith_data","OXCA_otolith_data_2022.csv"))%>% 
  mutate(FL..mm. = if_else(FL..mm. ==".",NA,NA),
         FL..mm. = as.numeric(FL..mm.))%>% 
  rename(TL = TL..mm.) %>% 
  select(c(Species, Site, Age,TL)) %>% 
  mutate(TL = TL*0.1) %>% 
  drop_na(Age)
emja <- read.csv(here::here("data","otolith_data","EMJA_otolith_data_2022.csv"))%>% 
  mutate(FL..mm. = if_else(FL..mm. ==".",NA,NA),
         FL..mm. = as.numeric(FL..mm.))%>% 
  rename(TL = TL..mm.) %>% 
  select(c(Species, Site, Age,TL)) %>% 
  mutate(TL = TL*0.1) %>% 
  drop_na(Age)
pacl <- read.csv(here::here("data","otolith_data","PACL_otolith_data_2022.csv"))%>% 
  rename(TL = TL..mm.) %>% 
  select(c(Species, Site, Age,TL)) %>% 
  mutate(TL = TL*0.1) %>% 
  drop_na()
sepu <- read.csv(here::here("data","otolith_data","SEPU_otolith_data_2022.csv"))%>% 
  mutate(FL..mm. = if_else(FL..mm. ==".",NA,NA),
         FL..mm. = as.numeric(FL..mm.))%>% 
  rename(TL = TL..mm.) %>% 
  select(c(Species, Site, Age,TL)) %>% 
  mutate(TL = TL*0.1) %>% 
  drop_na()

#combine two files
oto <- bind_rows(chpu,oxca,pacl,emja,sepu) %>% 
  select(c(Species, Site, Age,TL)) 


#summarize data - check distribution of age/size 
tal <- oto %>% 
  group_by(Age,Species) %>% 
  tally() %>% 
  filter(Species %in% c("PACL"))



####---------VBGF CHPU--------
#basic fitting of data to get reasonable starting values
# using svStarts in the "FSA" package to get starting values
svchpu <- vbStarts(TL~Age, data = chpu)
svchpu

# Model fitting 
#test model make sure it works - cool it works
vb(3, Linf = 1200,K =0.13, t0=-2.0)

#paramater estimate of VBGF
fit_chpu <- nls(TL~vb(Age,Linf,K,t0), #use nls for parameter parametization 
           data = chpu,  #use chpu age and lenght data
           start = svchpu) #startign with values from vbStart function

coef(fit_chpu) #get the coefficients

#confidence interval from a bootstrap method
confint(
  nlsBoot(fit_chpu)
)

#summary of model
summary(fit_chpu, correlation = TRUE)
#variation aroudn the model (resid std err) = 11.78

#check assumptions
residPlot(fit_chpu)

#get mortality from VBGF
coef(fit_chpu)
mort(0.368,22.11) #0.71 natural mortality rate
mort(0.36,21.8)

morttmax(9) # chpu max from this study - 0.65

#canned function in FSA package - same as mort function
metaM(method = "PaulyLNoT", Linf = 22.11, K = .368)

#cv - cv by age
chpu_age <- data_frame(Age = as.factor(c(1:9))) #make a df with all ages
chpu_avg <- chpu %>%  #estimate mean and sd of each age group
  summarise(mean = mean(TL),
            sd = sd(TL),
            .by = Age) %>% 
  mutate(Age = as.factor(Age))
chpu_cv <- full_join(chpu_age,chpu_avg) %>% 
  mutate(cv = sd/mean) #order by age in numerical order and calculate cv of each age
#cv range between 0.04 - 0.1 for different ages
a <- chpu_cv %>% drop_na(cv) 
mean(a$cv)
#mean cv = 0.067


############# NEXT: Graph plot

#Graph 
#basic plot w/ a vb fit 
ggplot(data=chpu,aes(x=Age,y=TL))+
  geom_point(size=2,alpha=0.1) +
  scale_y_continuous(name="Total Length (cm)",
                     limits=c(0,30)) +
  scale_x_continuous(name="Age (years)",
                     limits = c(0,9),
                     expand = c(0,0), #change view to the plot to just age 0-9
                     breaks = seq(0,10,1)) +
  geom_smooth(method="nls",se=FALSE,
              method.args=list(formula=y~vb(x,Linf,K,t0),start=svchpu),
              color="black",linewidth=1) +
  theme(panel.grid.minor.x=element_blank())

#predictions to exted the graph outside 
ages <- seq(-2,11, length.out = 312) # make sequence to predict outside data we have 

#trying using the predict function
predict(fit_chpu,data.frame(Age=2:7)) #in the dataframe, "Age" need to match how it is spelled in the fish dataframe

predictfish <- function(x) predict(x,data.frame(Age=ages))
predictfish(fit_chpu)

#predict with bootstraping of data
fit_boot_chpu <- Boot(fit_chpu, f = predictfish, method = "case")
pred_boot <- data.frame(ages,
                        predict(fit_chpu, #use fit fomula
                                data.frame(Age = ages)), #make data frame with age as column same as ages
                        confint(fit_boot_chpu))
  names(pred_boot) <- c("age","fit","low_CI","up_CI")
headtail(pred_boot)

#filter out to have biologically reasonable data
pred_chpu <- pred_boot %>% 
  filter(age>=min(chpu$Age),age<=max(chpu$Age))

# Make dataframe with mean vb model and ages, no bootstrapping
mean_vb_chpu <- data_frame(ages,
                           len = vb(ages,22.11619,0.3684, -1.4496)) %>%  #vb parameter for chpu
  filter(ages>=-2,ages<=10)

# Plot with simulated value
ggplot() + 
  geom_ribbon(data=pred_chpu,aes(x=age,ymin=low_CI,ymax=up_CI),fill="gray80")+
  geom_point(chpu, mapping= aes(x = Age, y = TL))+
  geom_line(mean_vb_chpu, mapping = aes(x = ages, y = len),
            linewidth = 1.0, linetype = "dashed")+
  scale_y_continuous(breaks = seq(0,25,5), limits = c(0,25))+
  scale_x_continuous(breaks = seq(-2,9,1),limits = c(-1,9))+
  annotate(geom="text",label=makeVBEqnLabel(fit_chpu),parse=TRUE,
           size=10,x=Inf,y=-Inf,hjust=1.1,vjust=-0.5)+
  labs(y = "TL (cm)",
       y = "Age",
       title = "CHPU length at age")+
  theme(plot.title = element_text(hjust = 0.1,size = 15))
# ggsave(here("output","CHPU_leng_age.png"),
#        width = 11, height = 9)





####---------VBGF OXCA-----------------------------------------------
#basic fitting of data to get reasonable starting values
# using svStarts in the "FSA" package to get starting values
svoxca <- vbStarts(TL~Age, data = oxca)
svoxca

# Model fitting 
#test model make sure it works - cool it works
vb(3, Linf = 1200,K =0.13, t0=-2.0)

#paramater estimate of VBGF
fit_oxca <- nls(TL~vb(Age,Linf,K,t0), #use nls for parameter parametization 
                data = oxca,  #use chpu age and lenght data
                start = svoxca) #startign with values from vbStart function

coef(fit_oxca) #get the coefficients
#test 
vb(-1.9,224.55,0.56,-0.6)

#confidence interval from a bootstrap method
confint(
  nlsBoot(fit_oxca)
)

#summary of model
summary(fit_oxca, correlation = TRUE)
#variation aroudn the model (resid std err) = 11.78

#check assumptions
residPlot(fit_oxca)

#get mortality from VBGF
coef(fit_oxca)
mort(0.564,22.45) #0.97 natural mortality rate?!?

morttmax(8) #max age from this study - 0.72

#canned function in FSA package - same as mort function
metaM(method = "PaulyLNoT", Linf = 22.45, K = 0.564)

#cv - cv by age
oxca_age <- data_frame(Age = as.factor(c(1:9)))
oxca_avg <- oxca %>% 
  summarise(mean = mean(TL),
            sd = sd(TL),
            .by = Age) %>% 
  mutate(Age = as.factor(Age))
oxca_cv <- full_join(oxca_age,oxca_avg) %>% 
  mutate(cv = sd/mean)
#cv range between 0.04 - 0.7 for different ages
a <- oxca_cv %>% drop_na(cv) 
mean(a$cv)
#mean cv = 0.061



############# NEXT: Graph plot
#Graph 
#basic plot w/ a vb fit 
ggplot(data=oxca,aes(x=Age,y=TL))+
  geom_point(size=2,alpha=0.1) +
  scale_y_continuous(name="Total Length (mm)",limits=c(0,300)) +
  scale_x_continuous(name="Age (years)",breaks = 0:10) +
  geom_smooth(method="nls",se=FALSE,
              method.args=list(formula=y~vb(x,Linf,K,t0),start=svoxca),
              color="black",linewidth=1) +
  theme(panel.grid.minor.x=element_blank())

#predictions to exted the graph outside 
ages <- seq(-2,11, length.out = 375) # make sequence to predict outside data we have 

#trying using the predict function
predictfish <- function(x) predict(x,data.frame(Age=ages),type ="class") #specify "Age" to the raw data column
predictfish(fit_oxca)

#predict with bootstraping of data 
fit_boot_oxca <- Boot(fit_oxca, f = predictfish, method = "case")
pred_oxca <- data.frame(ages,
                        predict(fit_oxca, 
                                data.frame(Age = ages)),
                        confint(fit_boot_oxca))
  names(pred_oxca) <- c("age","fit","low_CI","up_CI")
headtail(pred_oxca)

#filter out to have biologically reasonable data
pred_oxca <- pred_oxca %>% 
  filter(age>=1,age<=9)

# Make dataframe with mean vb model and ages, no bootstrapping
mean_vb_oxca <- data_frame(ages,
                   len = vb(ages,22.455,0.56, -0.602)) %>% 
  filter(ages>=-1,ages<=max(oxca$Age))

# Plot with simulated value
ggplot() + 
  geom_ribbon(data=pred_oxca,aes(x=age,ymin=low_CI,ymax=up_CI),fill="gray80")+
  geom_point(oxca, mapping= aes(x = Age, y = TL))+
  geom_line(mean_vb_oxca, mapping = aes(x = ages, y = len),
            linewidth = 1.0, linetype = "dashed")+
  scale_y_continuous(breaks = seq(0,25,5), limits = c(0,25))+
  scale_x_continuous(breaks = seq(-2,9,1),limits = c(-1,9))+
  annotate(geom="text",label=makeVBEqnLabel(fit_oxca),parse=TRUE,
           size=10,x=Inf,y=-Inf,hjust=1.1,vjust=-0.5)+
  labs(y = "TL (cm)",
       y = "Age",
       title = "OXCA length at age")+
  theme(plot.title = element_text(hjust = 0.1,size = 15))
# ggsave(here("output","OXCA_leng_age.png"),
#        width = 11, height = 9)


####---------VBGF EMJA-----------------------------------------------
#basic fitting of data to get reasonable starting values
# using svStarts in the "FSA" package to get starting values
svemja <- vbStarts(TL~Age, data = emja)
svemja

# Model fitting 
#test model make sure it works - cool it works
vb(3, Linf = 1200,K =0.13, t0=-2.0)

#paramater estimate of VBGF
fit_emja <- nls(TL~vb(Age,Linf,K,t0), #use nls for parameter parametization 
                data = emja,  #use chpu age and lenght data
                start = svemja) #startign with values from vbStart function

coef(fit_emja) #get the coefficients
#test 
vb(-1.9,224.55,0.56,-0.6)

#confidence interval from a bootstrap method
confint(
  nlsBoot(fit_emja)
)

#summary of model
summary(fit_emja, correlation = TRUE)
#variation aroudn the model (resid std err) = 11.78

#check assumptions
residPlot(fit_emja)

#get mortality from VBGF
coef(fit_emja)
#Use max age as suggested by Then et al. 2015, max age of 10 - fishbase.com
morttmax(10)
#0.59 mortality rate

#cv - cv by age
emja_age <- data_frame(Age = as.factor(c(0:5)))
emja_avg <- emja %>% 
  summarise(mean = mean(TL),
            sd = sd(TL),
            .by = Age) %>% 
  mutate(Age = as.factor(Age))
emja_cv <- full_join(emja_age,emja_avg) %>% 
  mutate(cv = sd/mean)
#cv range between 0.04 - 0.7 for different ages
a <- emja_cv %>% drop_na(cv) 
mean(a$cv)
#mean cv = 0.057



############# NEXT: Graph plot
#Graph 
#basic plot w/ a vb fit 
ggplot(data=emja,aes(x=Age,y=TL))+
  geom_point(size=2,alpha=0.1) +
  scale_y_continuous(name="Total Length (mm)",limits=c(0,30)) +
  scale_x_continuous(name="Age (years)",breaks = 0:10) +
  geom_smooth(method="nls",se=FALSE,
              method.args=list(formula=y~vb(x,Linf,K,t0),start=svemja),
              color="black",linewidth=1) +
  theme(panel.grid.minor.x=element_blank())

#predictions to exted the graph outside 
ages <- seq(-2,8, length.out = 326) # make sequence to predict outside data we have 

#trying using the predict function
predictfish <- function(x) predict(x,data.frame(Age=ages),type ="class") #specify "Age" to the raw data column
predictfish(fit_emja)

#predict with bootstraping of data 
fit_boot_emja <- Boot(fit_emja, f = predictfish, method = "case")
pred_emja <- data.frame(ages,
                        predict(fit_emja, 
                                data.frame(Age = ages)),
                        confint(fit_boot_emja))
names(pred_emja) <- c("age","fit","low_CI","up_CI")
headtail(pred_emja)

#filter out to have biologically reasonable data
pred_emja <- pred_emja %>% 
  filter(age>=0,age<=6)

# Make dataframe with mean vb model and ages, no bootstrapping
mean_vb_emja <- data_frame(ages,
                           len = vb(ages,23.33,0.7959, -1.03)) %>% 
  filter(ages>=-1,ages<=max(emja$Age))

# Plot with simulated value
ggplot() + 
  geom_ribbon(data=pred_emja,aes(x=age,ymin=low_CI,ymax=up_CI),fill="gray80")+
  geom_point(emja, mapping= aes(x = Age, y = TL, color = Site), alpha = 0.2)+
  geom_line(mean_vb_emja, mapping = aes(x = ages, y = len),
            linewidth = 1.0, linetype = "dashed")+
  scale_y_continuous(breaks = seq(0,25,5), limits = c(0,25))+
  scale_x_continuous(breaks = seq(-2,9,1),limits = c(-1,6))


####---------VBGF PACL---------------------
svpacl <- vbStarts(TL~Age, data = pacl, methLinf = "oldAge")
svpacl


#paramater estimate of VBGF
fit_pacl <- nls(TL~vb(Age,Linf,K,t0), #use nls for parameter parametization 
                data = pacl,  #use chpu age and lenght data
                start = c(Linf = 45.3,K = 0.027,t0 = -24.6199)) #startign with values from vbStart function


coef(fit_pacl) #get the coefficients
#test 

#confidence interval from a bootstrap method
confint(
  nlsBoot(fit_pacl)
)

#summary of model
summary(fit_pacl, correlation = TRUE)
#variation aroudn the model (resid std err) = 11.78

#check assumptions
residPlot(fit_pacl)

ggplot(data = pacl, aes(x = Age, y = TL))+
  geom_point()+
  scale_x_continuous(limits = c(0,15))

############# NEXT: Graph plot
#Graph 
#basic plot w/ a vb fit 
ggplot(data=pacl,aes(x=Age,y=TL))+
  geom_point(size=2,alpha=0.1) +
  scale_y_continuous(name="Total Length (mm)",limits=c(0,50)) +
  scale_x_continuous(name="Age (years)",breaks = 0:17) +
  geom_smooth(method="nls",se=FALSE,
              method.args=list(formula=y~vb(x,Linf,K,t0),start=svpacl),
              color="black",linewidth=1) +
  theme(panel.grid.minor.x=element_blank())

#predictions to exted the graph outside 
ages <- seq(-2,11, length.out = 375) # make sequence to predict outside data we have 

#trying using the predict function
predictfish <- function(x) predict(x,data.frame(Age=ages),type ="class") #specify "Age" to the raw data column
predictfish(fit_oxca)

#predict with bootstraping of data 
fit_boot_oxca <- Boot(fit_oxca, f = predictfish, method = "case")
pred_oxca <- data.frame(ages,
                        predict(fit_oxca, 
                                data.frame(Age = ages)),
                        confint(fit_boot_oxca))
names(pred_oxca) <- c("age","fit","low_CI","up_CI")
headtail(pred_oxca)

#filter out to have biologically reasonable data
pred_oxca <- pred_oxca %>% 
  filter(age>=1,age<=9)

# Make dataframe with mean vb model and ages, no bootstrapping
mean_vb_oxca <- data_frame(ages,
                           len = vb(ages,22.455,0.56, -0.602)) %>% 
  filter(ages>=-1,ages<=max(oxca$Age))

# Plot with simulated value
ggplot() + 
  geom_ribbon(data=pred_oxca,aes(x=age,ymin=low_CI,ymax=up_CI),fill="gray80")+
  geom_point(oxca, mapping= aes(x = Age, y = TL))+
  geom_line(mean_vb_oxca, mapping = aes(x = ages, y = len),
            linewidth = 1.0, linetype = "dashed")+
  scale_y_continuous(breaks = seq(0,25,5), limits = c(0,25))+
  scale_x_continuous(breaks = seq(-2,9,1),limits = c(-1,9))






####---------VBGF SEPU---------------------
svsepu <- vbStarts(TL~Age, data = sepu)
svsepu

# Model fitting 
#paramater estimate of VBGF
fit_sepu <- nls(TL~vb(Age,Linf,K,t0), #use nls for parameter parametization 
                data = sepu,  #use chpu age and lenght data
                start = svsepu) #startign with values from vbStart function

coef(fit_sepu) #get the coefficients

#confidence interval from a bootstrap method
confint(
  nlsBoot(fit_sepu)
)

#summary of model
summary(fit_sepu, correlation = TRUE)
#variation aroudn the model (resid std err) = 11.78

#check assumptions
residPlot(fit_sepu)

#mortality
coef(fit_sepu)
mort(0.19,35.9) #0.37 natural mortality rate


sepu_age <- data_frame(Age = as.factor(c(1:9)))
sepu_avg <- sepu %>% 
  summarise(mean = mean(TL),
            sd = sd(TL),
            .by = Age) %>% 
  mutate(Age = as.factor(Age))
sepu_cv <- full_join(sepu_age,sepu_avg) %>% 
  mutate(cv = sd/mean)
#cv range between 0.04 - 0.7 for different ages
a <- sepu_cv %>% drop_na(cv) 
mean(a$cv)

############# NEXT: Graph plot
#Graph 
#basic plot w/ a vb fit 
ggplot(data=sepu,aes(x=Age,y=TL))+
  geom_point(size=2,alpha=0.1) +
  scale_y_continuous(name="Total Length (mm)",limits=c(0,50)) +
  scale_x_continuous(name="Age (years)",breaks = 0:17) +
  geom_smooth(method="nls",se=FALSE,
              method.args=list(formula=y~vb(x,Linf,K,t0),start=svsepu),
              color="black",linewidth=1) +
  theme(panel.grid.minor.x=element_blank())

#predictions to exted the graph outside 
ages <- seq(-2,14, length.out = 219) # make sequence to predict outside data we have 

#trying using the predict function
predictfish <- function(x) predict(x,data.frame(Age=ages),type ="class") #specify "Age" to the raw data column
predictfish(fit_sepu)

#predict with bootstraping of data 
fit_boot_sepu <- Boot(fit_sepu, f = predictfish, method = "case")
pred_sepu <- data.frame(ages,
                        predict(fit_sepu, 
                                data.frame(Age = ages)),
                        confint(fit_boot_sepu))
names(pred_sepu) <- c("age","fit","low_CI","up_CI")
headtail(pred_sepu)

#filter out to have biologically reasonable data
pred_sepu <- pred_sepu %>% 
  filter(age>=1,age<=12)

# Make dataframe with mean vb model and ages, no bootstrapping
mean_vb_sepu <- data_frame(ages,
                           len = vb(ages,35.91,0.19, -2.26)) %>% 
  filter(ages>=-1,ages<=max(sepu$Age))

# Plot with simulated value
ggplot() + 
  geom_ribbon(data=pred_sepu,aes(x=age,ymin=low_CI,ymax=up_CI),fill="gray80")+
  geom_point(sepu, mapping= aes(x = Age, y = TL))+
  geom_line(mean_vb_sepu, mapping = aes(x = ages, y = len),
            linewidth = 1.0, linetype = "dashed")+
  scale_y_continuous(breaks = seq(0,40,5), limits = c(0,40))+
  scale_x_continuous(breaks = seq(-2,12,1),limits = c(-1,12))



