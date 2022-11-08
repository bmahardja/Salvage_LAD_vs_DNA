library(sharpshootR)
library(tidyverse)
library(lubridate)
library(grid)
library(gridExtra)

years<-c(2010:2021)



#Mossdale based on F  
data_list<-list()
for (i in years){
  data_list[[i]]<-CDECquery(id='MSD', sensor=25, interval='D', start=paste(i,'-06-01',sep=""), end=paste(i,'-09-30',sep="")) %>%
    filter(value>71.6)
  }

Mossdale_F<-do.call("rbind",data_list)
Mossdale_F_edit<-Mossdale_F %>% group_by(water_year) %>% top_n(-7,datetime) %>% mutate(N=1) %>% group_by(water_year) %>%
  summarise(firstdate=max(datetime),N_check=sum(N))


#Mossdale based on C  
years<-c(2012:2021)
data_list<-list()
for (i in years){
  data_list[[i]]<-CDECquery(id='MSD', sensor=146, interval='D', start=paste(i,'-06-01',sep=""), end=paste(i,'-09-30',sep="")) %>%
    filter(value>22.2)
}

Mossdale_C<-do.call("rbind",data_list)

Mossdale_C_edit<-Mossdale_C %>% group_by(water_year) %>% top_n(-7,datetime) %>% mutate(N=1) %>% group_by(water_year) %>%
  summarise(firstdate=max(datetime),N_check=sum(N))



#Prisoners Point based on C  
years<-c(2020:2022)
data_list<-list()
for (i in years){
  data_list[[i]]<-CDECquery(id='PPT', sensor=146, interval='D', start=paste(i,'-06-01',sep=""), end=paste(i,'-09-30',sep="")) %>%
    filter(value>22.2)
}

Prisoners_C<-do.call("rbind",data_list)

Prisoners_C_edit<-Prisoners_C %>% group_by(water_year) %>% top_n(-7,datetime) %>% mutate(N=1) %>% group_by(water_year) %>%
  summarise(firstdate=max(datetime),N_check=sum(N))


#Write out as CSV
write.csv(Mossdale_F_edit,file="Mossdale_71.6F.csv")
write.csv(Mossdale_C_edit,file="Mossdale_22.2C.csv")
write.csv(Prisoners_C_edit,file="PrisonersPt_22.2C.csv")
