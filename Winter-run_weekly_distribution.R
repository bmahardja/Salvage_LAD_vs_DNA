library(tidyverse)
library(rvest)
library(lubridate)
library(splitstackshape)
library(data.table)
##
#Tables for ReROC - understanding distribution of winter-run in the Delta

#Load unchecked DNA data post-2010
data_genetic_unchecked <- readRDS(file = file.path("output/post2010_DNA_data_unchecked.rds")) %>% mutate(Wateryear=ifelse(month(SampleTime)>9,year(SampleTime)+1,year(SampleTime)), Date=as.Date(SampleTime)) %>%
  filter(GeneticID=="Winter")

#Load Sac trawl data from EDI
#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.1054.1
data_sactrawl <- read_csv(file = file.path("data_input/LSR-sizebydate-2017-21.csv")) %>%
  filter(GeneticID=="Winter") %>%
  mutate(Wateryear=ifelse(month(SampleDate)>9,year(SampleDate)+1,year(SampleDate)), Date=as.Date(SampleDate))

#Load Chipps Island trawl data from EDI
#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.1055.1
data_chipps <- read_csv(file = file.path("data_input/DLC-sizebydate-2017-21.csv")) %>%
  filter(GeneticID=="Winter") %>%
  mutate(Wateryear=ifelse(month(SampleDate)>9,year(SampleDate)+1,year(SampleDate)), Date=as.Date(SampleDate))

####Salvage data
data_genetic_unchecked$week_number<-strftime(data_genetic_unchecked$Date, format = "%V")

week_dist_salvage_unchecked<-data_genetic_unchecked %>% group_by(week_number) %>% mutate(Count=1) %>% 
  summarise(percentage=round(sum(Count)/nrow(data_genetic_unchecked)*100,digits=2), Count=sum(Count))
sum(week_dist_salvage_unchecked$percentage)

write.csv(week_dist_salvage_unchecked,file.path("output/Weekly_distribution_salvagefacilities_unchecked.csv"),row.names = F)

data_chipps$week_number<-strftime(data_chipps$Date, format = "%V")

week_dist_chipps_unchecked<-data_chipps %>% group_by(week_number) %>% mutate(Count=1) %>% 
  summarise(percentage=round(sum(Count)/nrow(data_chipps)*100,digits=2), Count=sum(Count))
sum(week_dist_chipps_unchecked$percentage)

write.csv(week_dist_chipps_unchecked,file.path("output/Weekly_distribution_chipps_unchecked.csv"),row.names = F)

data_sactrawl$week_number<-strftime(data_sactrawl$Date, format = "%V")

week_dist_sactrawl_unchecked<-data_sactrawl %>% group_by(week_number) %>% mutate(Count=1) %>% 
  summarise(percentage=round(sum(Count)/nrow(data_sactrawl)*100,digits=2), Count=sum(Count))
sum(week_dist_sactrawl_unchecked$percentage)

write.csv(week_dist_sactrawl_unchecked,file.path("output/Weekly_distribution_sactrawl_unchecked.csv"),row.names = F)


