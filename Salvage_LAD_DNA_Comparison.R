library(tidyverse)
library(rvest)
library(lubridate)
library(splitstackshape)
library(data.table)

data <- readRDS(file = "Salvage_DNA_vs_LAD.rds") %>% mutate(Wateryear=ifelse(month(SampleTime)>9,year(SampleTime)+1,year(SampleTime))) %>%
  #There are no genetic winter-run left unpaired in 2015, 2017, 2018, 2019, and 2021 
  #Remove 2021 for now, since there's a single winter-run not assigned sample time by Scott
  filter(Wateryear %in% c("2015","2017","2018","2019"))

str(data)

winter_run_data <- data %>% mutate(Winter_Run_LAD_Expanded_Salvage = ifelse(LAD_Race=="Winter",ExpandedSalvage,0),
                                   Winter_Run_LAD_Loss = ifelse(LAD_Race=="Winter",LAD_Loss,0),
                                   Winter_Run_Genetic_Expanded_Salvage = ifelse(GeneticID=="Winter",ExpandedSalvage,0),
                                   Winter_Run_Genetic_Loss = ifelse(GeneticID=="Winter",LAD_Loss,0)) %>% 
  filter(GeneticID=="Winter"|LAD_Race=="Winter")

str(winter_run_data)

#Check LAD assignment
winter_run_data$Check<-winter_run_data$LAD_Race==winter_run_data$LengthByDate
#One of Scott's was incorrect (it is winter-run LAD)

drops <- c("SampleDate", "Julian", "LengthByDate","Check")

winter_run_data<-winter_run_data[ , !(names(winter_run_data) %in% drops)]
winter_run_data$Winter_Run_Genetic_Expanded_Salvage[is.na(winter_run_data$Winter_Run_Genetic_Expanded_Salvage)] <- 0
winter_run_data$Winter_Run_Genetic_Loss[is.na(winter_run_data$Winter_Run_Genetic_Loss)] <- 0


#Annual summary
winter_run_data_annual <- winter_run_data %>% group_by(Wateryear) %>% summarise(Winter_Run_LAD_Expanded_Salvage=sum(Winter_Run_LAD_Expanded_Salvage),
                                                                                Winter_Run_LAD_Loss=sum(Winter_Run_LAD_Loss),
                                                                                Winter_Run_Genetic_Expanded_Salvage=sum(Winter_Run_Genetic_Expanded_Salvage),
                                                                                Winter_Run_Genetic_Loss=sum(Winter_Run_Genetic_Loss))

#Print as csv
write.csv(winter_run_data,file="Winter_Run_LAD_vs_Genetic_comparison_daily_2022-02-23.csv",row.names = F)
write.csv(winter_run_data_annual,file="Winter_Run_LAD_vs_Genetic_comparison_annual_2022-02-23.csv",row.names = F)
