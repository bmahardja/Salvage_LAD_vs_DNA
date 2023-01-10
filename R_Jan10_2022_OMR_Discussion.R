library(tidyverse)
library(rvest)
library(lubridate)
library(splitstackshape)
library(data.table)
library(readxl)

##################### Load Salvage Count Data from Vanessa Gusman

salvage_data <- read.csv(file = file.path("data_input/ChinookLoss_20221215.csv"))
salvage_data$SampleDateTime<-as.POSIXct(paste(salvage_data$Date, salvage_data$Time), format="%m/%d/%Y %H:%M:%S", tz="America/Los_Angeles")

#Keep data with non-existent date and time
salvage_data_wrong_time <- salvage_data %>% filter(is.na(SampleDateTime))

########## Prep salvage data
str(salvage_data)
#Rename columns to make it easier to work in R and divide Loss + Expanded Salvage by nfish
salvage_data_adjusted<- salvage_data %>% 
  dplyr::mutate(Facility =case_when(Facility==1 ~ "SWP",
                                    Facility==2 ~ "CVP")) %>%

  #Remove erroneous sample date and time
  filter(!is.na(SampleDateTime)) %>%
  #Remove single sample with NA count 
  filter(!is.na(Count)) %>%
  #Ensure that data for salvage and loss are for a single fish
  mutate(Salvage=Salvage/Count, Encounter=Encounter/Count, Entrain=Entrain/Count, Release=Release/Count, Loss=Loss/Count)


#Multiply rows by nfish
salvage_data_adjusted<- setDT(expandRows(salvage_data_adjusted, "Count")) 

salvage_data_adjusted<- salvage_data_adjusted%>%
  # build grouping by combination of variables
  dplyr::group_by(SampleDateTime, Race, FL) %>%
  # add row number which works per group due to prior grouping
  dplyr::mutate(duplicateID = dplyr::row_number()) %>%
  # ungroup to prevent unexpected behaviour down stream
  dplyr::ungroup() 

#### Read Kevin Reece's Jan 5 2023 data

genetic_data <-read_excel(file.path("data_input/Genetically Confirmed Juvenile WRCHN with Loss_CVP and SWP Salvage_WY1996_WY2022 for 2022 OMR Discussions 01052023.xlsx"), sheet = "Revised Data", range = cell_cols("A:L")) %>% 
  rename(SampleDateTime='Sample Date/Time',FL=Forklength,Facility=Loc,Salvage_KevinReece=Salvage,Loss_KevinReece=Loss,LAD_KevinReece='Model Race',GeneticAssignment=Assignment) %>% select(-Catch,-'Sample Time')


genetic_data_expanded<- genetic_data %>%
  #adjust time zone
  mutate(SampleDateTime=force_tz(SampleDateTime, tz="America/Los_Angeles")) %>%
  mutate(WaterYear=as.numeric(WaterYear)) %>%
  # build grouping by combination of variables
  dplyr::group_by(SampleDateTime, Facility, FL) %>%
  # add row number which works per group due to prior grouping
  dplyr::mutate(duplicateID = dplyr::row_number()) %>%
  # ungroup to prevent unexpected behaviour down stream
  dplyr::ungroup()

genetic_data_expanded$SampleDate<- as.Date(genetic_data_expanded$'Sample Date')
genetic_data_expanded$'Sample Date'<- NULL

str(genetic_data_expanded)

##################### Read JPE file
jpe_data <- read.csv(file = file.path("data_input/Winter-run_JPE.csv")) %>% rename(WaterYear=WY)

annual_salvage<-salvage_data_adjusted %>% group_by(WaterYear) %>% filter(Race=="W") %>% summarise(WinterRun_LAD_Loss=sum(Loss))

annual_salvage_genetic<-genetic_data_expanded %>% group_by(WaterYear) %>% summarise(WinterRun_Genetic_Loss=sum(Loss_KevinReece))
annual_salvage_genetic[is.na(annual_salvage_genetic)] <- 0


combined_data<-full_join(annual_salvage,annual_salvage_genetic) %>% left_join(jpe_data) %>% filter(WaterYear>=1996) %>%
  mutate_all(~replace(., is.na(.), 0)) %>% mutate(JuvenileProductionEstimate=as.numeric(JuvenileProductionEstimate)) %>%
  mutate(PercentJPE_genetic=WinterRun_Genetic_Loss/JuvenileProductionEstimate*100,PercentJPE_LAD=WinterRun_LAD_Loss/JuvenileProductionEstimate*100)


#Print out csv
write.csv(combined_data,file=file.path("output/Percent_JPE_draft_summary_2023-01-10.csv"),row.names = F)
