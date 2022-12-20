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

#### Read Kevin Reece's Dec 19 2022, 2010 and one data

genetic_data_2010 <-read_excel(file.path("data_input/Genetically Confirmed Juvenile WRCHN with Loss_CVP and SWP Salvage_WY2011_WY2022 for 2022 OMR Discussions.xlsx"), sheet = "Sheet1", range = cell_cols("A:M")) %>% 
  filter(!is.na(Group)) %>%
  rename(SampleDateTime='Sample Date/Time',FL=Forklength,Facility=Loc,Salvage_KevinReece=Salvage,Loss_KevinReece=Loss,LAD_KevinReece='Model Race',GeneticAssignment=Assignment) %>% select(-Catch,-'Sample Date')


genetic_data_2010_expanded<- genetic_data_2010 %>%
  #adjust time zone
  mutate(SampleDateTime=force_tz(SampleDateTime, tz="America/Los_Angeles")) %>%
  mutate(WaterYear=as.numeric(WaterYear)) %>%
  # build grouping by combination of variables
  dplyr::group_by(SampleDateTime, Facility, FL) %>%
  # add row number which works per group due to prior grouping
  dplyr::mutate(duplicateID = dplyr::row_number()) %>%
  # ungroup to prevent unexpected behaviour down stream
  dplyr::ungroup()

##################### Combine the salvage and genetic data sets
combined_data_v1<-left_join(salvage_data_adjusted,genetic_data_2010_expanded)

str(salvage_data_adjusted)
str(genetic_data_2010_expanded)
unique(genetic_data_2010_expanded$Facility)
unique(salvage_data_adjusted$Facility)

# About 8.7% of the data are not matched up to the salvage database
unpaired_genetic_data<-full_join(salvage_data_adjusted,genetic_data_2010_expanded) %>% filter(is.na(Salvage))
count(unpaired_genetic_data)/count(genetic_data_2010_expanded)

#Print out csv
write.csv(unpaired_genetic_data,file=file.path("output/Unpaired_Genetic_data_2022-12-20_From_DecemberKevinReeceData.csv"),row.names = F)

############# Filter just the paired genetic data
paired_genetic_data <- combined_data_v1 %>% filter(!is.na(GeneticAssignment))


#Print out csv
write.csv(paired_genetic_data,file=file.path("output/Paired_Genetic_data_2022-12-20_From_DecemberKevinReeceData.csv"),row.names = F)

#Print out csv for erroneous time
write.csv(salvage_data_wrong_time,file=file.path("output/SalvageData_with_incorrect_datetime_2022-12-20.csv"),row.names = F)

#######
genetic_data_1996 <-read_excel(file.path("data_input/1996-2010 WRCHN Genetic Data CVP and SWP for 2022 OMR Discussions 12192022.xlsx"), sheet = "Sheet1")
