library(tidyverse)
library(rvest)
library(lubridate)
library(splitstackshape)
library(data.table)
library(readxl)

##################### Load Salvage Count Data from SacPAS

#Function adjusted from Trinh's code to pull salvage datasets from SacPAS
pull_salvage <- function(salvageURL = "http://www.cbr.washington.edu/sacramento/data/query_loss_detail.html") {
  startingSession <- session(salvageURL)
  startingForm <- html_form(startingSession)[[1]]
  
  df <- lapply(c(1993:2022), function(x) {
    filledForm <- set_values(startingForm,
                             year = x,
                             species = "1:f")
    
    submittedFormURL <- suppressMessages(submit_form(session = startingSession, 
                                                     form = filledForm, POST = salvageURL)$url)
    
    csvLink <- submittedFormURL
    
    if (length(csvLink) == 0) {
      return(NULL)
    } else {
      csvDownload <- csvLink
    }
    
    df <- csvDownload %>% 
      read_csv() %>% filter(!is.na(nfish)) }) %>%
    bind_rows() 
  df
}

#Run actual function to load data
#salvage_data <- suppressWarnings(pull_salvage())

# Save  dataset to a file
saveRDS(salvage_data, file = file.path("output/SalvageData_preloaded.rds"))

# Load pre-loaded salvage data
salvage_data <- readRDS(file = file.path("output/SalvageData_preloaded.rds"))

########## Prep salvage data

#Rename columns to make it easier to work in R and divide Loss + Expanded Salvage by nfish
salvage_data_adjusted<- salvage_data %>%
  rename(SampleTime='Sample Time',LAD_Race='LAD Race',SampleFraction='Sample Fraction',ExpandedSalvage='Expanded Salvage',LAD_Loss='LAD Loss') %>%
  mutate(ExpandedSalvage=ExpandedSalvage/nfish, LAD_Loss=LAD_Loss/nfish)


#Multiply rows by nfish
salvage_data_adjusted<- setDT(expandRows(salvage_data_adjusted, "nfish")) 

salvage_data_adjusted<- salvage_data_adjusted%>%
  # build grouping by combination of variables
  dplyr::group_by(SampleTime, LAD_Race, Length) %>%
  # add row number which works per group due to prior grouping
  dplyr::mutate(duplicateID = dplyr::row_number()) %>%
  # ungroup to prevent unexpected behaviour down stream
  dplyr::ungroup()

#Adjust Sample Time to the proper format
salvage_data_adjusted$SampleTime <- as.POSIXlt(salvage_data_adjusted$SampleTime,format='%Y-%m-%d  %H:%M:%S', tz = "UTC")

#### Read Kevin Reece's Dec 2022 data

genetic_data<-read_excel(file.path("data_input/OMR Discussions_Genetic Data_Nov 2022.xlsx"), sheet = "11-22 WR Salvage") %>% filter(!is.na(Group)) %>%
  rename(SampleTime='Sample Date/Time',Length=Forklength,Facility=Loc,Salvage_KevinReece=Salvage,Loss_KevinReece=Loss,LAD_KevinReece='Model Race',GeneticAssignment=Assignment) %>% select(-Catch,-'Sample Date')

genetic_data$SampleTime <- as.POSIXlt(genetic_data$SampleTime,format='%Y-%m-%d %H:%M', tz = "UTC")


genetic_data_expanded<- genetic_data %>%
  # build grouping by combination of variables
  dplyr::group_by(SampleTime, Facility, Length) %>%
  # add row number which works per group due to prior grouping
  dplyr::mutate(duplicateID = dplyr::row_number()) %>%
  # ungroup to prevent unexpected behaviour down stream
  dplyr::ungroup()



##################### Combine the salvage and genetic data sets
combined_data_v1<-left_join(salvage_data_adjusted,genetic_data_expanded)

# About 16% of the data are not matched up to the salvage database
unpaired_genetic_data<-full_join(salvage_data_adjusted,genetic_data_expanded) %>% filter(is.na(ExpandedSalvage))
count(unpaired_genetic_data)/count(genetic_data_expanded)

#Print out csv
write.csv(unpaired_genetic_data,file=file.path("output/Unpaired_Genetic_data_2022-12-13_From_NovemberKevinReeceData.csv"),row.names = F)


############# Filter just the paired genetic data
paired_genetic_data <- combined_data_v1 %>% filter(!is.na(GeneticAssignment))


#Print out csv
write.csv(paired_genetic_data,file=file.path("output/Paired_Genetic_data_2022-12-13_From_NovemberKevinReeceData.csv"),row.names = F)
