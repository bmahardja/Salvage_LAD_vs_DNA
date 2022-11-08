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
  
  df <- lapply(startingForm$fields$year$options, function(x) {
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
salvage_data <- suppressWarnings(pull_salvage())

########## Prep salvage data

#Rename columns to make it easier to work in R and divide Loss + Expanded Salvage by nfish
salvage_data_adjusted<- salvage_data %>%
  rename(SampleTime='Sample Time',LAD_Race='LAD Race',SampleFraction='Sample Fraction',ExpandedSalvage='Expanded Salvage',LAD_Loss='LAD Loss') %>%
  mutate(ExpandedSalvage=ExpandedSalvage/nfish, LAD_Loss=LAD_Loss/nfish)

test<-salvage_data_adjusted %>% filter(LAD_Race=="Winter") %>% mutate(Month=month(SampleTime))
test11<-test %>% filter(Month==11)

test<-salvage_data_adjusted %>% filter(LAD_Race=="Spring") %>% mutate(Month=month(SampleTime))
test11<-test %>% filter(Month==11)

test_nov<-salvage_data_adjusted %>% mutate(Month=month(SampleTime)) %>% filter(Month==11)


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


##############
#Cumulative Loss
salvage_data_cum<-salvage_data_adjusted %>% filter(as.Date(SampleTime)>"2020-02-18") %>% filter(LAD_Race=="Winter")

sum(salvage_data_cum$LAD_Loss)
