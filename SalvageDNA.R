library(tidyverse)
library(rvest)
library(lubridate)
library(splitstackshape)
library(data.table)

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


#################################### Read genetic data

genetic_data_CVP <-read.csv("CVP-sizebydate-2010-21.csv")
genetic_data_SWP <-read.csv("SWP-sizebydate-2010-21.csv")

genetic_data_combined<-bind_rows(genetic_data_CVP,genetic_data_SWP) %>% rename(SampleTime=SampleDate2)

genetic_data_combined$SampleTime <- as.POSIXlt(genetic_data_combined$SampleTime,format='%m/%d/%Y %H:%M', tz = "UTC")

genetic_data_combined<- genetic_data_combined%>%
  # add facility
  dplyr::mutate(Facility = case_when(
    grepl("CVP",ID) ~ "CVP",
    grepl("SWP",ID) ~ "SWP",
  )) %>%
  # build grouping by combination of variables
  dplyr::group_by(SampleTime, Facility, ForkLength) %>%
  # add row number which works per group due to prior grouping
  dplyr::mutate(duplicateID = dplyr::row_number()) %>%
  # ungroup to prevent unexpected behaviour down stream
  dplyr::ungroup() %>%
  dplyr::rename(Length=ForkLength)

# Isolate data with NA sample time
genetic_data_NA_sampletime<- genetic_data_combined %>% filter(is.na(SampleTime))
write.csv(genetic_data_NA_sampletime,file="Missingsampletime_2022-02-23.csv",row.names = F)


# Remove NA sample time from data frame
genetic_data_combined <- genetic_data_combined %>% filter(!is.na(SampleTime))

####### Combine the two data sets
combined_data_v1<-left_join(salvage_data_adjusted,genetic_data_combined)

# About 1/3 of the data are not matched up to the salvage database
unpaired_genetic_data<-full_join(salvage_data_adjusted,genetic_data_combined) %>% filter(is.na(LAD_Race))
count(unpaired_genetic_data)/count(genetic_data_combined)

######## Try moving up by 12 hours
# Add 12 hours from unpaired genetic data to see if that leads to more matches
unpaired_genetic_data_plus12<- unpaired_genetic_data
unpaired_genetic_data_plus12$SampleTime = unpaired_genetic_data_plus12$SampleTime + 12*60*60
drops <- c("Species", "Adipose Clip", "LAD_Race", "Count Duration (minutes)", "Pumping Duration (minutes)", "SampleFraction", "Study Type","ExpandedSalvage", "LAD_Loss")
unpaired_genetic_data_plus12<-unpaired_genetic_data_plus12[ , !(names(unpaired_genetic_data_plus12) %in% drops)]

### Recombine genetic datasets and rejoin
paired_genetic_data <-left_join(salvage_data_adjusted,genetic_data_combined) %>% filter(!is.na(GeneticID))
paired_genetic_data<-paired_genetic_data[ , !(names(paired_genetic_data) %in% drops)]
# Now unpaired data have been readjusted by 12 hours
genetic_data_recombined<-bind_rows(paired_genetic_data,unpaired_genetic_data_plus12)

#Add *fixed* dataset back to genetic dataset
combined_data_v2 <- left_join(salvage_data_adjusted,genetic_data_recombined)

unpaired_genetic_data_v2<-full_join(salvage_data_adjusted,genetic_data_recombined) %>% filter(is.na(LAD_Race))
unpaired_genetic_data_v2<-unpaired_genetic_data_v2[ , !(names(unpaired_genetic_data_v2) %in% drops)]
unpaired_genetic_data_v2$SampleTime<- unpaired_genetic_data_v2$SampleTime - 12*60*60

write.csv(unpaired_genetic_data,file="Unpaired_Genetic_data_2022-02-23.csv",row.names = F)

# Save combined dataset to a file
saveRDS(combined_data_v2, file = "Salvage_DNA_vs_LAD.rds")

#########################################################Not used

test<-inner_join(combined_data_v1,combined_data_v2)
test<-genetic_data_recombined %>% filter(is.na(SampleTime))





paired_genetic_data_v2<-combined_data %>% filter(!is.na(GeneticID))

unpaired_genetic_data_round2 <-left_join(genetic_data_combined_fixed,salvage_data_adjusted) %>% filter(is.na(LAD_Race))




write.csv(unpaired_genetic_data,file="Unpaired_Genetic_data_2022-02-18.csv",row.names = F)

write.csv(combined_data,file="Total_Salvage_data_2022-02-18.csv",row.names = F)
