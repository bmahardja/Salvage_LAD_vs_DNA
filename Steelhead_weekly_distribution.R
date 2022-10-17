library(tidyverse)
library(rvest)
library(lubridate)
library(splitstackshape)
library(data.table)
##
#Tables for ReROC - understanding distribution of steelhead in the Delta


#Acquire steelhead loss data

#Function adjusted from Trinh's code to pull salvage datasets from SacPAS
pull_salvage <- function(salvageURL = "http://www.cbr.washington.edu/sacramento/data/query_loss_detail.html") {
  startingSession <- session(salvageURL)
  startingForm <- html_form(startingSession)[[1]]
  
  df <- lapply(c(1993:2022), function(x) {
    filledForm <- set_values(startingForm,
                             year = x,
                             species = "2:f")
    
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
sth_salvage_data <- suppressWarnings(pull_salvage())


####Salvage data
sth_salvage_data$Date<-as.Date(sth_salvage_data$'Sample Time')

sth_salvage_data$week_number<-strftime(sth_salvage_data$Date, format = "%V")

#Use fish count (nfish) per Josh Israel's instructions
totalfish<-sum(sth_salvage_data$nfish)

week_dist_salvage<-sth_salvage_data %>% group_by(week_number) %>% 
  summarise(percentage=round(sum(nfish)/totalfish*100,digits=2), nfish=sum(nfish))

sum(week_dist_salvage$percentage)

write.csv(week_dist_salvage,file.path("output/Weekly_distribution_steelhead_salvagefacilities.csv"),row.names = F)

