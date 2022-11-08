library(tidyverse)
library(rvest)
library(lubridate)
library(splitstackshape)
library(data.table)
library(deltafish)
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

#add year
sth_salvage_data$Year<-year(sth_salvage_data$Date)

#sum fish by year, facility, and week
week_dist_salvage<-sth_salvage_data %>% group_by(Year,Facility,week_number) %>% 
  summarise(nfish=sum(nfish),Loss=sum(Loss))

week_dist_salvage<- pivot_wider(week_dist_salvage,names_from = c(Facility),values_from = c(Loss, nfish)) 

#spread data
week_dist_salvage<-spread(week_dist_salvage,Facility,nfish,Loss)

#add zeroes
week_dist_salvage[is.na(week_dist_salvage)] <- 0

#Add cvp and Swp combined
week_dist_salvage$Loss_CVP_SWP<-week_dist_salvage$Loss_CVP+week_dist_salvage$Loss_SWP
week_dist_salvage$nfish_CVP_SWP<-week_dist_salvage$nfish_CVP+week_dist_salvage$nfish_SWP

#print out csv
write.csv(week_dist_salvage,file.path("output/Year_and_weekly_distribution_steelhead_salvagefacilities.csv"),row.names = F)

