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

week_dist_salvage<-sth_salvage_data %>% group_by(week_number) %>% 
  summarise(percentage=round(sum(nfish)/totalfish*100,digits=2), nfish=sum(nfish))

sum(week_dist_salvage$percentage)

write.csv(week_dist_salvage,file.path("output/Weekly_distribution_steelhead_salvagefacilities.csv"),row.names = F)

###########
#Data from Sac and Chipps Trawl
#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.244.9

#DJFMP_trawl_data<-read.csv(file.path("data_input/2002-2021_DJFMP_trawl_fish_and_water_quality_data.csv"))
#saveRDS(DJFMP_trawl_data,file.path("data_input/2002-2021_DJFMP_trawl_fish_and_water_quality_data.rds"))

DJFMP_trawl_data<-readRDS(file = file.path("data_input/2002-2021_DJFMP_trawl_fish_and_water_quality_data.rds")) 
DJFMP_trawl_data_sth<-DJFMP_trawl_data %>% filter(OrganismCode == "RBT")
DJFMP_trawl_data_sth$week_number<-strftime(DJFMP_trawl_data_sth$SampleDate, format = "%V")

#Sac trawl
sactrawl<-DJFMP_trawl_data_sth %>% filter(Location == "Sherwood Harbor", MarkCode=="None")
totalfish_sactrawl<-sum(sactrawl$Count)
  
week_dist_sactrawl<-sactrawl%>% group_by(week_number) %>% 
  summarise(percentage=round(sum(Count)/totalfish_sactrawl*100,digits=2), Count=sum(Count))

write.csv(week_dist_sactrawl,file.path("output/Weekly_distribution_steelhead_SacTrawl.csv"),row.names = F)

#Chipps Trawl
chippstrawl<-DJFMP_trawl_data_sth %>% filter(Location == "Chipps Island", MarkCode=="None")
totalfish_chippstrawl<-sum(chippstrawl$Count)

week_dist_chippstrawl<-chippstrawl%>% group_by(week_number) %>% 
  summarise(percentage=round(sum(Count)/totalfish_chippstrawl*100,digits=2), Count=sum(Count))

write.csv(week_dist_chippstrawl,file.path("output/Weekly_distribution_steelhead_ChippsTrawl.csv"),row.names = F)

#Mossdale trawl
mossdaletrawl<-DJFMP_trawl_data_sth %>% filter(Location == "Mossdale", MarkCode=="None")
totalfish_mossdaletrawl<-sum(mossdaletrawl$Count)

week_dist_mossdaletrawl<-mossdaletrawl%>% group_by(week_number) %>% 
  summarise(percentage=round(sum(Count)/totalfish_mossdaletrawl*100,digits=2), Count=sum(Count))

write.csv(week_dist_mossdaletrawl,file.path("output/Weekly_distribution_steelhead_MossdaleTrawl.csv"),row.names = F)



DJFMP_trawl_data_wag<-DJFMP_trawl_data %>% filter(OrganismCode == "WAG")



devtools::install_github("Delta-Stewardship-Council/deltafish", force=T)
##Unused code for deltafish

surv <- open_survey()
fish <- open_fish()


# filter for sources and taxa of interest
surv_DJFMP <- surv %>% 
  filter(Source == "DJFMP") %>% 
  select(SampleID, Date) %>% filter(year(Date)>=1995)

fish_steelhead <- fish %>% 
  filter(Taxa %in% c("Oncorhynchus mykiss"))


# do a join and collect the resulting data frame
# collect executes the sql query and gives you a table
data_DJFMP <- left_join(surv_DJFMP, fish_steelhead) %>% 
  collect() 
