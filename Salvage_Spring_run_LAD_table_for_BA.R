library(tidyverse)
library(rvest)
library(lubridate)
library(splitstackshape)
library(data.table)
library(readxl)
##Create table summarizing spring-run Loss for the BA per Suzanne and Elissa's request 6/6/2023


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

str(salvage_data)

#Summarize data for spring-run LAD
salvage_SR_LAD <- salvage_data %>% rename(SampleTime='Sample Time',LAD_Race='LAD Race',LAD_Loss='LAD Loss') %>%
  mutate(Date=as.Date(SampleTime)) %>% filter(LAD_Race=="Spring") %>%  mutate(Wateryear=ifelse(month(Date)>9,year(Date)+1,year(Date))) %>%
  group_by(Wateryear) %>% summarise(SpringRun_LAD_Loss=sum(LAD_Loss))


#Summarize data for all chinook observed at salvage                  
salvage_total_CHN <- salvage_data %>% rename(SampleTime='Sample Time') %>%
  mutate(Date=as.Date(SampleTime)) %>%  mutate(Wateryear=ifelse(month(Date)>9,year(Date)+1,year(Date))) %>%
  group_by(Wateryear) %>% summarise(unclipped_salmon_samplesize=sum(nfish))

#Load Tracy Fish facility genetic data from EDI
#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.1049.2
data_cvp <- read_csv(file = file.path("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1049.2&entityid=7a8e220e83f9c9facce6ecd9d3933a9a")) %>%
  mutate(Wateryear=ifelse(month(SampleDate)>9,year(SampleDate)+1,year(SampleDate)), Date=as.Date(SampleDate))

#Load Skinner Fish facility genetic data from EDI
#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.1056.1
data_swp <- read_csv(file = file.path("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1056.1&entityid=ebcf5ce43e12876706d8ece935874f0d")) %>%
  mutate(Wateryear=ifelse(month(SampleDate)>9,year(SampleDate)+1,year(SampleDate)), Date=as.Date(SampleDate))

#Combine CVP and SWP data
data_genetic <- rbind(data_cvp,data_swp)

#Summarize data for total number of fish genetically analyzed
salvage_genetic_sample <- data_genetic %>% mutate(nfish=1) %>% group_by(Wateryear) %>% summarise(samplesize_genetics=sum(nfish))

#Summarize data for total number of genetically confirmed spring-run
salvage_genetic_SR_nfish <- data_genetic %>% mutate(nfish=1) %>% filter(GeneticID=="Spring") %>% group_by(Wateryear) %>% summarise(genetic_spring_nfish=sum(nfish))


##Load QA/QC'ed genetic data
genetic_qaqc_data <- readRDS(file = file.path("output/Salvage_DNA_vs_LAD.rds")) 

#Summarize just the paired genetic data for spring-run
salvage_genetic_SR_loss_qaqc <- genetic_qaqc_data %>% filter(GeneticID=="Spring") %>% mutate(Date=as.Date(SampleTime)) %>% 
  mutate(Wateryear=ifelse(month(Date)>9,year(Date)+1,year(Date))) %>%
  group_by(Wateryear) %>% summarise(SpringRun_Genetic_Loss=sum(LAD_Loss))

#Summarize sample size for the paired genetic data for spring-run
salvage_genetic_SR_nfish_qaqc <- genetic_qaqc_data %>% filter(GeneticID=="Spring") %>% mutate(Date=as.Date(SampleTime)) %>% 
  mutate(Wateryear=ifelse(month(Date)>9,year(Date)+1,year(Date))) %>%
  group_by(Wateryear) %>% mutate(samplesize=1) %>% summarise(SpringRun_Genetic_QAQC_nfish=sum(samplesize))


###########
#Combine all the data into one
salvage_SR_final_table <- salvage_SR_LAD %>% left_join(salvage_total_CHN) %>% left_join(salvage_genetic_sample) %>% 
  left_join(salvage_genetic_SR_nfish) %>% left_join(salvage_genetic_SR_loss_qaqc) %>% left_join(salvage_genetic_SR_nfish_qaqc)
