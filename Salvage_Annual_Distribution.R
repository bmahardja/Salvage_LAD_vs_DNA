library(tidyverse)
library(rvest)
library(lubridate)
library(splitstackshape)
library(data.table)
##
#Figures for ReROC - understanding distribution of winter-run in the Delta

#Load matched data
data <- readRDS(file = file.path("output/Salvage_DNA_vs_LAD.rds")) %>% mutate(Wateryear=ifelse(month(SampleTime)>9,year(SampleTime)+1,year(SampleTime)),
                                                                              Date=as.Date(SampleTime))
str(data)
#Load matched data pre-2010 from Trinh Nguyen
data_TN <- readRDS(file = file.path("data_input/geneticDataCombined_BH.rds")) %>%
  filter(WaterYear<=2010) %>% filter(W_DNA_wildFull>0)
#Multiply rows by nfish
data_TN_adjusted<- setDT(expandRows(data_TN, "W_DNA_wildFull")) 

#Acquire genetic and LAD data from salvage through this project
winter_run_genetic_data <- data %>%  filter(GeneticID=="Winter")
winter_run_LAD_data <- data %>%  filter(LAD_Race=="Winter") %>% mutate(Month=month(Date))

#Load unchecked DNA data post-2010
data_genetic_unchecked <- readRDS(file = file.path("output/post2010_DNA_data_unchecked.rds")) %>% mutate(Wateryear=ifelse(month(SampleTime)>9,year(SampleTime)+1,year(SampleTime)), Date=as.Date(SampleTime)) %>%
  filter(GeneticID=="Winter")
                                                                              
#Load Sac trawl data from EDI
#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.1054.1
data_sactrawl <- read_csv(file = file.path("data_input/LSR-sizebydate-2017-21.csv")) %>%
  filter(GeneticID=="Winter") %>%
  mutate(Wateryear=ifelse(month(SampleDate)>9,year(SampleDate)+1,year(SampleDate)), Date=as.Date(SampleDate))

#Load Chipps Island trawl data from EDI
#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.1055.1
data_chipps <- read_csv(file = file.path("data_input/DLC-sizebydate-2017-21.csv")) %>%
  filter(GeneticID=="Winter") %>%
  mutate(Wateryear=ifelse(month(SampleDate)>9,year(SampleDate)+1,year(SampleDate)), Date=as.Date(SampleDate))


#######Create plots

plot_salvage_genetic_matched_early<-ggplot(data_TN_adjusted, aes(x=SampleDate)) + 
  geom_histogram(color="black", fill="white") + facet_wrap(vars(WaterYear),scales="free")+
  ggtitle("Genetic winter-run at salvage facilities (matched data only) - pre-2010")+
  xlab(paste("Total sample size =",nrow(data_TN_adjusted),sep=" "))
plot_salvage_genetic_matched_early

plot_salvage_genetic_matched_recent<-ggplot(winter_run_genetic_data, aes(x=Date)) + 
  geom_histogram(color="black", fill="white") + facet_wrap(vars(Wateryear),scales="free")+
  ggtitle("Genetic winter-run at salvage facilities (matched data only) - post-2010")+
  xlab(paste("Total sample size =",nrow(winter_run_genetic_data),sep=" "))
plot_salvage_genetic_matched_recent

plot_salvage_genetic_unchecked_recent<-ggplot(data_genetic_unchecked, aes(x=Date)) + 
  geom_histogram(color="black", fill="white") + facet_wrap(vars(Wateryear),scales="free")+
  ggtitle("Genetic winter-run at salvage facilities (unchecked data) - post-2010")+
  xlab(paste("Total sample size =",nrow(data_genetic_unchecked),sep=" "))
plot_salvage_genetic_unchecked_recent

plot_salvage_LAD<-ggplot(winter_run_LAD_data, aes(x=Date)) + 
  geom_histogram(color="black", fill="white") + facet_wrap(vars(Wateryear),scales="free") +
  ggtitle("Winter-run length-at-date at salvage facilities")+
  xlab(paste("Total sample size =",nrow(winter_run_LAD_data),sep=" "))
plot_salvage_LAD

plot_sactrawl_genetic<-ggplot(data_sactrawl, aes(x=SampleDate)) + 
  geom_histogram(color="black", fill="white") + facet_wrap(vars(Wateryear),scales="free")+
  ggtitle("Genetic winter-run at Sac Trawl (unchecked) - 2017-2021")+
  xlab(paste("Total sample size =",nrow(data_sactrawl),sep=" "))
plot_sactrawl_genetic

plot_chipps_genetic<-ggplot(data_chipps, aes(x=SampleDate)) + 
  geom_histogram(color="black", fill="white") + facet_wrap(vars(Wateryear),scales="free")+
  ggtitle("Genetic winter-run at Chipps Island Trawl (unchecked) - 2017-2021")+
  xlab(paste("Total sample size =",nrow(data_chipps),sep=" "))
plot_chipps_genetic

##Print figures
tiff(filename=file.path("output/Figure_salvage_genetic_matched_early.png"), 
     type="cairo",
     units="in", 
     width=12, #10*1, 
     height=7, #22*1, 
     pointsize=5, #12, 
     res=500,
     compression="lzw")
print(plot_salvage_genetic_matched_early)
dev.off()

tiff(filename=file.path("output/Figure_salvage_genetic_matched_recent.png"), 
     type="cairo",
     units="in", 
     width=12, #10*1, 
     height=7, #22*1, 
     pointsize=5, #12, 
     res=500,
     compression="lzw")
print(plot_salvage_genetic_matched_recent)
dev.off()

tiff(filename=file.path("output/Figure_salvage_genetic_unchecked_recent.png"), 
     type="cairo",
     units="in", 
     width=12, #10*1, 
     height=7, #22*1, 
     pointsize=5, #12, 
     res=500,
     compression="lzw")
print(plot_salvage_genetic_unchecked_recent)
dev.off()

tiff(filename=file.path("output/Figure_salvage_LAD.png"), 
     type="cairo",
     units="in", 
     width=12, #10*1, 
     height=7, #22*1, 
     pointsize=5, #12, 
     res=500,
     compression="lzw")
print(plot_salvage_LAD)
dev.off()

tiff(filename=file.path("output/Figure_sactrawl_genetic.png"), 
     type="cairo",
     units="in", 
     width=12, #10*1, 
     height=7, #22*1, 
     pointsize=5, #12, 
     res=500,
     compression="lzw")
print(plot_sactrawl_genetic)
dev.off()

tiff(filename=file.path("output/Figure_chipps_genetic.png"), 
     type="cairo",
     units="in", 
     width=12, #10*1, 
     height=7, #22*1, 
     pointsize=5, #12, 
     res=500,
     compression="lzw")
print(plot_chipps_genetic)
dev.off()


####
data_chipps$dummydate<-data_chipps$SampleDate
year(data_chipps$dummydate)<-ifelse(month(data_chipps$SampleDate)>=10,2017,2018)
data_chipps_sum<-data_chipps %>%  
  summarise(quantile = scales::percent(c(0.25, 0.5, 0.75)),
            date = quantile(dummydate, c(0.25, 0.5, 0.75), type=1)) 


plot_chipps_timing<-ggplot(data_chipps, aes(x=dummydate)) + 
  geom_histogram(color="black", fill="white") +
  ggtitle("Genetic winter-run at Chipps Island Trawl (unchecked) - 2017-2021")+
  geom_vline(xintercept=data_chipps_sum$date, color="red", linetype="dashed", size=1)+
  xlab(paste("Total sample size =",nrow(data_chipps),sep=" "))
plot_chipps_timing


data_sactrawl$dummydate<-data_sactrawl$SampleDate
year(data_sactrawl$dummydate)<-ifelse(month(data_sactrawl$SampleDate)>=10,2017,2018)


data_sactrawl_sum<-data_sactrawl %>%  
  summarise(quantile = scales::percent(c(0.25, 0.5, 0.75)),
            date = quantile(dummydate, c(0.25, 0.5, 0.75), type=1)) 

plot_sactrawl_timing<-ggplot(data_sactrawl, aes(x=dummydate)) + 
  geom_histogram(color="black", fill="white") +
  ggtitle("Genetic winter-run at Sac Trawl (unchecked) - 2017-2021")+
  geom_vline(xintercept=data_sactrawl_sum$date, color="red", linetype="dashed", size=1)+
  xlab(paste("Total sample size =",nrow(data_chipps),sep=" "))
plot_sactrawl_timing



tiff(filename=file.path("output/Figure_chipps_genetic_timing.png"), 
     type="cairo",
     units="in", 
     width=12, #10*1, 
     height=7, #22*1, 
     pointsize=5, #12, 
     res=500,
     compression="lzw")
print(plot_chipps_timing)
dev.off()


tiff(filename=file.path("output/Figure_sactrawl_genetic_timing.png"), 
     type="cairo",
     units="in", 
     width=12, #10*1, 
     height=7, #22*1, 
     pointsize=5, #12, 
     res=500,
     compression="lzw")
print(plot_sactrawl_timing)
dev.off()