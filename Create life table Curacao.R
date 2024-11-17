
 #load packages
  library("dplyr")
  library("data.table")
  library("ggplot2")
  library("openxlsx")
  
  #clean environment
  rm(list=ls())
  
  #set file directory
  setwd("/home/tuinschepje/Surfdrive/2021 - Surinaamse sterftetafels")
  
  #Create output folder
  if (!dir.exists("Life tables")){
    dir.create("Life tables")
  }
  
  ################################################
  #### 0. open data and select 1839-1863 data ####
  ################################################
  
  #open dataset
  SR <- fread("Data HDSC/Slaveregister_Curacao_V1_0.csv", encoding="UTF-8") #21515
  
  #recode variables
  SR$Sex <- ifelse(SR$Sex=="v", "female", SR$Sex)
  SR$Sex <- ifelse(SR$Sex=="m", "male", SR$Sex)
  SR$StartEntryYear <- SR$StartEntryYear_cor
  SR$EndEntryYear <- SR$EndEntryYear_cor
  
  #select 1839-1863 with known sex
  SR <- SR[SR$EndEntryYear>=1839 & SR$StartEntryYear>0, ] #21,387
  SR <- SR[SR$Sex=="male" | SR$Sex=="female", ] #21,383
  
  #explore date of entry & exit
  barplot(table(SR$StartEntryYear))
  barplot(table(SR$EndEntryYear))
  barplot(table(SR[SR$EndEntryEvent=="Death", "EndEntryYear"]))
  
  #drop impossible ages
  SR <- SR[SR$EndEntryYear-SR$Year_birth<100 & SR$EndEntryYear-SR$Year_birth>=0, ] #21,276
  
  
  ######################################
  #### 1. create person period file ####
  ######################################
  

  #set 1839 as starting date
  SR$StartEntryDay <- ifelse(SR$StartEntryYear<1839, 1, SR$StartEntryDay)
  SR$StartEntryMonth <- ifelse(SR$StartEntryYear<1839, 1, SR$StartEntryMonth)
  SR$StartEntryYear <- ifelse(SR$StartEntryYear<1839, 1839, SR$StartEntryYear)
  
  #filter exits before 1839
  SR <- SR[SR$EndEntryYear>=1839 | is.na(EndEntryYear),] #21,276

  #check availability of year of birth, entry, and exit
  length(which(is.na(SR$Year_birth))) #0
  length(which(is.na(SR$StartEntryYear))) #0
  length(which(is.na(SR$EndEntryYear))) #0
  length(which(is.na(SR$StartEntryYear) | is.na(SR$EndEntryYear))) #0

  #explore reasons for missing dates
  table(SR[is.na(SR$StartEntryYear),]$StartEntryEvent)
  table(SR[is.na(SR$EndEntryYear),]$EndEntryEvent)

  #
  SR[SR$StartEntryEvent=="Birth" & SR$StartEntryYear<SR$Year_birth,c("Id_person", "StartEntryYear", "Year_birth")]
  SR$StartEntryYear[SR$Id_person==19710] <- NA
  SR$Year_birth[SR$Id_person==19710] <- NA
  
  #select cases with known sex, start date, and end date
  SR <- SR[SR$Sex=="male" | SR$Sex=="female", ] #21,276
  SR <- SR[!is.na(SR$StartEntryYear), ] #21,276
  SR <- SR[!is.na(SR$EndEntryYear), ] #21,276
  
  
  
  
  ######################################
  #### 1. create person period file ####
  ######################################
  
  #make person period file with 5-year cats
  #1839
  SR2 <- SR
  SR2$EndEntryEvent2 <- NA
  SR2$EndEntryEvent2 <- ifelse(grepl("death", tolower(SR2$EndEntryEvent)), "Death", SR2$EndEntryEvent2)
  SR2$EndEntryEvent2 <- ifelse(grepl("escaped", tolower(SR2$EndEntryEvent)), "Escaped", SR2$EndEntryEvent2)
  SR2$EndEntryEvent2 <- ifelse(grepl("exported", tolower(SR2$EndEntryEvent)), "Exported", SR2$EndEntryEvent2)
  SR2$EndEntryEvent2 <- ifelse(grepl("manumission", tolower(SR2$EndEntryEvent)), "Freedom", SR2$EndEntryEvent2)
  SR2$EndEntryEvent2 <- ifelse(grepl("free of taxation", tolower(SR2$EndEntryEvent)) |
                                grepl("written off", tolower(SR2$EndEntryEvent)), "Written off", SR2$EndEntryEvent2)
  SR2$EndEntryEvent2 <- ifelse(grepl("unknown", tolower(SR2$EndEntryEvent)), "Unknown", SR2$EndEntryEvent2)

  SR2$year  <- 1839
  SR2$alive <- ifelse(SR2$StartEntryYear==1839 & SR$EndEntryYear>1839, 1, 0)
  SR2$age   <- ifelse(SR2$StartEntryYear>1839 | SR2$EndEntryYear<1839, NA, 1839 - SR2$Year_birth)
  SR2$age   <- ifelse(is.na(SR2$Year_birth) & SR2$StartEntryYear==1839 & SR2$EndEntryYear>1839, "Unknown", SR2$age)
  SR2$birth1 <- ifelse(SR2$Year_birth==1839, 1, 0)
  SR2$birth2 <- ifelse(SR2$StartEntryYear==1839 & SR2$StartEntryEvent=="Birth", 1, 0)
  SR2$imported <- ifelse(SR2$StartEntryYear==1839 & SR2$StartEntryEvent=="Imported", 1, 0)
  SR2$death <- ifelse(SR2$StartEntryYear>1839, NA,
                      ifelse(SR2$EndEntryYear==1839 & SR2$EndEntryEvent2=="Death", 1, 0))
  SR2$diseased <- ifelse(SR2$StartEntryYear>1839, NA,
                      ifelse(SR2$EndEntryYear==1839 & SR2$EndEntryEvent2=="Diseased", 1, 0))
  SR2$escaped <- ifelse(SR2$StartEntryYear>1839, NA,
                      ifelse(SR2$EndEntryYear==1839 & SR2$EndEntryEvent2=="Escaped", 1, 0))
  SR2$exported <- ifelse(SR2$StartEntryYear>1839, NA,
                       ifelse(SR2$EndEntryYear==1839 & SR2$EndEntryEvent2=="Exported", 1, 0))
  SR2$manumission <- ifelse(SR2$StartEntryYear>1839, NA,
                      ifelse(SR2$EndEntryYear==1839 & SR2$EndEntryEvent2=="Freedom", 1, 0))
  SR2$written_off <- ifelse(SR2$StartEntryYear>1839, NA,
                      ifelse(SR2$EndEntryYear==1839 & SR2$EndEntryEvent2=="Written off", 1, 0))
  SR2$unknown <- ifelse(SR2$StartEntryYear>1839, NA,
                        ifelse(SR2$EndEntryYear==1839 & SR2$EndEntryEvent2=="Unknown", 1, 0))
  colnames(SR2)[colnames(SR2)=="Sex"] <- "sex"
  lifetable_5_S <- SR2[,c("year", "age", "alive", "birth1", "birth2", "imported", "death", "diseased", "escaped", "exported", "manumission", "written_off", "unknown", "sex")]
  #1840-1862
  x <- 1840
  repeat{
    SR2$year  <- x
    SR2$alive <- ifelse(SR2$StartEntryYear<=x & SR$EndEntryYear>x, 1, 0)
    SR2$age   <- ifelse(SR2$StartEntryYear>x | SR2$EndEntryYear<x, NA, x - SR2$Year_birth)
    SR2$age   <- ifelse(is.na(SR2$Year_birth) & SR2$StartEntryYear<=x & SR2$EndEntryEvent2>x, "Unknown", SR2$age)
    SR2$birth1 <- ifelse(SR2$Year_birth==x, 1, 0)
    SR2$birth2 <- ifelse(SR2$StartEntryYear==x & SR2$StartEntryEvent=="Birth", 1, 0)
    SR2$imported <- ifelse(SR2$StartEntryYear==x & SR2$StartEntryEvent=="Imported", 1, 0)
    SR2$death <- ifelse(SR2$StartEntryYear>x | SR2$EndEntryYear<x, NA,
                        ifelse(SR2$EndEntryYear==x & SR2$EndEntryEvent2=="Death", 1, 0))
    SR2$diseased <- ifelse(SR2$StartEntryYear>x | SR2$EndEntryYear<x, NA,
                        ifelse(SR2$EndEntryYear==x & SR2$EndEntryEvent2=="Diseased", 1, 0))
    SR2$escaped <- ifelse(SR2$StartEntryYear>x | SR2$EndEntryYear<x, NA,
                        ifelse(SR2$EndEntryYear==x & SR2$EndEntryEvent2=="Escaped", 1, 0))
    SR2$exported <- ifelse(SR2$StartEntryYear>x | SR2$EndEntryYear<x, NA,
                         ifelse(SR2$EndEntryYear==x & SR2$EndEntryEvent2=="Exported", 1, 0))
    SR2$manumission <- ifelse(SR2$StartEntryYear>x | SR2$EndEntryYear<x, NA,
                        ifelse(SR2$EndEntryYear==x & SR2$EndEntryEvent2=="Freedom", 1, 0))
    SR2$written_off <- ifelse(SR2$StartEntryYear>x | SR2$EndEntryYear<x, NA,
                        ifelse(SR2$EndEntryYear==x & SR2$EndEntryEvent2=="Written off", 1, 0))
    SR2$unknown <- ifelse(SR2$StartEntryYear>x | SR2$EndEntryYear<x, NA,
                          ifelse(SR2$EndEntryYear==x & SR2$EndEntryEvent2=="Unknown", 1, 0))
    lifetable_5_S <- rbind(lifetable_5_S, SR2[,c("year", "age", "alive", "birth1", "birth2", "imported", "death", "diseased", "escaped", "exported", "manumission", "written_off", "unknown", "sex")])
    if(x==1862){
      break
    }
    x <- x+1
  }
  #1863-7-1
    x <- 1863
    SR2$year  <- x
    SR2$alive <- ifelse(SR2$EndEntryDay==1 & SR2$EndEntryMonth>=7 & SR2$EndEntryYear==1863, 1, 0)
    SR2$age   <- ifelse(SR2$EndEntryYear<x, NA, 
                        ifelse(SR2$EndEntryMonth<7, x - SR2$Year_birth, x - SR2$Year_birth - 1))
    SR2$age   <- ifelse(is.na(SR2$Year_birth) & SR2$EndEntryDay==1 & SR2$EndEntryMonth>=7 & SR2$EndEntryYear==1863, "Unknown", SR2$age)
    SR2$birth1 <- ifelse(SR2$Year_birth==x, 1, 0)
    SR2$birth2 <- ifelse(SR2$StartEntryYear==x & SR2$StartEntryEvent=="Birth", 1, 0)
    SR2$death <- ifelse(SR2$EndEntryYear<x, NA,
                        ifelse(SR2$EndEntryYear==x & SR2$EndEntryEvent=="Death", 1, 0))
    SR2$diseased <- ifelse(SR2$EndEntryYear<x, NA,
                        ifelse(SR2$EndEntryYear==x & SR2$EndEntryEvent=="Diseased", 1, 0))
    SR2$exported <- ifelse(SR2$EndEntryYear<x, NA,
                         ifelse(SR2$EndEntryYear==x & SR2$EndEntryEvent2=="Exported", 1, 0))
    SR2$manumission <- ifelse(SR2$EndEntryYear<x, NA,
                        ifelse(SR2$EndEntryYear==x & SR2$EndEntryEvent2=="Freedom", 1, 0))
    SR2$written_off <- ifelse(SR2$EndEntryYear<x, NA,
                        ifelse(SR2$EndEntryYear==x & SR2$EndEntryEvent2=="Written off", 1, 0))
    SR2$unknown <- ifelse(SR2$EndEntryYear<x, NA,
                          ifelse(SR2$EndEntryYear==x & SR2$EndEntryEvent2=="Unknown", 1, 0))
    lifetable_5_S <- rbind(lifetable_5_S, SR2[,c("year", "age", "alive", "birth1", "birth2", "imported", "death", "diseased", "escaped", "exported", "manumission", "written_off", "unknown", "sex")])

  #drop NAs & set unknown to NA
  lifetable_5_S <- lifetable_5_S[!is.na(lifetable_5_S$age),]
  as.data.frame(table(as.numeric(lifetable_5_S$year)))
  barplot(table(as.numeric(lifetable_5_S$age)))
  
  #save life tables
  lifetable_5_S <- lifetable_5_S %>% arrange(year, age, sex)
  
  
  
  #############################
  ### 2. Create life tables ###
  #############################
  
  lifetable_5_men <- lifetable_5_S %>% filter(sex=="male") %>% group_by(year, age) %>% summarise(n_men=sum(alive),
                                                                                                 births_men_by_birth_year=sum(birth1, na.rm=T),
                                                                                                 births_men_by_registration_year=sum(birth2, na.rm=T),
                                                                                                 imported_men=sum(imported, na.rm=T),
                                                                                                 deaths_men=sum(death, na.rm=T),
                                                                                                 diseased_men=sum(diseased, na.rm=T),
                                                                                                 escaped_men=sum(escaped, na.rm=T),
                                                                                                 exported_men=sum(exported, na.rm=T),
                                                                                                 manumission_men=sum(manumission, na.rm=T),
                                                                                                 written_off_men=sum(written_off, na.rm=T),
                                                                                                 unknown_men=sum(unknown, na.rm=T)) %>% ungroup()
  lifetable_5_women <- lifetable_5_S %>% filter(sex=="female") %>% group_by(year, age) %>% summarise(n_women=sum(alive),
                                                                                                     births_women_by_birth_year=sum(birth1, na.rm=T),
                                                                                                     births_women_by_registration_year=sum(birth2, na.rm=T),
                                                                                                     imported_women=sum(imported, na.rm=T),
                                                                                                     deaths_women=sum(death, na.rm=T),
                                                                                                     diseased_women=sum(diseased, na.rm=T),
                                                                                                     escaped_women=sum(escaped, na.rm=T),
                                                                                                     exported_women=sum(exported, na.rm=T),
                                                                                                     manumission_women=sum(manumission, na.rm=T),
                                                                                                     written_off_women=sum(written_off, na.rm=T),
                                                                                                     unknown_women=sum(unknown, na.rm=T)) %>% ungroup()
  
  lifetable_5_S2 <- data.frame(year=rep(1839:1862, each=102), age=rep(c(0:100,"Unknown"),12))
  
  lifetable_5_S2 <- merge(lifetable_5_S2, lifetable_5_men, by=c("year", "age"), all=T)
  lifetable_5_S2 <- merge(lifetable_5_S2, lifetable_5_women, by=c("year", "age"), all=T)
  #lifetable_5_S2[is.na(lifetable_5_S2)] <- 0
  
  lifetable_5_S2 <- lifetable_5_S2 %>% arrange(year, as.numeric(age))

  write.xlsx(lifetable_5_S2, "Life tables/Alphonse/Curacao/Opzet life tables Caracao slave registers.xlsx", overwrite=T)
  
  
  
  ##################
  ### 3. figures ###
  ##################
  
  plaatje <- lifetable_5_S %>% group_by(year, age, sex) %>% summarise(n=n(), deaths=sum(death)) %>% ungroup()
  plaatje$age <- as.numeric(plaatje$age)
  
  #Suriname
  ggplot(data=plaatje[plaatje$year==1839,], aes(x=age, y=n, colour=sex, group=sex, shape=sex)) +
    geom_point() +
    geom_line(lwd=0.5, alpha=.3) +
    geom_point(aes(shape=sex), size=1) +
    geom_vline(xintercept = 0) +
    geom_vline(xintercept = seq(0,100,10), linetype=3, linewidth=.1) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = seq(0,150,25), linetype=3, linewidth=.1) +
    theme(panel.background = element_blank(),
          axis.text.y = element_text(colour="grey20",size=9,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.text.x = element_text(colour="grey20",size=9,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title = element_text(size=14),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 16, face="bold"),
          legend.position="bottom", legend.title = element_blank(),
          legend.key=element_blank()) +
    scale_color_manual(values=c("#8624f5", "#1fc3aa")) +
    scale_x_continuous(expand = c(0, 0),
                       breaks=seq(0,100,by=10), 
                       limit=c(0,102)) +
    scale_y_continuous(expand = c(0, 0),
                       breaks=seq(0,150,by=25), 
                       limit=c(0,160)) +
    labs(x="Leeftijd",
         y="Aantal slaafgemaakten")
  ggsave("Life tables/Alphonse/Curacao/1839.jpg", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  

 
  ggplot(data=plaatje[plaatje$year==1862,], aes(x=age, y=n, colour=sex, group=sex, shape=sex)) +
    geom_point() +
    geom_line(lwd=0.5, alpha=.3) +
    geom_point(aes(shape=sex), size=1) +
    geom_vline(xintercept = 0) +
    geom_vline(xintercept = seq(0,100,10), linetype=3, linewidth=.1) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = seq(0,150,25), linetype=3, linewidth=.1) +
    theme(panel.background = element_blank(),
          axis.text.y = element_text(colour="grey20",size=9,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.text.x = element_text(colour="grey20",size=9,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title = element_text(size=14),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 16, face="bold"),
          legend.position="bottom", legend.title = element_blank(),
          legend.key=element_blank()) +
    scale_color_manual(values=c("#8624f5", "#1fc3aa")) +
    scale_x_continuous(expand = c(0, 0),
                       breaks=seq(0,100,by=10), 
                       limit=c(0,102)) +
    scale_y_continuous(expand = c(0, 0),
                       breaks=seq(0,150,by=25), 
                       limit=c(0,160)) +
    labs(x="Leeftijd",
         y="Aantal slaafgemaakten")
  ggsave("Life tables/Alphonse/Curacao/1862.jpg", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  