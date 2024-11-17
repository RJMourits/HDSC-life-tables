
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
  #### 0. open data and select 1851-1863 data ####
  ################################################
  
  #open dataset
  SR <- fread("Data HDSC/Dataset Suriname Slave and Emancipation Registers Version 1.1.csv", encoding="UTF-8")
  
  #select series 4
  SR <- SR[SR$Serieregister=="1851-1863",] #66,722

  #set 1851 as starting date
  SR$StartEntryDay <- ifelse(SR$StartEntryYear<1851, 1, SR$StartEntryDay)
  SR$StartEntryMonth <- ifelse(SR$StartEntryYear<1851, 1, SR$StartEntryMonth)
  SR$StartEntryYear <- ifelse(SR$StartEntryYear<1851, 1851, SR$StartEntryYear)
  
  #filter exits before 1851
  SR <- SR[SR$EndEntryYear>=1851 | is.na(EndEntryYear),] #66,679

  #check availability of year of birth, entry, and exit
  length(which(is.na(SR$Year_birth))) #1,093
  length(which(is.na(SR$StartEntryYear))) #66
  length(which(is.na(SR$EndEntryYear))) #410
  length(which(is.na(SR$StartEntryYear) | is.na(SR$EndEntryYear))) #438

  #explore reasons for missing dates
  table(SR[is.na(SR$StartEntryYear),]$StartEntryEvent)
  table(SR[is.na(SR$EndEntryYear),]$EndEntryEvent)

  
  #select cases with known sex, start date, and end date
  SR <- SR[SR$Sex=="male" | SR$Sex=="female", ] #66,605
  SR <- SR[!is.na(SR$StartEntryYear), ] #66,598
  SR <- SR[!is.na(SR$EndEntryYear), ] #66,228
  
  
  #drop impossible ages
  length(which(SR$EndEntryYear-SR$Year_birth>100)) #22
  length(which(SR$EndEntryYear-SR$Year_birth<0)) #2
  SR[SR$EndEntryYear-SR$Year_birth>100,c("Id_source", "Name_enslaved", "Name_mother", "Year_birth", "EndEntryYear")]
  SR$Day_birth <- ifelse(SR$EndEntryYear-SR$Year_birth>100 | SR$EndEntryYear-SR$Year_birth<0, NA, SR$Day_birth)
  SR$Month_birth <- ifelse(SR$EndEntryYear-SR$Year_birth>100 | SR$EndEntryYear-SR$Year_birth<0, NA, SR$Month_birth)
  SR$Year_birth <- ifelse(SR$EndEntryYear-SR$Year_birth>100 | SR$EndEntryYear-SR$Year_birth<0, NA, SR$Year_birth)
  SR$Year_birth[SR$Id_source=="260953153723"] <- 1852 
  SR$EndEntryYear[SR$Id_source=="332322122060"] <- 1858 

  #explore missing birth
  length(which(is.na(SR$Year_birth)))
  length(which(is.na(SR$Year_birth) & is.na(SR$EndEntryYear)))
  length(which(is.na(SR$Year_birth) & is.na(SR$StartEntryYear)))
  SR2 <- SR[is.na(SR$Year_birth),]
  write.xlsx(SR2, "Life tables/Alphonse/Geen geboortedatum 1.1 na selectie.xlsx")

  #explore year birth != year entry
  SR2 <- SR[SR$StartEntryEvent=="Birth",]
  tijd <- as.data.frame(table(SR2$StartEntryYear-SR2$Year_birth))
  write.xlsx(tijd, "Life tables/Alphonse/Verschil tussen registratiejaar en geboortejaar.xlsx")
  length(which(SR2$StartEntryEvent=="Birth" & SR2$StartEntryYear==SR2$Year_birth)) #10,986 (79.1%)
  length(which(SR2$StartEntryEvent=="Birth" & SR2$StartEntryYear!=SR2$Year_birth)) # 2,901 (20.9%)
  length(which(SR$StartEntryYear<SR$Year_birth)) #66
  SR[SR$StartEntryYear<SR$Year_birth,c("Id_source", "Name_enslaved", "Name_mother", "Year_birth", "EndEntryYear")]
  length(which(SR$StartEntryEvent=="Birth" & SR$StartEntryYear-SR$Year_birth>9)) #21
  SR[SR$StartEntryEvent=="Birth" & SR$StartEntryYear-SR$Year_birth>9,c("Id_source", "Name_enslaved", "Name_mother", "Year_birth", "EndEntryYear")]
  SR$Year_birth[SR$Id_source=="25051775082"] <- 1859
  
  
  SR$Day_birth <- ifelse(SR$StartEntryYear<SR$Year_birth | SR$StartEntryEvent=="Birth" & SR$StartEntryYear-SR$Year_birth>9, NA, SR$Day_birth)
  SR$Month_birth <- ifelse(SR$StartEntryYear<SR$Year_birth | SR$StartEntryEvent=="Birth" & SR$StartEntryYear-SR$Year_birth>9, NA, SR$Month_birth)
  SR$Year_birth <- ifelse(SR$StartEntryYear<SR$Year_birth | SR$StartEntryEvent=="Birth" & SR$StartEntryYear-SR$Year_birth>9, NA, SR$Year_birth)

  #visualise
  tijd1 <- SR2[SR2$StartEntryEvent=="Birth" & SR2$StartEntryYear==SR2$Year_birth,]
  tijd1 <- table(tijd1$StartEntryMonth - tijd1$Month_birth)
  barplot(tijd1)
  ggplot(data=mutate(as.data.frame(tijd1), Var1=as.numeric(as.character(Var1)), Perc=Freq/sum(Freq)*100), aes(x=Var1, y=Perc)) +
  geom_col() +
  geom_vline(xintercept = seq(-12,12,3), linetype=3, linewidth=.1) +
  geom_hline(yintercept = seq(0,35,5), linetype=3, linewidth=.1) +
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
                     breaks=seq(-12,12,by=3), 
                     limit=c(-12.5,12.5)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks=seq(0,35,by=5), 
                     limit=c(0,37.5)) +
  labs(x="Maanden tussen geboorte en inschrijving",
       y="Percentage")
  ggsave("Life tables/Alphonse/Tijd tussen registratie en geboorte (zelfde jaar).jpg", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  
  tijd2 <- SR2[SR2$StartEntryEvent=="Birth" & SR2$StartEntryYear-SR2$Year_birth==1,]
  tijd2 <- table(tijd2$StartEntryMonth+12 - tijd2$Month_birth)
  barplot(tijd2)
  ggplot(data=mutate(as.data.frame(tijd2), Var1=as.numeric(as.character(Var1)), Perc=Freq/sum(Freq)*100), aes(x=Var1, y=Perc)) +
  geom_col() +
  geom_vline(xintercept = seq(0,12,3), linetype=3, linewidth=.1) +
  geom_hline(yintercept = seq(0,35,5), linetype=3, linewidth=.1) +
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
                     breaks=seq(0,12,by=1), 
                     limit=c(-0.5,12.5)) +
  scale_y_continuous(expand = c(0, 0),
                     breaks=seq(0,35,by=5), 
                     limit=c(0,37.5)) +
  labs(x="Maanden tussen geboorte en inschrijving",
       y="Percentage")
  ggsave("Life tables/Alphonse/Tijd tussen registratie en geboorte (1 jaar later).jpg", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  


  
  ######################################
  #### 1. create person period file ####
  ######################################
  
  #make person period file with 5-year cats
  #1851
  SR2 <- SR
  SR2$year  <- 1851
  SR2$alive <- ifelse(SR2$StartEntryYear==1851 & SR$EndEntryYear>1851, 1, 0)
  SR2$age   <- ifelse(SR2$StartEntryYear>1851 | SR2$EndEntryYear<1851, NA, 1851 - SR2$Year_birth)
  SR2$age   <- ifelse(is.na(SR2$Year_birth) & SR2$StartEntryYear==1851 & SR2$EndEntryYear>1851, "Unknown", SR2$age)
  SR2$birth1 <- ifelse(SR2$Year_birth==1851, 1, 0)
  SR2$birth2 <- ifelse(SR2$StartEntryYear==1851 & SR2$StartEntryEvent=="Birth", 1, 0)
  SR2$death <- ifelse(SR2$StartEntryYear>1851, NA,
                      ifelse(SR2$EndEntryYear==1851 & SR2$EndEntryEvent=="Death", 1, 0))
  SR2$diseased <- ifelse(SR2$StartEntryYear>1851, NA,
                      ifelse(SR2$EndEntryYear==1851 & SR2$EndEntryEvent=="Diseased", 1, 0))
  SR2$escaped <- ifelse(SR2$StartEntryYear>1851, NA,
                      ifelse(SR2$EndEntryYear==1851 & SR2$EndEntryEvent=="Escaped", 1, 0))
  SR2$manumission <- ifelse(SR2$StartEntryYear>1851, NA,
                      ifelse(SR2$EndEntryYear==1851 & SR2$EndEntryEvent=="Freedom", 1, 0))
  SR2$written_off <- ifelse(SR2$StartEntryYear>1851, NA,
                      ifelse(SR2$EndEntryYear==1851 & SR2$EndEntryEvent=="Written off", 1, 0))
  colnames(SR2)[colnames(SR2)=="Sex"] <- "sex"
  lifetable_5_S <- SR2[,c("year", "age", "alive", "birth1", "birth2", "death", "diseased", "escaped", "manumission", "written_off", "sex")]
  #1852-1862
  x <- 1852
  repeat{
    SR2$year  <- x
    SR2$alive <- ifelse(SR2$StartEntryYear<=x & SR$EndEntryYear>x, 1, 0)
    SR2$age   <- ifelse(SR2$StartEntryYear>x | SR2$EndEntryYear<x, NA, x - SR2$Year_birth)
    SR2$age   <- ifelse(is.na(SR2$Year_birth) & SR2$StartEntryYear<=x & SR2$EndEntryEvent>x, "Unknown", SR2$age)
    SR2$birth1 <- ifelse(SR2$Year_birth==x, 1, 0)
    SR2$birth2 <- ifelse(SR2$StartEntryYear==x & SR2$StartEntryEvent=="Birth", 1, 0)
    SR2$death <- ifelse(SR2$StartEntryYear>x | SR2$EndEntryYear<x, NA,
                        ifelse(SR2$EndEntryYear==x & SR2$EndEntryEvent=="Death", 1, 0))
    SR2$diseased <- ifelse(SR2$StartEntryYear>x | SR2$EndEntryYear<x, NA,
                        ifelse(SR2$EndEntryYear==x & SR2$EndEntryEvent=="Diseased", 1, 0))
    SR2$escaped <- ifelse(SR2$StartEntryYear>x | SR2$EndEntryYear<x, NA,
                        ifelse(SR2$EndEntryYear==x & SR2$EndEntryEvent=="Escaped", 1, 0))
    SR2$manumission <- ifelse(SR2$StartEntryYear>x | SR2$EndEntryYear<x, NA,
                        ifelse(SR2$EndEntryYear==x & SR2$EndEntryEvent=="Freedom", 1, 0))
    SR2$written_off <- ifelse(SR2$StartEntryYear>x | SR2$EndEntryYear<x, NA,
                        ifelse(SR2$EndEntryYear==x & SR2$EndEntryEvent=="Written off", 1, 0))
    lifetable_5_S <- rbind(lifetable_5_S, SR2[,c("year", "age", "alive", "birth1", "birth2", "death", "diseased", "escaped", "manumission", "written_off", "sex")])
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
    SR2$escaped <- ifelse(SR2$EndEntryYear<x, NA,
                        ifelse(SR2$EndEntryYear==x & SR2$EndEntryEvent=="Escaped", 1, 0))
    SR2$manumission <- ifelse(SR2$EndEntryYear<x, NA,
                        ifelse(SR2$EndEntryYear==x & SR2$EndEntryEvent=="Freedom", 1, 0))
    SR2$written_off <- ifelse(SR2$EndEntryYear<x, NA,
                        ifelse(SR2$EndEntryYear==x & SR2$EndEntryEvent=="Written off", 1, 0))
    lifetable_5_S <- rbind(lifetable_5_S, SR2[,c("year", "age", "alive", "birth1", "birth2", "death", "diseased", "escaped", "manumission", "written_off", "sex")])

  #drop NAs & set unknown to NA
  lifetable_5_S <- lifetable_5_S[!is.na(lifetable_5_S$age),]
  as.data.frame(table(as.numeric(lifetable_5_S$year)))
  barplot(table(as.numeric(lifetable_5_S$age)))
  
  y <- SR2[,]
  z <- SR2[SR2$EndEntryEvent=="Ended",]

  #save life tables
  lifetable_5_S <- lifetable_5_S %>% arrange(year, age, sex)
  
  
  
  #############################
  ### 2. Create life tables ###
  #############################
  
  lifetable_5_men <- lifetable_5_S %>% filter(sex=="male") %>% group_by(year, age) %>% summarise(n_men=sum(alive),
                                                                                                 births_men_by_birth_year=sum(birth1, na.rm=T),
                                                                                                 births_men_by_registration_year=sum(birth2, na.rm=T),
                                                                                                 deaths_men=sum(death, na.rm=T),
                                                                                                 diseased_men=sum(diseased, na.rm=T),
                                                                                                 escaped_men=sum(escaped, na.rm=T),
                                                                                                 manumission_men=sum(manumission, na.rm=T),
                                                                                                 written_off_men=sum(written_off, na.rm=T)) %>% ungroup()
  lifetable_5_women <- lifetable_5_S %>% filter(sex=="female") %>% group_by(year, age) %>% summarise(n_women=sum(alive),
                                                                                                     births_women_by_birth_year=sum(birth1, na.rm=T),
                                                                                                     births_women_by_registration_year=sum(birth2, na.rm=T),
                                                                                                     deaths_women=sum(death, na.rm=T),
                                                                                                     diseased_women=sum(diseased, na.rm=T),
                                                                                                     escaped_women=sum(escaped, na.rm=T),
                                                                                                     manumission_women=sum(manumission, na.rm=T),
                                                                                                     written_off_women=sum(written_off, na.rm=T)) %>% ungroup()
  
  lifetable_5_S2 <- data.frame(year=rep(1851:1862, each=102), age=rep(c(0:100,"Unknown"),12))
  
  lifetable_5_S2 <- merge(lifetable_5_S2, lifetable_5_men, by=c("year", "age"), all=T)
  lifetable_5_S2 <- merge(lifetable_5_S2, lifetable_5_women, by=c("year", "age"), all=T)
  #lifetable_5_S2[is.na(lifetable_5_S2)] <- 0
  
  lifetable_5_S2 <- lifetable_5_S2 %>% arrange(year, as.numeric(age))

  write.xlsx(lifetable_5_S2, "Life tables/Alphonse/Opzet life tables Surinamese slave registers.xlsx", overwrite=T)
  
  
  
  ##################
  ### 3. figures ###
  ##################
  
  plaatje <- lifetable_5_S %>% group_by(year, age, sex) %>% summarise(n=n(), deaths=sum(death)) %>% ungroup()
  plaatje$age <- as.numeric(plaatje$age)
  
  #Suriname
  ggplot(data=plaatje[plaatje$year==1851,], aes(x=age, y=n, colour=sex, group=sex, shape=sex)) +
    geom_point() +
    geom_line(lwd=0.5, alpha=.3) +
    geom_point(aes(shape=sex), size=1) +
    geom_vline(xintercept = 0) +
    geom_vline(xintercept = seq(0,100,10), linetype=3, linewidth=.1) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = seq(0,600,100), linetype=3, linewidth=.1) +
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
                       breaks=seq(0,600,by=100), 
                       limit=c(0,630)) +
    labs(x="Leeftijd",
         y="Aantal slaafgemaakten")
  ggsave("Life tables/Alphonse/1851.jpg", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  

  ggplot(data=plaatje[plaatje$year==1857,], aes(x=age, y=n, colour=sex, group=sex, shape=sex)) +
    geom_point() +
    geom_line(lwd=0.5, alpha=.3) +
    geom_point(aes(shape=sex), size=1) +
    geom_vline(xintercept = 0) +
    geom_vline(xintercept = seq(0,100,10), linetype=3, linewidth=.1) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = seq(0,600,100), linetype=3, linewidth=.1) +
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
                       breaks=seq(0,600,by=100), 
                       limit=c(0,630)) +
    labs(x="Leeftijd",
         y="Aantal slaafgemaakten")
  ggsave("Life tables/Alphonse/1857.jpg", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  
  
  ggplot(data=plaatje[plaatje$year==1862,], aes(x=age, y=n, colour=sex, group=sex, shape=sex)) +
    geom_point() +
    geom_line(lwd=0.5, alpha=.3) +
    geom_point(aes(shape=sex), size=1) +
    geom_vline(xintercept = 0) +
    geom_vline(xintercept = seq(0,100,10), linetype=3, linewidth=.1) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = seq(0,600,100), linetype=3, linewidth=.1) +
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
                       breaks=seq(0,600,by=100), 
                       limit=c(0,630)) +
    labs(x="Leeftijd",
         y="Aantal slaafgemaakten")
  ggsave("Life tables/Alphonse/1862.jpg", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  