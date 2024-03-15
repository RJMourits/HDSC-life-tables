
 #load packages
  library("dplyr")
  library("data.table")
  library("ggplot2")
  library("openxlsx")
  
  #clean environment
  rm(list=ls())
  
  #set file directory
  setwd("C:/Surfdrive/2021 - Surinaamse sterftetafels/")
  
  #Create output folder
  if (!dir.exists("Life tables")){
    dir.create("Life tables")
  }
  
  ################################################
  #### 0. open data and select 1851-1863 data ####
  ################################################
  
  #open dataset
  SR <- fread("Data HDSC/Slaveregister_Curacao_V1_0.csv", encoding="UTF-8")
  
  #recode variables
  SR$Sex <- ifelse(SR$Sex=="v", "female", SR$Sex)
  SR$Sex <- ifelse(SR$Sex=="m", "male", SR$Sex)
  SR$StartEntryYear <- SR$StartEntryYear_cor
  SR$EndEntryYear <- SR$EndEntryYear_cor
  
  #select 1851-1863 with known sex
  SR <- SR[SR$EndEntryYear>=1851 & SR$StartEntryYear>0, ] #13,837
  SR <- SR[SR$Sex=="male" | SR$Sex=="female", ] #13,834
  
  #explore date of entry & exit
  barplot(table(SR$StartEntryYear))
  barplot(table(SR$EndEntryYear))
  barplot(table(SR[SR$EndEntryEvent=="Death", "EndEntryYear"]))
  
  #set 1851 as starting date
  SR <- SR[SR$EndEntryYear>=1851, ] #13,834
  SR$StartEntryYear <- ifelse(SR$StartEntryYear<1851, 1851, SR$StartEntryYear)
  
  #drop impossible ages
  SR <- SR[SR$EndEntryYear-SR$Year_birth<100 & SR$EndEntryYear-SR$Year_birth>=0, ] #13,784
  
  
  ######################################
  #### 1. create person period file ####
  ######################################
  
  #make person period file with 5-year cats
  #1851
  SR2 <- SR
  SR2$year  <- 1851
  SR2$age   <- ifelse(SR2$Year_birth>1851, NA, 1851 - SR2$Year_birth)
  SR2$age   <- cut(SR2$age, breaks=c(0, 1, seq(5, 100, by=5)), labels=c("0",
                                                                        "1-4",
                                                                        "5-9",
                                                                        "10-14",
                                                                        "15-19",
                                                                        "20-24",
                                                                        "25-29",
                                                                        "30-34",
                                                                        "35-39",
                                                                        "40-44",
                                                                        "45-49",
                                                                        "50-54",
                                                                        "55-59",
                                                                        "60-64",
                                                                        "65-69",
                                                                        "70-74",
                                                                        "75-79",
                                                                        "80-84",
                                                                        "85-89",
                                                                        "90-94",
                                                                        "95-99"), right = FALSE)
  SR2$death <- ifelse(SR2$Year_birth>1851, NA,
                      ifelse(SR2$EndEntryYear==1851 & SR2$EndEntryEvent=="Death", 1, 0))
  colnames(SR2)[colnames(SR2)=="Sex"] <- "sex"
  lifetable_5_S <- SR2[,c("year", "age", "death", "sex")]
  #1852-1863
  x <- 1852
  repeat{
    SR2$year  <- x
    SR2$age   <- ifelse(SR2$EndEntryYear<x | SR2$Year_birth>x, NA, x - SR2$Year_birth)
    SR2$age   <- cut(SR2$age, breaks=c(0, 1, seq(5, 100, by=5)), labels=c("0",
                                                                          "1-4",
                                                                          "5-9",
                                                                          "10-14",
                                                                          "15-19",
                                                                          "20-24",
                                                                          "25-29",
                                                                          "30-34",
                                                                          "35-39",
                                                                          "40-44",
                                                                          "45-49",
                                                                          "50-54",
                                                                          "55-59",
                                                                          "60-64",
                                                                          "65-69",
                                                                          "70-74",
                                                                          "75-79",
                                                                          "80-84",
                                                                          "85-89",
                                                                          "90-94",
                                                                          "95-99"), right = FALSE)    
    SR2$death <- ifelse(SR2$EndEntryYear<x | SR2$Year_birth>x, NA,
                        ifelse(SR2$EndEntryYear==x & SR2$EndEntryEvent=="Death", 1, 0))
    lifetable_5_S <- rbind(lifetable_5_S, SR2[,c("year", "age", "death", "sex")])
    if(x==1862){
      break
    }
    x <- x+1
  }
  #drop NAs
  lifetable_5_S <- lifetable_5_S[!is.na(lifetable_5_S$age),]
  as.data.frame(table(lifetable_5_S$year))
  barplot(table(lifetable_5_S$age))
  
  #save life tables
  lifetable_5_S <- lifetable_5_S %>% arrange(year, age, sex)
  
  
  
  #############################
  ### 2. Create life tables ###
  #############################
  
  lifetable_5_sex <- lifetable_5_S %>% group_by(age, sex) %>% summarise(qx=mean(death), db="Suriname", n=n()) %>% ungroup()
  
  
  ##########################
  ### 2a. Surinamese men ###
  ##########################
  
  #men
  lifetable_5_male <- lifetable_5_sex[lifetable_5_sex$sex=="male", c("age", "sex", "qx")]
  
  #mention number of years in categories
  lifetable_5_male$nyears <- ifelse(lifetable_5_male$age=="0", 1,
                                    ifelse(lifetable_5_male$age=="1,4", 4, 5))
  
  #qx_period
  lifetable_5_male$qx_period <- 1 - (1 - lifetable_5_male$qx)^lifetable_5_male$nyears #interval is 5 years
  
  #lx
  lifetable_5_male$lx <- ifelse(lifetable_5_male$age=="0", 
                                100000, 
                                NA)
  repeat{
    lifetable_5_male$lx <- ifelse(is.na(lifetable_5_male$lx), 
                                  lag(lifetable_5_male$lx) * (1-lag(lifetable_5_male$qx_period)),
                                  lifetable_5_male$lx)
    if(length(which(is.na(lifetable_5_male$lx)))==0){
      break
    }
  }
  
  #dx
  lifetable_5_male$dx <- ifelse(is.na(lead(lifetable_5_male$lx)), 
                                lifetable_5_male$lx - (1 - lifetable_5_male$qx_period) * lifetable_5_male$lx, 
                                lifetable_5_male$lx - lead(lifetable_5_male$lx))
  
  #Lx
  lifetable_5_male$Lx <- ifelse(lifetable_5_male$age=="0",
                                lifetable_5_male$lx * 0.25 + lead(lifetable_5_male$lx) * (1 - 0.25),
                                ifelse(is.na(lead(lifetable_5_male$lx)),
                                       (lifetable_5_male$lx + 0) * (lifetable_5_male$nyears / 2),
                                       (lifetable_5_male$lx + lead(lifetable_5_male$lx)) * (lifetable_5_male$nyears / 2)  ))
  
  #Tx
  lifetable_5_male$Tx <- ifelse(lifetable_5_male$age=="95-99", 
                                lifetable_5_male$Lx, 
                                NA)
  repeat{
    lifetable_5_male$Tx <- ifelse(is.na(lifetable_5_male$Tx), 
                                  lifetable_5_male$Lx + lead(lifetable_5_male$Tx),
                                  lifetable_5_male$Tx)
    if(length(which(is.na(lifetable_5_male$Tx)))==0){
      break
    }
  }
  
  #ex
  lifetable_5_male$ex <- lifetable_5_male$Tx / lifetable_5_male$lx
  
  #export
  lifetable_5_male$nyears <- NULL
  lifetable_5_male[,5:8] <- round(lifetable_5_male[,5:8])
  lifetable_5_male[,9] <- round(lifetable_5_male[,9], 1)
  
  
  write.xlsx(lifetable_5_male, "Life tables/Life tables Curaçao slave registers, men.xlsx", overwrite=T)
  
  
  
  ############################
  ### 2b. Surinamese women ###
  ############################
  
  #women
  lifetable_5_female <- lifetable_5_sex[lifetable_5_sex$sex=="female", c("age", "sex", "qx")]
  
  #mention number of years in categories
  lifetable_5_female$nyears <- ifelse(lifetable_5_female$age=="0", 1,
                                      ifelse(lifetable_5_female$age=="1,4", 4, 5))
  
  #qx_period
  lifetable_5_female$qx_period <- 1 - (1 - lifetable_5_female$qx)^lifetable_5_female$nyears #interval is 5 years
  
  #lx
  lifetable_5_female$lx <- ifelse(lifetable_5_female$age=="0", 
                                  100000, 
                                  NA)
  repeat{
    lifetable_5_female$lx <- ifelse(is.na(lifetable_5_female$lx), 
                                    lag(lifetable_5_female$lx) * (1-lag(lifetable_5_female$qx_period)),
                                    lifetable_5_female$lx)
    if(length(which(is.na(lifetable_5_female$lx)))==0){
      break
    }
  }
  
  #dx
  lifetable_5_female$dx <- ifelse(is.na(lead(lifetable_5_female$lx)), 
                                  lifetable_5_female$lx - (1 - lifetable_5_female$qx_period) * lifetable_5_female$lx, 
                                  lifetable_5_female$lx - lead(lifetable_5_female$lx))
  
  #Lx
  lifetable_5_female$Lx <- ifelse(lifetable_5_female$age=="0",
                                  lifetable_5_female$lx * 0.25 + lead(lifetable_5_female$lx) * (1 - 0.25),
                                  ifelse(is.na(lead(lifetable_5_female$lx)),
                                         (lifetable_5_female$lx + 0) * (lifetable_5_female$nyears / 2),
                                         (lifetable_5_female$lx + lead(lifetable_5_female$lx)) * (lifetable_5_female$nyears / 2)  ))
  
  #Tx
  lifetable_5_female$Tx <- ifelse(lifetable_5_female$age=="95-99", 
                                  lifetable_5_female$Lx, 
                                  NA)
  repeat{
    lifetable_5_female$Tx <- ifelse(is.na(lifetable_5_female$Tx), 
                                    lifetable_5_female$Lx + lead(lifetable_5_female$Tx),
                                    lifetable_5_female$Tx)
    if(length(which(is.na(lifetable_5_female$Tx)))==0){
      break
    }
  }
  
  #ex
  lifetable_5_female$ex <- lifetable_5_female$Tx / lifetable_5_female$lx
  
  #export
  lifetable_5_female$nyears <- NULL
  lifetable_5_female[,5:8] <- round(lifetable_5_female[,5:8])
  lifetable_5_female[,9] <- round(lifetable_5_female[,9], 1)
  
  
  write.xlsx(lifetable_5_female, "Life tables/Life tables Curaçao slave registers, women.xlsx", overwrite=T)
  
  
  
  ##################
  ### 3. figures ###
  ##################
  
  #Suriname
  df <- lifetable_5_sex
  ggplot(data=df, aes(x=age, y=qx, colour=sex, group=sex, shape=sex)) +
    geom_point() +
    geom_line(lwd=1, alpha=.3) +
    geom_point(aes(shape=sex), size=1) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = seq(0,0.3,0.025), linetype=3, linewidth=.1) +
    theme(panel.background = element_blank(),
          axis.text.y = element_text(colour="grey20",size=9,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.text.x = element_text(colour="grey20",size=9,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title = element_text(size=14),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 16, face="bold"),
          legend.position="bottom", legend.title = element_blank(),
          legend.key=element_blank()) +
    scale_color_manual(values=c("#8624f5", "#1fc3aa")) +
    scale_y_continuous(expand = c(0, 0),
                       breaks=seq(0,0.3,by=0.025), 
                       limit=c(0,0.315)) +
    labs(x="Age bracket",
         y="Yearly mortality rate")
  ggsave("Life tables/Mortality rates Curaçao slave registers.jpg", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  
  
  ggplot(data=df[1:26,], aes(x=age, y=qx, colour=sex, group=sex, shape=sex)) +
    geom_point() +
    geom_line(lwd=1, alpha=.3) +
    geom_point(aes(shape=sex), size=1) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = seq(0,0.075,0.025), linetype=3, linewidth=.1) +
    theme(panel.background = element_blank(),
          axis.text.y = element_text(colour="grey20",size=9,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.text.x = element_text(colour="grey20",size=9,angle=0,hjust=.5,vjust=0,face="plain"),
          axis.title = element_text(size=14),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 16, face="bold"),
          legend.position="bottom", legend.title = element_blank(),
          legend.key=element_blank()) +
    scale_color_manual(values=c("#8624f5", "#1fc3aa")) +
    scale_y_continuous(expand = c(0, 0),
                       breaks=seq(0,0.075,by=0.025), 
                       limit=c(0,0.08)) +
    labs(x="Age bracket",
         y="Yearly mortality rate")
  ggsave("Life tables/Mortality rates Curaçao slave registers 0-59.jpg", plot = last_plot(), unit="in", dpi=900, width=8, height=6)
  
  