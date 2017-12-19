#########################################################################################
# Last Date modified: 12/19/2017
# Author: Katy Torres
# Description: Subset of question 29, Tinnitus Screener
##########################################################################################

#Load plyr library
library(plyr)

#To the user: Set path to where data is stored
setwd("~/Biobank/data")
#________________________________________________________________________________________
#READ AND SUBSET LARGE DATA TO ONLY CONTAIN DESIRED QUESTIONAIRE VARIABLES
#----------------------------------------------------------------------------------------
#Read all data
dat0 <- read.csv('joined_data_export_20171218.csv',header=T,na.strings=c(NA,999))

#Only retain relevant variables
dattinnitus <- subset(dat0, 
              select= c(assessment_id,vista_lastname,
                        Tinnitus.1_2.3mins,
                        Tinnitus.2_6months,
                        Tinnitus.3_quietroom,
                        Tinnitus.4_recentevents,
                        Tinnitus.5_comeandgo,
                        Tinnitus.6_experience
                        
              ))
#________________________________________________________________________________________
# Data Manipulation and cleaning
#----------------------------------------------------------------------------------------

#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------
#Scoring function defined
tinnitus <- function(x)
{
  
  #attach(x)
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
  #summary score is the summation of items 1-7
  #Note: This function is not designed to handle NA values (subject must have complete data)
  
  
  tinnitus_total <-   Tinnitus.1_2.3mins+
  Tinnitus.2_6months+
  Tinnitus.3_quietroom+
  Tinnitus.4_recentevents+
  Tinnitus.5_comeandgo+
  Tinnitus.6_experience
  
#SCORING of categories
  
  no_tinnitus<- as.numeric(!Tinnitus.1_2.3mins)
  
  
  if(!is.na(Tinnitus.1_2.3mins) & !is.na(Tinnitus.5_comeandgo) &  !is.na(Tinnitus.4_recentevents)) {
    if(Tinnitus.4_recentevents ==1 & Tinnitus.5_comeandgo == 1 & Tinnitus.1_2.3mins==1) { tinnitus_temporary_only<- 1} 
    else{ tinnitus_temporary_only<- 0} 
  }
  else { tinnitus_temporary_only<- NA } 
  
  
  if(!is.na(Tinnitus.6_experience) & !is.na(Tinnitus.1_2.3mins)) {
    if(Tinnitus.6_experience == 2 & Tinnitus.1_2.3mins == 1) { tinnitus_occassional<- 1}
    else{ tinnitus_occassional <- 0}
  } 
  else { tinnitus_occassional <- NA } 
  
  
  
  if(!is.na(Tinnitus.1_2.3mins) & !is.na(Tinnitus.2_6months)) {
    if(Tinnitus.2_6months==0 & Tinnitus.1_2.3mins==1) { tinnitus_acute<- 1} 
    else{ tinnitus_acute<- 0} 
  }
  else { tinnitus_acute <- NA } 
  
  
  
  if(!is.na(Tinnitus.1_2.3mins) & !is.na(Tinnitus.2_6months)) {
    if(Tinnitus.1_2.3mins == 1 & Tinnitus.2_6months == 1) { tinnitus_chronic<- 1} 
    else if(Tinnitus.1_2.3mins == 1 & Tinnitus.2_6months == 0) { tinnitus_chronic<- 0} 
    else if(Tinnitus.1_2.3mins == 0) { tinnitus_chronic <- 0} 
  }
  else { tinnitus_chronic <- NA } 


  

  
  tinnitus_data_complete <- as.numeric(
    sum(
      is.na(
        c(Tinnitus.1_2.3mins,
          Tinnitus.2_6months,
          Tinnitus.3_quietroom,
          Tinnitus.4_recentevents,
          Tinnitus.5_comeandgo,
          Tinnitus.6_experience
        )
      )
    ) == 0
  )
  
  scores <- data.frame(tinnitus_total, no_tinnitus, tinnitus_temporary_only, tinnitus_occassional,
                       tinnitus_acute, tinnitus_chronic,
                       tinnitus_data_complete)
  
  return(scores)
}


#Calculate summary scores in data
tinnitus_scores <- adply(dattinnitus, 1, tinnitus)

#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
write.csv( tinnitus_scores, "~/Biobank/29_Tinnitus_Screener/tinnitus_screener_reduced_data_export_20171219.csv",quote=T,row.names=F,na="#N/A")


