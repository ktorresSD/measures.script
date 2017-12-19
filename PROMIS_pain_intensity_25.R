#########################################################################################
# Last Date modified: 12/19/2017
# Author: Katy Torres
# Description: Subset of question 25, PROMIS PAIN INTENSITY
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
datpain <- subset(dat0, 
              select= c(assessment_id,vista_lastname,
                        pain_level,
                        pain_intensity,
                        pain_average,
                        
                        pain_interfere_life,
                        pain_interfere_conc,
                        pain_interfere_day,
                        pain_interfere_rec,
                        pain_interfere_task,
                        
                        pain_interfere_social,
                        pain_score_intensity,
                        pain_score_interference
              ))
#________________________________________________________________________________________
# Data Manipulation and cleaning
#----------------------------------------------------------------------------------------

#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------

#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
write.csv( datpain, "~/Biobank/25_PROMIS_Pain_Intensity/PROMIS_Pain_Intensity_reduced_data_export_20171219.csv",quote=T,row.names=F,na="#N/A")


