#########################################################################################
# Last Date modified: 12/19/2017
# Author: Katy Torres
# Description: Subset of question 24, Positive and Negative Affect Schedule
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
datpanas <- subset(dat0, select= c(assessment_id,vista_lastname,
                        panas1_interest,
                        panas2_distress,
                        panas3_excite,
                        panas4_upset,
                        panas5_strong,
                        
                        panas6_guilt,
                        panas7_scare,
                        panas8_host,
                        panas9_enth,
                        panas10_proud,
                        
                        panas11_irri,
                        panas12_alert,
                        panas13_asham,
                        panas14_insp,
                        panas15_nerv,
                        
                        panas16_deter,
                        panas17_atten,
                        panas18_jitt,
                        panas19_act,
                        panas20_afraid,
                        
                        Panas.Positive,
                        Positive.Affect.Score,
                        Negative.Affect.Score,
                        PANAS.Positive,
                        Liz.s.Formula
                        
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
write.csv( datpanas, "~/Biobank/24_PANAS/PANAS_reduced_data_export_20171219.csv",quote=T,row.names=F,na="#N/A")


