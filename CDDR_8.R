#########################################################################################
# Last Date modified: 12/18/2017
# Author: Katy Torres
# Description: Subset of question 8, CDDR and scoring functions
##########################################################################################

#Load plyr library
library(plyr)

#To the user: Set path to where data is stored
setwd("~/Biobank/data")
#________________________________________________________________________________________
#READ AND SUBSET LARGE DATA TO ONLY CONTAIN DESIRED QUESTIONAIRE VARIABLES
#----------------------------------------------------------------------------------------
#Read all data
datcddr0 <- read.csv('joined_data_export_20171218.csv',header=T,na.strings=c(NA,999))

#Only retain relevant variables
datcddr <- subset(datcddr0, 
              select= c(assessment_id,vista_lastname,
                        CDDR_AgeFirstUse,
                        CDDR_EverSmoked,
                        CDDR_AgeFirstReg,
                        CDDR_RegUse,
                        CDDR_Method,
                        CDDR_Quantity,
                        CDDR_THC,
                        CDDR_LastUse,
                        CDDR_Lifetime,
                        CDDR_Year,
                        CDDR_Month,
                        CDDR_5year
                        
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
write.csv( datcddr, "~/Biobank/8_CDDR/CDDR_reduced_data_export_20171218.csv",quote=T,row.names=F,na="#N/A")


