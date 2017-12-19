#########################################################################################
# Last Date modified: 12/18/2017
# Author: Katy Torres
# Description: Subset of question 1, AUDIT
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
dataudit <- subset(dat0, 
              select= c(assessment_id,vista_lastname,
                        alcol2_many,
                        alcol1_often,
                        alcol3_six,
                        alcol4_often,
                        alcol5_fail,
                        alcol6_start,
                        alcol7_guilt,
                        alcol8_remember,
                        alcol9_injure,
                        alcol10_concern,
                        audit10_score,
                        audit.c_score
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
write.csv( dataudit, "~/Biobank/1_AUDIT/AUDIT_reduced_data_export_20171218.csv",quote=T,row.names=F,na="#N/A")


