#########################################################################################
# Last Date modified: 12/20/2017
# Author: Katy Torres
# Description: TBI and scoring functions
##########################################################################################

#Load plyr library
library(plyr)

#To the user: Set path to where data is stored
setwd("~/Biobank/TBI")
#________________________________________________________________________________________
#READ AND SUBSET LARGE DATA TO ONLY CONTAIN DESIRED QUESTIONAIRE VARIABLES
#----------------------------------------------------------------------------------------
#Read all data
datTBI <- read.csv('TBI data.csv',header=T,na.strings=c(NA,999))

#________________________________________________________________________________________
# Data Manipulation and cleaning
#----------------------------------------------------------------------------------------
#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------
attach(datTBI)
vista_lastname <- Subject.ID
#if ever had injuries
injurypresent <- ifelse(X1.ever.had.injuries >=1, "yes", "no")
injurywithLOC <- ifelse(X8.lose.cosciousness ==1 | 
                               X23.lose.cosciousness ==1 | 
                               X38.lose.cosciousness ==1 |
                               X53.lose.cosciousness ==1 | 
                               X68.lose.cosciousness ==1, "yes", "no")

injurywithAMS<- ifelse(X10.symptoms.immediately ==1 |  
                              X25.symptoms.immediately ==1 | 
                              X40.symptoms.immediately ==1 | 
                              X55.symptoms.immediately ==1 | 
                              X70.symptoms.immediately ==1, "yes", "no")

injurywithLOCorAMS<- ifelse(injurywithAMS =="yes" | injurywithLOC =="yes", "yes", "no")

LOClessthan1<- ifelse(injurywithLOC ==1 & (X9.how.long == 1 | X24.how.long == 1 | X39.how.long == 1| 
                                                    X54.how.long == 1 | X69.how.long == 1), "yes", "no")

LOC2to25 <- ifelse(injurywithLOC ==1 & (X9.how.long == 2 | X24.how.long == 2 | X39.how.long == 2| 
                                                 X54.how.long == 2 | X69.how.long == 2), "yes", "no")

LOC36to30 <- ifelse(injurywithLOC ==1 & (X9.how.long == 3 | X24.how.long == 3 | X39.how.long == 3| 
                                                  X54.how.long == 3 | X69.how.long == 3), "yes", "no")
LOCmorethan30 <- ifelse(injurywithLOC ==4 & (X9.how.long == 4 | X24.how.long == 4 | X39.how.long == 4| 
                                                      X54.how.long == 4 | X69.how.long == 4), "yes", "no")

detach(datTBI)
#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
write.csv( datTBI, "~/Biobank/TBI/TBI_data_export_20171220.csv",quote=T,row.names=F,na="#N/A")

