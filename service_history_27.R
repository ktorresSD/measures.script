#########################################################################################
# Last Date modified: 9/28/2017
# Author: Katy Torres
# Description: Subset of question 27, service history and scoring functions
##########################################################################################

#Load plyr library
library(plyr)

#To the user: Set path to where data is stored
setwd("~/Biobank/data")
#________________________________________________________________________________________
#READ AND SUBSET LARGE DATA TO ONLY CONTAIN DESIRED QUESTIONAIRE VARIABLES
#----------------------------------------------------------------------------------------
#Read all data
dat0 <- read.csv('joined_data_export_09252017_no_duplicates.csv',header=T,na.strings=c(NA,999))

#Only retain relevant variables
dat <- subset(dat0, 
              select= c(assessment_id,vista_lastname,
                        serv_oper_none,
                        serv_oper_OEF,
                        serv_oper_OIF,
                        serv_oper_gwot,
                        serv_oper_ond,
                        serv_oper_caribbean,
                        serv_oper_gulf,
                        serv_oper_somalia,
                        serv_oper_bosnia,
                        serv_oper_kosovo,
                        serv_oper_djibouti,
                        serv_oper_libya,
                        Serv_oper_vietnam,
                        serv_oper_korea,
                        serv_oper_other,
                        serv_oper_other1spec,
                        serv_oper_other,
                        serv_oper_other2spec,
                        serv,
                        
                        #serv_type,
                        serv_branch,
                        serv_start,
                        serv_stop,
                        serv_discharge,
                        serv_rank,
                        serv_job,
                        
                        serv_type_0,                    
                        serv_branch_0,                 
                        serv_start_0,                   
                        serv_stop_0,                   
                        serv_discharge_0,               
                        serv_rank_0,                   
                        serv_job_0,  
                        
                        serv_type_1,                   
                        serv_branch_1,                  
                        serv_start_1,                  
                        serv_stop_1,                    
                        serv_discharge_1,              
                        serv_rank_1,                    
                        serv_job_1,
                        
                        serv_count
                        
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
write.csv( dat, "~/Biobank/27_service_history/service_history_reduced_data_export_09282017.csv",quote=T,row.names=F,na="#N/A")


