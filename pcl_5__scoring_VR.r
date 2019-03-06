#########################################################################################
# Last Date modified: 12/14/2017
# Author: Katy Torres
# Description: PCL lifetime DX
##########################################################################################

#Load plyr library
 library(plyr)
#read in all files
 setwd("C:/Users/Psychiatry Lab/Documents/GWAS_Freeze2_CURRENT/58_Risbrough_rutgers/phenotypes")
 
 
 #________________________________________________________________________________________
 #READ AND SUBSET LARGE DATA TO ONLY CONTAIN DESIRED QUESTIONAIRE VARIABLES
 #----------------------------------------------------------------------------------------
 #Read all data
 vr.dat<- read.csv('./phenotype_20171121.csv',header=T,na.strings=c(NA,"#N/A"))
 
#Only retain relevant variables
 dat <- subset(vr.dat, 
               select= c(subnum, 
                 ssn1PCL5_1,
                 ssn1PCL5_2,
                    ssn1PCL5_3,
                    ssn1PCL5_4,
                    ssn1PCL5_5,
                    ssn1PCL5_6,
                    ssn1PCL5_7,
                    ssn1PCL5_8,
                    ssn1PCL5_9,
                    ssn1PCL5_10,
                    ssn1PCL5_11,
                    ssn1PCL5_12,
                    ssn1PCL5_13,
                    ssn1PCL5_14,
                    ssn1PCL5_15,
                    ssn1PCL5_16,
                    ssn1PCL5_17,
                    ssn1PCL5_18,
                    ssn1PCL5_19,
                    ssn1PCL5_20, 
                    ssn1PCL5_21
                 ))

             
#Scoring function defined
pcl_5_entire_life <- function(x)
{

    #attach(x)
    for (v in 1:length(x)) assign(names(x)[v], x[[v]])
    
	#PCL summary score is just the summation of all items 1-20
	#Note: This function is not designed to handle NA values (subject must have complete data)

                
                
	pcl_b <- ssn1PCL5_1 +
        ssn1PCL5_2 +
        ssn1PCL5_3 +
        ssn1PCL5_4 +
        ssn1PCL5_5 
        
    pcl_c <- ssn1PCL5_6 +
        ssn1PCL5_7 
        
    pcl_d <-  ssn1PCL5_8 +
        ssn1PCL5_9 +
        ssn1PCL5_10 +
        ssn1PCL5_11 +
        ssn1PCL5_12 +
        ssn1PCL5_13 +
        ssn1PCL5_14 
        
    pcl_e <- ssn1PCL5_15 +
        ssn1PCL5_16 +
        ssn1PCL5_17 +
        ssn1PCL5_18 +
        ssn1PCL5_19 +
        ssn1PCL5_20
    
    pcl_total <- pcl_b + pcl_c + pcl_d + pcl_e
    

                    
                    
   pcl_33 <- as.numeric(pcl_total >= 33)
    
   pcl_incomplete <- sum(ssn1PCL5_1,
                ssn1PCL5_2,
                ssn1PCL5_3,
                ssn1PCL5_4,
                ssn1PCL5_5,
                ssn1PCL5_6,
                ssn1PCL5_7,
                ssn1PCL5_8,
                ssn1PCL5_9,
                ssn1PCL5_10,
                ssn1PCL5_11,
                ssn1PCL5_12,
                ssn1PCL5_13,
                ssn1PCL5_14,
                ssn1PCL5_15,
                ssn1PCL5_16,
                ssn1PCL5_17,
                ssn1PCL5_18,
                ssn1PCL5_19,
                ssn1PCL5_20,na.rm=T)
                
    if(pcl_incomplete >= 33)
    {
     pcl_33 <- 1
    }
   
    
    ##PCL B 
    #Assign TRUE to each PCL B item score that is >= 2
    pcl_5_b_gt2 <- c(ssn1PCL5_1,ssn1PCL5_2,ssn1PCL5_3, ssn1PCL5_4, ssn1PCL5_5) >= 2
    
    #Assign TRUE if at least one PCL B is >= 2
    pcl_5_b_dsm5 <- sum(pcl_5_b_gt2) >= 1
    
    ##PCL C
    #Assign TRUE to each PCL C item score that is >= 2
    pcl_5_c_gt2 <- c(ssn1PCL5_6, ssn1PCL5_7 ) >= 2
    
    #Assign TRUE if at least one PCL C is >= 2
    pcl_5_c_dsm5 <- sum(pcl_5_c_gt2) >= 1
    
    ##PCL D 
    #Assign TRUE to each PCL D item score that is >= 2
    pcl_5_d_gt2 <- c(ssn1PCL5_8 ,
        ssn1PCL5_9 ,
        ssn1PCL5_10 ,
        ssn1PCL5_11 ,
        ssn1PCL5_12 ,
        ssn1PCL5_13 ,
        ssn1PCL5_14  ) >= 2
    
    #Assign TRUE if at least two PCL Ds are >= 2
    pcl_5_d_dsm5 <- sum(pcl_5_d_gt2) >= 2
    
    ##PCL E 
    #Assign TRUE to each PCL E item score that is >= 2
    pcl_5_e_gt2 <- c(ssn1PCL5_15 ,
        ssn1PCL5_16 ,
        ssn1PCL5_17 ,
        ssn1PCL5_18 ,
        ssn1PCL5_19 ,
        ssn1PCL5_20  ) >= 2
      

    
    #Assign TRUE if at least two PCL Es are >= 2
    pcl_5_e_dsm5 <- sum(pcl_5_e_gt2) >= 2
    
    #Assign TRUE if all PCL sub-symptoms are TRUE
    pcl_5_dsm <- as.numeric(
                    sum(pcl_5_b_dsm5, pcl_5_c_dsm5, pcl_5_d_dsm5, pcl_5_e_dsm5) == 4
                        )
    
    ###Infer DSM if data is incomplete
    ##PCL B 
    #Assign TRUE to each PCL B item score that is >= 2
    pcl_5_b_gt2_infer <- na.omit(c(ssn1PCL5_1,ssn1PCL5_2,ssn1PCL5_3, ssn1PCL5_4, ssn1PCL5_5)) >= 2
    
    #Assign TRUE if at least one PCL B is >= 2
    pcl_5_b_dsm5_infer <- sum(pcl_5_b_gt2_infer) >= 1
    
    ##PCL C
    #Assign TRUE to each PCL C item score that is >= 2
    pcl_5_c_gt2_infer <- na.omit(c(ssn1PCL5_6, ssn1PCL5_7 )) >= 2
    
    #Assign TRUE if at least one PCL C is >= 2
    pcl_5_c_dsm5_infer <- sum(pcl_5_c_gt2_infer) >= 1
    
    ##PCL D 
    #Assign TRUE to each PCL D item score that is >= 2
    pcl_5_d_gt2_infer <- na.omit(c(ssn1PCL5_8 ,
        ssn1PCL5_9 ,
        ssn1PCL5_10 ,
        ssn1PCL5_11 ,
        ssn1PCL5_12 ,
        ssn1PCL5_13 ,
        ssn1PCL5_14  )) >= 2
    
    #Assign TRUE if at least two PCL Ds are >= 2
    pcl_5_d_dsm5_infer <- sum(pcl_5_d_gt2_infer) >= 2
    
    ##PCL E 
    #Assign TRUE to each PCL E item score that is >= 2
    pcl_5_e_gt2_infer <- na.omit(c(ssn1PCL5_15 ,
        ssn1PCL5_16 ,
        ssn1PCL5_17 ,
        ssn1PCL5_18 ,
        ssn1PCL5_19 ,
        ssn1PCL5_20  )) >= 2
      

    
    #Assign TRUE if at least two PCL Es are >= 2
    pcl_5_e_dsm5_infer <- sum(pcl_5_e_gt2_infer) >= 2
    
    #Assign TRUE if all PCL sub-symptoms are TRUE
    pcl_5_dsm_infer <- as.numeric(
                    sum(pcl_5_b_dsm5_infer, pcl_5_c_dsm5_infer, pcl_5_d_dsm5_infer, pcl_5_e_dsm5_infer) == 4
                        )
    if(pcl_5_dsm_infer == TRUE)
    {
     pcl_5_dsm = 1
    }    

    
    data_complete <- as.numeric( 
                sum(
                    is.na(
                    c(ssn1PCL5_1,
                    ssn1PCL5_2,
                    ssn1PCL5_3,
                    ssn1PCL5_4,
                    ssn1PCL5_5,
                    ssn1PCL5_6,
                    ssn1PCL5_7,
                    ssn1PCL5_8,
                    ssn1PCL5_9,
                    ssn1PCL5_10,
                    ssn1PCL5_11,
                    ssn1PCL5_12,
                    ssn1PCL5_13,
                    ssn1PCL5_14,
                    ssn1PCL5_15,
                    ssn1PCL5_16,
                    ssn1PCL5_17,
                    ssn1PCL5_18,
                    ssn1PCL5_19,
                    ssn1PCL5_20)
                    )
                ) == 0
                )
                
    scores <- data.frame(pcl_b,pcl_c,pcl_d,pcl_e,pcl_total,pcl_33,pcl_5_dsm,data_complete)
    
	return(scores)
}


#Calculate summary scores in data 
 pcl_5_scores <- adply(dat, 1, pcl_5_entire_life)

#Export data
 write.csv(pcl_5_scores , "C:/Users/Psychiatry Lab/Documents/GWAS_Freeze2_CURRENT/58_Risbrough_rutgers/measures/PCL-5/PCL_scored_v1_R_output.csv",quote=T,row.names=F,na="#N/A")
 

