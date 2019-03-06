#########################################################################################
# Last Date modified: 12/14/2017
# Author: Katy Torres
# Description: PCL lifetime DX
##########################################################################################

#Load plyr library
 library(plyr)
#read in all files
 setwd("C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/57_Baker_rutgers/PTSD_PCL_Case_scoring")
 
 
 #________________________________________________________________________________________
 #READ AND SUBSET LARGE DATA TO ONLY CONTAIN DESIRED QUESTIONAIRE VARIABLES
 #----------------------------------------------------------------------------------------
 #Read all data
 #bake_dat<- read.csv('./PCL4_meg_and_breecher.csv',header=T,na.strings=c(NA,"#N/A"))
 
 bake_dat<- read.csv('./PCL4_meg_and_breecher.csv',header=T,na.strings=c(NA,"#N/A"))
 
             
#Scoring function defined
pcl_4_entire_life <- function(x)
{

    #attach(x)
    for (v in 1:length(x)) assign(names(x)[v], x[[v]])
    
	#PCL summary score is just the summation of all items 1-20
	#Note: This function is not designed to handle NA values (subject must have complete data)

                
                
	pcl_b  <- cj_memories+cj_dreams+cj_reliving+cj_upset+cj_reaction
        
  pcl_ca <- cj_thinking+cj_activities
	
	pcl_cn <- cj_lossinterest+cj_distant+cj_emotionally
        
  pcl_d  <- cj_asleep+cj_irritable+cj_concentrating+cj_alert+cj_startled
        
    
  pcl_total <- cj_memories+
      cj_dreams+
      cj_reliving+
      cj_upset+
      cj_reaction+
      cj_thinking+
      cj_activities+
      cj_remembering+
      cj_lossinterest+
      cj_distant+
      cj_emotionally+
      cj_future+
      cj_asleep+
      cj_irritable+
      cj_concentrating+
      cj_alert+
      cj_startled
    
   pcl_39 <- as.numeric(pcl_total >= 39)
   pcl_44 <- as.numeric(pcl_total >= 44)
   pcl_56 <- as.numeric(pcl_total >= 56)
    
   pcl_incomplete <- sum(cj_memories,
                           cj_dreams,
                           cj_reliving,
                           cj_upset,
                           cj_reaction,
                           cj_thinking,
                           cj_activities,
                           cj_remembering,
                           cj_lossinterest,
                           cj_distant,
                           cj_emotionally,
                           cj_future,
                           cj_asleep,
                           cj_irritable,
                           cj_concentrating,
                           cj_alert,
                           cj_startled,na.rm=T)
                
    if(pcl_incomplete >= 39)
    {
     pcl_39 <- 1
    }
   
   if(pcl_incomplete >= 44)
   {
     pcl_44 <- 1
   }
   
   if(pcl_incomplete >= 56)
   {
     pcl_56 <- 1
   }
    
    ##PCL B 
    #Assign TRUE to each PCL B item score that is >= 2
    pcl_5_b_gt2 <- c(cj_memories,cj_dreams, cj_reliving, cj_upset,cj_reaction) > 2
    
    #Assign TRUE if at least one PCL B is >= 2
    pcl_4_b_dsm4 <- sum(pcl_5_b_gt2) >= 1
    
    ##PCL C
    #Assign TRUE to each PCL C item score that is >= 2
    pcl_5_c_gt2 <- c(cj_thinking, cj_activities,cj_remembering, cj_lossinterest, cj_distant, cj_emotionally,cj_future) > 2
    
    #Assign TRUE if at least one PCL C is >= 2
    pcl_4_c_dsm4 <- sum(pcl_5_c_gt2) >= 3
    
    ##PCL D 
    #Assign TRUE to each PCL D item score that is >= 2
    pcl_5_d_gt2 <- c(cj_asleep,cj_irritable,cj_concentrating,cj_alert,cj_startled ) > 2
    
    #Assign TRUE if at least two PCL Ds are >= 2
    pcl_4_d_dsm4 <- sum(pcl_5_d_gt2) >= 2


    #Assign TRUE if all PCL sub-symptoms are TRUE
    pcl_4_dsm <- as.numeric(
                    sum(pcl_4_b_dsm4, pcl_4_c_dsm4, pcl_4_d_dsm4) == 3
                        )
    


    
    data_complete <- as.numeric( 
                sum(
                    is.na(
                    c(cj_memories,
                        cj_dreams,
                        cj_reliving,
                        cj_upset,
                        cj_reaction,
                        cj_thinking,
                        cj_activities,
                        cj_remembering,
                        cj_lossinterest,
                        cj_distant,
                        cj_emotionally,
                        cj_future,
                        cj_asleep,
                        cj_irritable,
                        cj_concentrating,
                        cj_alert,
                        cj_startled)
                    )
                ) == 0
                )
                
    scores <- data.frame(pcl_b,pcl_ca, pcl_cn,pcl_d,pcl_total,pcl_39, pcl_44, pcl_56,pcl_4_dsm,data_complete)
    
	return(scores)
}


#Calculate summary scores in data 
 pcl_4_scores <- adply(bake_dat, 1, pcl_4_entire_life)

#Export data
write.csv(pcl_4_scores , "C:/Users/Nievergelt Lab/Documents/GWAS_Freeze2_CURRENT/57_Baker_rutgers/PCL4_scored_R_output.csv",quote=T,row.names=F,na="#N/A")
 

