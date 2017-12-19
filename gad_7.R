#########################################################################################
# Last Date modified: 12/19/2017
# Author: Katy Torres
# Description: Subset of question 14, GAD7 scoring functions
##########################################################################################

#Load plyr library
 library(plyr)

#To the user: Set path to where data is stored
 setwd('~/Desktop/biobank/analysis')


#Read all data
 dat0 <- read.csv('joined_data_export_20171218.csv',header=T,na.strings=c(NA,999))

#Only retain relevant variables
 datgad <- subset(dat0, 
               select= c(assessment_id,vista_lastname,
               gad1_nervous,
               gad2_notable,
               gad3_worry,
               gad4_trouble,
               gad5_restless,
               gad6_annoyed,
               gad7_afraid,
               gad8_difficult,
                          gad7_score))
              
#Scoring function defined
gad7 <- function(x)
{

    #attach(x)
    for (v in 1:length(x)) assign(names(x)[v], x[[v]])
    
	#GAD-7 summary score is the summation of items 1-7 (not 8)
	#Note: This function is not designed to handle NA values (subject must have complete data)

                           
	gad7_total <- gad1_nervous +
               gad2_notable +
               gad3_worry +
               gad4_trouble +
               gad5_restless +
               gad6_annoyed +
               gad7_afraid
        

                      
                    
  
  
   mild_anxiety <- as.numeric(gad7_total >= 5 & gad7_total <= 9)
   
   moderate_anxiety <- as.numeric(gad7_total >= 10 & gad7_total <= 14)
   
   severe_anxiety <- as.numeric(gad7_total >= 15)
   
    
   gad7_incomplete <- sum(c(gad1_nervous ,
	           gad2_notable ,
               gad3_worry ,
               gad4_trouble ,
               gad5_restless ,              
               gad6_annoyed ,
               gad7_afraid),na.rm=T)
                
    if(is.na(gad7_incomplete) )
    {
     gad7_poss_dx <- NA
    } else if(gad7_incomplete >= 10)
    {
     gad7_poss_dx <- 1
    } else if(is.na(gad7_total))
    {
     gad7_poss_dx <- NA
    } else if (gad7_total < 10)
    {
     gad7_poss_dx <- 0
    } else if (gad7_incomplete < 10 )
    {
     gad7_poss_dx <- NA
    } 
  


    
    data_complete_gad <- as.numeric( 
                sum(
                    is.na(
                    c(gad1_nervous,
               gad2_notable,
               gad3_worry,
               gad4_trouble,
               gad5_restless,
               gad6_annoyed,
               gad7_afraid,
               gad8_difficult
               )
                    )
                ) == 0
                )
                
    gadscores <- data.frame(gad7_total, mild_anxiety, moderate_anxiety,severe_anxiety,gad7_incomplete,gad7_poss_dx,data_complete_gad)
    
	return(gadscores)
}


#Calculate summary scores in data 
 gad7_scores <- adply(datgad, 1, gad7)

#Export data
 write.csv( gad7_scores, "~/Biobank/14_GAD7/gad7_reduced_data_export_20171219.csv",quote=T,row.names=F,na="#N/A")


