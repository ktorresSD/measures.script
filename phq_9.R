#########################################################################################
# Last Date modified: 12/19/2017
# Author: Katy Torres
# Description: Subset of question 23, PHQ9
##########################################################################################

#Load plyr library
 library(plyr)

#To the user: Set path to where data is stored
setwd("~/Biobank/data")


#Read all data
 dat0 <- read.csv('joined_data_export_20171218.csv',header=T,na.strings=c(NA,999))

#Only retain relevant variables
 datphq9 <- subset(dat0, 
               select= c(assessment_id,vista_lastname,
               dep1_interest,
               dep2_down,
               dep3_sleep,
               dep4_tired,
               dep5_appetite,
               dep6_feelbad,
               dep7_concentrate,
               dep8_moveslow,
               dep9_dead,
               dep10_difficult,
               dep_score_phq9))
              
#Scoring function defined
phq9 <- function(x)
{

    #attach(x)
    for (v in 1:length(x)) assign(names(x)[v], x[[v]])
    
	#PHQ-9 summary score is the summation of items 1-9 (not 10)
	#Note: This function is not designed to handle NA values (subject must have complete data)

                
                
	phq9_total <- dep1_interest +
	           dep2_down +
               dep3_sleep +
               dep4_tired +
               dep5_appetite +
               dep6_feelbad +
               dep7_concentrate +
               dep8_moveslow +
               dep9_dead
        

                      
                    
   minimal_depression <- as.numeric(phq9_total <= 4)
  
   mild_depression <- as.numeric(phq9_total >= 5 & phq9_total <= 9)
   
   moderate_depression <- as.numeric(phq9_total >= 10 & phq9_total <= 14)
   
   moderately_severe_depression <- as.numeric(phq9_total >= 15 & phq9_total <= 19)
   
   severe_depression <- as.numeric(phq9_total >= 20)
    
   phq9_incomplete <- sum(c(dep1_interest ,
	           dep2_down ,
               dep3_sleep ,
               dep4_tired ,
               dep5_appetite ,              
               dep6_feelbad ,
               dep7_concentrate ,
               dep8_moveslow ,
               dep9_dead),na.rm=T)
                
    if(is.na(phq9_incomplete > 14))
    {
     phq9_treatment <- NA
    } else if (phq9_incomplete > 14)
    {
     phq9_treatment <- 1
    } else if (is.na(phq9_total))
    {
     phq9_treatment <- NA
    } else if (phq9_total <= 14)
    {
     phq9_treatment <- 0
    }
    
    data_complete_phq9 <- as.numeric( 
                sum(
                    is.na(
                    c(dep1_interest ,	           dep2_down ,               dep3_sleep ,
               dep4_tired ,               dep5_appetite ,
               dep6_feelbad ,              dep7_concentrate ,              dep8_moveslow ,               dep9_dead ,
               dep10_difficult
               )
                    )
                ) == 0
                )
                
    scoresphq <- data.frame(phq9_total,minimal_depression,moderate_depression,moderately_severe_depression,severe_depression,phq9_treatment,data_complete_phq9)
    
	return(scoresphq)
    
}


#Calculate summary scores in data 
 phq9_scores <- adply(datphq9, 1, phq9)

#Export data
 write.csv( phq9_scores, "~/Biobank/23_PHQ9/phq9_reduced_data_export_20171219.csv",quote=T,row.names=F,na="#N/A")


