#########################################################################################
# Last Date modified: 12/19/2017
# Author: Katy Torres
# Description: Subset of question 22, PHQ15_22
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
datphq15 <- subset(dat0, 
              select= c(assessment_id,vista_lastname,
                        health1_stomach,
                        health2_back,
                        health3_arm,
                        health4_cramp,
                        health5_headache,
                        health6_chest,
                        health7_dizzy,
                        health8_faint,
                        health9_heart,
                        health10_breath,
                        health11_sex,
                        health12_constipation,
                        health13_nausea,
                        health14_tired,
                        health15_sleeping,
                        
                        health_score_phq15,
                        health_score_phq14
              ))
#________________________________________________________________________________________
# Data Manipulation and cleaning
#----------------------------------------------------------------------------------------

#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------
score <- function(x)
{

    #attach(x)
    for (v in 1:length(x)) assign(names(x)[v], x[[v]])

	#PHQ-15 summary score is the summation of items 1-15
	#Note: This function is not designed to handle NA values (subject must have complete data)


  phq15_score_total <-  health1_stomach+
	health2_back+
	health3_arm+
	health4_cramp+
	health5_headache+
	health6_chest+
	health7_dizzy+
	health8_faint+
	health9_heart+
	health10_breath+
	health11_sex+
	health12_constipation+
	health13_nausea+
	health14_tired+
	health15_sleeping


	phq15_minimal <- as.numeric(phq15_score_total <= 4)
	phq15_low <- as.numeric(phq15_score_total >= 5 & phq15_score_total <= 9)
	phq15_medium <- as.numeric(phq15_score_total >= 10 & phq15_score_total <= 14)
	phq15_high<- as.numeric(phq15_score_total >= 15)


   score_incomplete_phq15 <- sum(c(health1_stomach,
                             health2_back,
                             health3_arm,
                             health4_cramp,
                             health5_headache,
                             health6_chest,
                             health7_dizzy,
                             health8_faint,
                             health9_heart,
                             health10_breath,
                             health11_sex,
                             health12_constipation,
                             health13_nausea,
                             health14_tired,
                             health15_sleeping),na.rm=T)


    data_complete_phq15 <- as.numeric(
                sum(
                    is.na(
                    c(health1_stomach,
                      health2_back,
                      health3_arm,
                      health4_cramp,
                      health5_headache,
                      health6_chest,
                      health7_dizzy,
                      health8_faint,
                      health9_heart,
                      health10_breath,
                      health11_sex,
                      health12_constipation,
                      health13_nausea,
                      health14_tired,
                      health15_sleeping
               )
                    )
                ) == 0
                )

    scoresphq15 <- data.frame(phq15_score_total, phq15_minimal, phq15_low, phq15_medium, phq15_high, score_incomplete_phq15, data_complete_phq15)

	return(scoresphq15)
}


#Calculate summary scores in data
 score_datphq15 <- adply(datphq15 , 1, score)


#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
write.csv( score_datphq15, "~/Biobank/22_PHQ-15/PHQ15_22_reduced_data_export_20171219.csv",quote=T,row.names=F,na="#N/A")


