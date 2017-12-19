#########################################################################################
# Last Date modified: 12/19/2017
# Author: Katy Torres
# Description: Subset of question 28, Demographic: Social Environment
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
datdemosocial <- subset(dat0, 
              select= c(assessment_id,vista_lastname,
                        demo_livewith_alone,
                        demo_livewith_parent,
                        demo_livewith_friend,
                        demo_livewith_spouse,
                        demo_livewith_child,
                        demo_livewith_other,
                        demo_livewith_otherspec,
                        
                        demo_emo_none,
                        demo_emo_parents,
                        demo_emo_friends,
                        demo_emo_spouse,
                        demo_emo_therapist,
                        demo_emo_spiritual,
                        demo_emo_children,
                        demo_emo_other,
                        demo_emo_other_spec,
                        demo_rel_hurt,
                        child_agegroup,
                        demo_children,
                        child_count
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
write.csv( datdemosocial, "~/Biobank/28_Demo_Social/Demographic_social_reduced_data_export_20171219.csv",quote=T,row.names=F,na="#N/A")


