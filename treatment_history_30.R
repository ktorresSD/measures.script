#########################################################################################
# Last Date modified: 12/19/2017
# Author: Katy Torres
# Description: Subset of question 30, Treatment History
##########################################################################################

#Load plyr library
library(plyr)

#To the user: Set path to where data is stored
setwd("~/Biobank/data")

#________________________________________________________________________________________
#READ AND SUBSET LARGE DATA TO ONLY CONTAIN DESIRED QUESTIONAIRE VARIABLES
#----------------------------------------------------------------------------------------
#Read all data
dat0 <- read.csv('joined_data_export_20171219.csv',header=T,na.strings=c(NA,999))

#Only retain relevant variables
dattreatment <- subset(dat0, 
              select= c(assessment_id,vista_lastname,
                        TreatHis_1_treatment,
                        TreatHis_2_depression,
                        TreatHis_2_anxiety,
                        TreatHis_2_ptsd,
                        TreatHis_2_schizo,
                        TreatHis_2_bipolar,
                        TreatHis_2_substance,
                        TreatHis_2_other,
                        TreatHis_2_none,
                        TreatHis_3_VAtreat,
                        TreatHis_4_clinic,
                        TreatHis_5a_Antidep,
                        TreatHis_5b_moodstab,
                        TreatHis_5c_stimulant,
                        TreatHis_5d_sleepaid,
                        TreatHis_5e_Benzo,
                        TreatHis_5f_Antipsy,
                        TreatHis_5g_AdBlk,
                        TreatHis_5h_other,
                        TreatHis_6_presc,
                        TreatHis_7_helpful,
                        TreatHis_8_discont,
                        TreatHis_9_Psycho,
                        TreatHis_9_therapy,
                        TreatHis_9_VAtherap,
                        TreatHis_10_gpdates,
                        TreatHis_10_gphelp,
                        TreatHis_10_gpcomp,
                        TreatHis_10_gpCBTA,
                        TreatHis_10_gpCBTD,
                        TreatHis_10_gpCBTI,
                        TreatHis_10_gpIRTNM,
                        TreatHis_10_gpCBTBP,
                        TreatHis_10_gpCPT,
                        TreatHis_10_gpAM,
                        TreatHis_10_gpACT,
                        TreatHis_10_gpother,
                        TreatHis_11_indCBTA,
                        TreatHis_11_indCBTD,
                        TreatHis_11_indCBTI,
                        TreatHis_11_indIRTNM,
                        TreatHis_11_indCPT,
                        TreatHis_11_indPE,
                        TreatHis_11_indEMDR,
                        TreatHis_11_indACT,
                        TreatHis_11_indAM,
                        TreatHis_11_indother,
                        TreatHis_11_inddates,
                        TreatHis_11_indhelp,
                        TreatHis_11_indcomp,
                        TreatHis_12_OtherTX,
                        TreatHis_12_OtherDate
              ))

#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
#write.csv( TreatHis_scores, "TreatHis_reduced_data_export_08132017.csv",quote=T,row.names=F,na="#N/A")
write.csv( dattreatment, "~/Biobank/30_treatment_history/TreatHis_reduced_data_export_20171219.csv",quote=T,row.names=F,na="#N/A")


