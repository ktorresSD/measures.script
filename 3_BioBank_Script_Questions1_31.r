#########################################################################################
# Last Date modified: 1/24/2018 (only change this date if the scoring code itself is modified)
# Author: Katy Torres
# Description: BIOBANK CODE FOR QUESTIONS 1-31
##########################################################################################

#Load required libraries
library(plyr)

#Note to the user: Set path to where data is stored
setwd("~/Biobank/data")

#Read all data
#Change this location to be where master data is currently stored
dat0 <- read.csv('joined_data_export_20180212_2.csv',header=T,na.strings=c(NA,999))

# !!! STOP !!!!

#Now replace all "20180212" with today's date!
#Do this with ctrl + F
#replace all dates from here down (not the date of the masted data file)

#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 1, AUDIT
##########################################################################################
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
#Export data
#----------------------------------------------------------------------------------------
write.csv( dataudit, "~/Biobank/1_AUDIT/AUDIT_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")

#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 2, BAT-L interval (head injury)
##########################################################################################
#________________________________________________________________________________________
#READ AND SUBSET LARGE DATA TO ONLY CONTAIN DESIRED QUESTIONAIRE VARIABLES
#----------------------------------------------------------------------------------------

#Only retain relevant variables
datbatli <- subset(dat0, 
              select= c(assessment_id,vista_lastname,
                        BATL_Int_headinj,BATL_Int_howinj,BATL_timeuncon
              ))

#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
write.csv(datbatli , "~/Biobank/2_BAT-L_interval/BAT-L_interval_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")

#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 3, BAT-L 
##########################################################################################

#Only retain relevant variables
 dat_BATL <- subset(dat0, 
               select= c(assessment_id,vista_lastname,
               BATL1_crash,
               BATL2_motor,
               BATL3_terr,
               BATL4_ped,
               BATL5_obj,
               BATL6_equip,
               BATL7_stairs,
               BATL8_high,
               BATL9_faint,
               BATL10_drug,
               BATL11_bike,
               BATL12_roll,
               BATL13_horse,
               BATL14_ski,
               BATL15_sky,
               BATL16_sport,
               BATL17_play,
               BATL18_water,
               BATL19_abuse,
               BATL20_mugg,
               BATL21_mil,
               BATL22_comb,
               BATL23_other,
               BATL25_time,
               BATL24_worst))
               
               
               
              
#Scoring function defined
batl <- function(x)
{

    #attach(x)
    for (v in 1:length(x)) assign(names(x)[v], x[[v]])
    
	#BATL summary score is the summation of items 1-23
         
                
	batl_total <- BATL1_crash +
               BATL2_motor +
               BATL3_terr +
               BATL4_ped +
               BATL5_obj +
               BATL6_equip +
               BATL7_stairs +
               BATL8_high +
               BATL9_faint +
               BATL10_drug +
               BATL11_bike +
               BATL12_roll +
               BATL13_horse +
               BATL14_ski +
               BATL15_sky +
               BATL16_sport +
               BATL17_play +
               BATL18_water +
               BATL19_abuse +
               BATL20_mugg +
               BATL21_mil +
               BATL22_comb +
               BATL23_other 

    
	batl_total_incomplete <- sum(c(BATL1_crash ,BATL2_motor ,BATL3_terr ,
	                               BATL4_ped ,BATL5_obj ,BATL6_equip ,
	                               BATL7_stairs ,BATL8_high ,BATL9_faint ,
	                               BATL10_drug , BATL11_bike ,BATL12_roll ,
	                               BATL13_horse ,BATL14_ski ,BATL15_sky ,
	                               BATL16_sport ,BATL17_play ,BATL18_water ,
	                               BATL19_abuse ,BATL20_mugg ,BATL21_mil ,
	                               BATL22_comb ,BATL23_other),na.rm=T)
                    
   severe_tbi <- as.numeric(BATL25_time == 4)
   batl_quality_flag = NA
   if (is.na(BATL24_worst) | is.na(BATL25_time))
   {
   	batl_quality_flag = NA
   } else if(BATL24_worst == 0 & BATL25_time > 0 )
   {
     batl_quality_flag = 1
   } else if (BATL24_worst >  0 & BATL25_time > 0 )
   {
   	batl_quality_flag = 0
   } else if (BATL24_worst == 0 & BATL25_time == 0 )
   {
   	batl_quality_flag = 0
   } 
  
   data_complete_batl<- as.numeric(
     sum(
       is.na(
         c(BATL1_crash ,
             BATL2_motor ,
             BATL3_terr ,
             BATL4_ped ,
             BATL5_obj ,
             BATL6_equip ,
             BATL7_stairs ,
             BATL8_high ,
             BATL9_faint ,
             BATL10_drug ,
             BATL11_bike ,
             BATL12_roll ,
             BATL13_horse ,
             BATL14_ski ,
             BATL15_sky ,
             BATL16_sport ,
             BATL17_play ,
             BATL18_water ,
             BATL19_abuse ,
             BATL20_mugg ,
             BATL21_mil ,
             BATL22_comb ,
             BATL23_other
         )
       )
     ) == 0
   )
                
    scores <- data.frame(batl_total,severe_tbi,batl_quality_flag, batl_total_incomplete, data_complete_batl)
    
	return(scores)
}


#Calculate summary scores in data 
 batl_scores <- adply(dat_BATL, 1, batl)

#Export data
 write.csv( batl_scores, "~/Biobank/3_BAT-L/batl_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")


#########################################################################################
# Last datBTBISe modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 4, 4_BTBIS
##########################################################################################
#Only retain relevant variables
datBTBIS <- subset(dat0, select= c(assessment_id,vista_lastname,
                              tbi_blast_BB,
                              tbi_vehicle_BB,
                              tbi_fragment_BB,
                              tbi_fall_BB,
                              tbi_blow_BB,
                              tbi_otherinj_BB,
                              tbi_none_BB,
                              
                              tbi_immed_loss_BB,
                              tbi_immed_dazed_BB,
                              tbi_immed_memory_BB,
                              tbi_immed_concussion_BB,
                              tbi_immed_headinj_BB,
                              tbi_immed_none_BB,
                              tbi_immed_na_BB,
                              
                              tbi_worse_memory_BB,
                              tbi_worse_balance_BB,
                              tbi_worse_light_BB,
                              tbi_worse_irritable_BB,
                              tbi_worse_headache_BB,
                              tbi_worse_sleep_BB,
                              tbi_worse_none_BB,
                              tbi_worse_na_BB,
                              
                              tbi_week_memory_BB,
                              tbi_week_balance_BB,
                              tbi_week_light_BB,
                              tbi_week_irritable_BB,
                              tbi_week_headache_BB,
                              tbi_week_sleep_BB,
                              tbi_week_none_BB,
                              tbi_week_na_BB
              ))

#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------
score_BTBIS <- function(x)
{
  
  #attach(x)
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
tbi_tot_week_symotoms <- tbi_blast_BB+
                               tbi_vehicle_BB+
                               tbi_fragment_BB+
                               tbi_fall_BB+
                               tbi_blow_BB+
                               tbi_otherinj_BB+
                               tbi_none_BB+
                               
                               tbi_immed_loss_BB+
                               tbi_immed_dazed_BB+
                               tbi_immed_memory_BB+
                               tbi_immed_concussion_BB+
                               tbi_immed_headinj_BB+
                               tbi_immed_none_BB+
                               tbi_immed_na_BB+
                               
                               tbi_worse_memory_BB+
                               tbi_worse_balance_BB+
                               tbi_worse_light_BB+
                               tbi_worse_irritable_BB+
                               tbi_worse_headache_BB+
                               tbi_worse_sleep_BB+
                               tbi_worse_none_BB+
                               tbi_worse_na_BB+
                               
                               tbi_week_memory_BB+
                               tbi_week_balance_BB+
                               tbi_week_light_BB+
                               tbi_week_irritable_BB+
                               tbi_week_headache_BB+
                               tbi_week_sleep_BB+
                               tbi_week_none_BB+
                               tbi_week_na_BB

BTBIS_total_incomplete <- sum(c(tbi_blast_BB,tbi_vehicle_BB,tbi_fragment_BB,tbi_fall_BB,tbi_blow_BB,
                                tbi_otherinj_BB,tbi_none_BB, tbi_immed_loss_BB,tbi_immed_dazed_BB,tbi_immed_memory_BB,
                                tbi_immed_concussion_BB,tbi_immed_headinj_BB,tbi_immed_none_BB,tbi_immed_na_BB,tbi_worse_memory_BB,
                                tbi_worse_balance_BB,tbi_worse_light_BB,tbi_worse_irritable_BB,tbi_worse_headache_BB,tbi_worse_sleep_BB,
                                tbi_worse_none_BB,tbi_worse_na_BB,tbi_week_memory_BB,tbi_week_balance_BB,tbi_week_light_BB,
                                tbi_week_irritable_BB,tbi_week_headache_BB,tbi_week_sleep_BB, tbi_week_none_BB, tbi_week_na_BB),na.rm=T)

data_complete_btbis<- as.numeric(
  sum(
    is.na(
      c(tbi_blast_BB,tbi_vehicle_BB,tbi_fragment_BB,tbi_fall_BB,tbi_blow_BB,
        tbi_otherinj_BB,tbi_none_BB, tbi_immed_loss_BB,tbi_immed_dazed_BB,tbi_immed_memory_BB,
        tbi_immed_concussion_BB,tbi_immed_headinj_BB,tbi_immed_none_BB,tbi_immed_na_BB,tbi_worse_memory_BB,
        tbi_worse_balance_BB,tbi_worse_light_BB,tbi_worse_irritable_BB,tbi_worse_headache_BB,tbi_worse_sleep_BB,
        tbi_worse_none_BB,tbi_worse_na_BB,tbi_week_memory_BB,tbi_week_balance_BB,tbi_week_light_BB,
        tbi_week_irritable_BB,tbi_week_headache_BB,tbi_week_sleep_BB, tbi_week_none_BB, tbi_week_na_BB
      )
    )
  ) == 0
)

scores <- data.frame(tbi_tot_week_symotoms, BTBIS_total_incomplete, data_complete_btbis )

return(scores)
}

#Calculate summary scores in datBTBISa
score_datBTBIS <- adply(datBTBIS, 1, score_BTBIS)

#________________________________________________________________________________________ 
#Export datBTBISa
#----------------------------------------------------------------------------------------
write.csv( score_datBTBIS, "~/Biobank/4_BTBIS/BTBIS_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")


#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 5, Basic Demographic and scoring functions
##########################################################################################
#Only retain relevant variables
datdemo <- subset(dat0, 
              select= c(assessment_id,vista_lastname,
                        vista_lastname,
                        demo_gender_r,
                        demo_YOB_r,
                        demo_weight_r,
                        demo_heightft_r,
                        demo_heightinch_r,
                        demo_ethnic_r,
                        
                        demo_racewhite,
                        demo_race_black,
                        demo_race_amind,
                        demo_race_pacisl,
                        demo_race_asian,
                        demo_race_decline,
                        demo_race_oth,
                        
                        demo_relationship_r
                        
              ))

#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
write.csv( datdemo, "~/Biobank/5_Basic_Demographic/Basic_Demographic_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")

#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 6,basic_pain
##########################################################################################
#Only retain relevant variables
datpain <- subset(dat0, select= c(assessment_id,vista_lastname,
                              pain_area_text,
                              pain_area,
                              pain_number,
                              pain_area_count,
                              pain_formula
                              
              ))

#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------

datpain$pain_score<-ifelse(datpain$pain_number >=4, TRUE, FALSE)

#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
write.csv(datpain, "~/Biobank/6_basic_pain/basic_pain_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")


#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 7, Brief CESAMH Biorepository Sample Status Survey
##########################################################################################
#Only retain relevant variables
datcesamh<- subset(dat0, 
              select= c(assessment_id,vista_lastname,
                        Stat1_cold,
                        Stat2_nic,
                        
                        Stat2_nicroute_chew,
                        Stat2_nicroute_cig,
                        Stat2_nicroute_vap,
                        Stat2_nicroute_inh,
                        Stat2_nicroute_pat,
                        Stat2_nictime,
                        Stat3_eat,
                        Stat4_caff,
                        Stat5_alc,
                        Stat6_diet,
                        
                        
                        Stat7_med_blood,
                        Stat7_med_statin,
                        Stat7_med_heart,
                        Stat7_med_diabetes,
                        Stat7_med_psych,
                        Stat7_med_pain,
                        Stat7_med_sleep,
                        Stat7_med_antibiotics,
                        Stat7_med_prost,
                        Stat7_med_allerg,
                        Stat7_med_steroid,
                        
                        Stat8_marijtoday,
                        Stat9_cannabisweek,
                        Stat10_bed_night,
                        Stat10_bed_morn,
                        Stat10_bed_wakeupnight
                        
              ))
#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
write.csv( datcesamh, "~/Biobank/7_Brief_CESAMH_Biorepository_Survey/Brief_CESAMH_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")


#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 8, CDDR and scoring functions
##########################################################################################
#Only retain relevant variables
datcddr <- subset(dat0, 
              select= c(assessment_id,vista_lastname,
                        CDDR_AgeFirstUse,
                        CDDR_EverSmoked,
                        CDDR_AgeFirstReg,
                        CDDR_RegUse,
                        CDDR_Method,
                        CDDR_Quantity,
                        CDDR_THC,
                        CDDR_LastUse,
                        CDDR_Lifetime,
                        CDDR_Year,
                        CDDR_Month,
                        CDDR_5year
                        
              ))

#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
write.csv( datcddr, "~/Biobank/8_CDDR/CDDR_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")

#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 9, CURRENT TREATMENTS
##########################################################################################
#Only retain relevant variables
 datcurrent <- subset(dat0, 
               select= c(assessment_id,vista_lastname,
                         CurrentTreatments1_MHTx,	
                         CurrTx2a_Dep,	CurrTx2b_Anx,	
                         CurrTx2c_PTSD,	CurrTx2d_Schiz,	
                         CurrTx2e_BP,	
                         CurrTx2f_Subst,	 CurrTx2g_Other,	
                         CurrentTreatments3_VA,	
                         CurrTx4a_LJBHIP,	
                         CurrTx4b_LJPTSD,	
                         CurrTx4c_LJMood,	
                         CurrTx4d_Other,	
                         CurrTx5a_AntiD,
                         CurrTx5b_Mood,	
                         CurrTx5c_Stim,
                         CurrTx5d_Sleep,
                         CurrTx5e_Benzo,	
                         CurrTx5f_AntiPsy,
                         CurrTx5g_Adren, 
                         CurrTx5h_Other, 
                         CurrTx5.1_VAProvider, 
                         CurrTx5.2_Helpful, 
                         CurrTx5.3_SideEffects, 
                         CurrentTreatments6_Psycho, 
                         CurrTx6.1_Group, 
                         CurrTx6.1_Ind, 
                         CurrTx6.1_Fam, 
                         CurrTx6.1_Couples, 
                         CurrTx6.2_AtVA, 
                         CurrTx7a_CBTAnx, 
                         CurrTx7b_CBTDep, 
                         CurrTx7c_CBTInsom, 
                         CurrTx7d_IRTNM, 
                         CurrTx7e_CBTBP, 
                         CurrTx7f_CPT, 
                         CurrTx7g_Anger, 
                         CurrTx7h_ACT, 
                         CurrTx7i_Other, 
                         CurrTx7.1_Helpful, 
                         CurrTx8a_CBTAnx, 
                         CurrTx8b_CBTDep, 
                         CurrTx8c_CBTInsom, 
                         CurrTx8d_IRTNM, 
                         CurrTx8e_CPT, 
                         CurrTx8f_PE, 
                         CurrTx8g_EMDR, 
                         CurrTx8h_ACT, 
                         CurrTx8g_Anger, 
                         CurrTx8i_Other, 
                         CurrTx8.1_Helpful, 
                         CurrentTreatments9_OtherType
               ))
              

#Export data
 write.csv( datcurrent, "~/Biobank/9_current_treatments/CurrTx_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")

#########################################################################################
# Last datBTBISe modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 10, DRRI_CES
##########################################################################################
#Only retain relevant variables
 datces <- subset(dat0, 
               select= c(assessment_id,vista_lastname,
                         DRRICE1,
                         DRRICE2,
                         DRRICE3,
                         DRRICE4,
                         DRRICE5,
                         DRRICE6,
                         DRRICE7,
                         DRRICE8,
                         DRRICE9,
                         DRRICE10,
                         DRRICE11,
                         DRRICE12,
                         DRRICE13,
                         DRRICE14,
                         DRRICE15,
                         DRRICE16,
                         DRRICE17
                         
               ))
#________________________________________________________________________________________              
#Scoring function
#----------------------------------------------------------------------------------------
#Calculate summary scores in data
 score_ces <- function(x)
 {
   
   #attach(x)
   for (v in 1:length(x)) assign(names(x)[v], x[[v]])
   
   DRRI_CES_scores_total<- sum(c(DRRICE1,
                                             DRRICE2,
                                             DRRICE3,
                                             DRRICE4,
                                             DRRICE5,
                                             DRRICE6,
                                             DRRICE7,
                                             DRRICE8,
                                             DRRICE9,
                                             DRRICE10,
                                             DRRICE11,
                                             DRRICE12,
                                             DRRICE13,
                                             DRRICE14,
                                             DRRICE15,
                                             DRRICE16,
                                             DRRICE17),na.rm=F)

   DRRI_CES_scores_total_incomplete <- sum(c(DRRICE1,
                                   DRRICE2,
                                   DRRICE3,
                                   DRRICE4,
                                   DRRICE5,
                                   DRRICE6,
                                   DRRICE7,
                                   DRRICE8,
                                   DRRICE9,
                                   DRRICE10,
                                   DRRICE11,
                                   DRRICE12,
                                   DRRICE13,
                                   DRRICE14,
                                   DRRICE15,
                                   DRRICE16,
                                   DRRICE17),na.rm=T)

   data_complete_ces<- as.numeric(
     sum(
       is.na(
         c(DRRICE1,
           DRRICE2,
           DRRICE3,
           DRRICE4,
           DRRICE5,
           DRRICE6,
           DRRICE7,
           DRRICE8,
           DRRICE9,
           DRRICE10,
           DRRICE11,
           DRRICE12,
           DRRICE13,
           DRRICE14,
           DRRICE15,
           DRRICE16,
           DRRICE17
         )
       )
     ) == 0
   )
   #detach(x)
   scoresces <- data.frame( DRRI_CES_scores_total, DRRI_CES_scores_total_incomplete, data_complete_ces)
   
   return(scoresces)
  
 }
 
 #Calculate summary scores in datBTBISa
 score_datces <- adply(datces, 1, score_ces)
 
#________________________________________________________________________________________ 
#Export data
 write.csv( score_datces, "~/Biobank/10_DRRI2_CES/DRRICES_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")

#########################################################################################
# Last datBTBISe modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 11, DRRI_PBE
##########################################################################################
#Only retain relevant variables
 datpbe <- subset(dat0, 
               select= c(assessment_id,vista_lastname,
                         DRRIPBE1,
                         DRRIPBE2,
                         DRRIPBE3,
                         DRRIPBE4,
                         DRRIPBE5,
                         DRRIPBE6,
                         DRRIPBE7,
                         DRRIPBE8,
                         DRRIPBE9,
                         DRRIPBE10,
                         DRRIPBE11,
                         DRRIPBE12,
                         DRRIPBE13
               ))
#________________________________________________________________________________________              
#Scoring function
#----------------------------------------------------------------------------------------
#Calculate summary scores in data
#NOTE: For summation purposes we will remove all NA's/treat them as zeros
 datpbe$DRRI_PBE_scores <- as.numeric(rowSums(datpbe[,c("DRRIPBE1",
                                                  "DRRIPBE2",
                                                  "DRRIPBE3",
                                                  "DRRIPBE4",
                                                  "DRRIPBE5",
                                                  "DRRIPBE6",
                                                  "DRRIPBE7",
                                                  "DRRIPBE8",
                                                  "DRRIPBE9",
                                                  "DRRIPBE10",
                                                  "DRRIPBE11",
                                                  "DRRIPBE12",
                                                  "DRRIPBE13")],na.rm=FALSE))
 
 score_pbe <- function(x)
 {
   
   #attach(x)
   for (v in 1:length(x)) assign(names(x)[v], x[[v]])
   
   DRRI_pbe_scores_total<- sum(c(DRRIPBE1,
                                 DRRIPBE2,
                                 DRRIPBE3,
                                 DRRIPBE4,
                                 DRRIPBE5,
                                 DRRIPBE6,
                                 DRRIPBE7,
                                 DRRIPBE8,
                                 DRRIPBE9,
                                 DRRIPBE10,
                                 DRRIPBE11,
                                 DRRIPBE12,
                                 DRRIPBE13),na.rm=F)
   
   DRRI_pbe_scores_total_incomplete <- sum(c(DRRIPBE1,
                                             DRRIPBE2,
                                             DRRIPBE3,
                                             DRRIPBE4,
                                             DRRIPBE5,
                                             DRRIPBE6,
                                             DRRIPBE7,
                                             DRRIPBE8,
                                             DRRIPBE9,
                                             DRRIPBE10,
                                             DRRIPBE11,
                                             DRRIPBE12,
                                             DRRIPBE13),na.rm=T)
   
   data_complete_pbe<- as.numeric(
     sum(
       is.na(
         c(DRRIPBE1,
           DRRIPBE2,
           DRRIPBE3,
           DRRIPBE4,
           DRRIPBE5,
           DRRIPBE6,
           DRRIPBE7,
           DRRIPBE8,
           DRRIPBE9,
           DRRIPBE10,
           DRRIPBE11,
           DRRIPBE12,
           DRRIPBE13
         )
       )
     ) == 0
   )
   #detach(x)
   scorespbe <- data.frame( DRRI_pbe_scores_total, DRRI_pbe_scores_total_incomplete, data_complete_pbe)
   
   return(scorespbe)
   
 }
 
 #Calculate summary scores in datBTBISa
 score_datpbe <- adply(datpbe, 1, score_pbe)
 
#________________________________________________________________________________________ 
#Export data
 write.csv(  score_datpbe, "~/Biobank/11_DRR12_PBE/DRR12_PBE_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")

 #########################################################################################
# Last Date modified:12/21/2017
# Author: Katy Torres
# Description: Subset of question 12, Demographic: Education, Employment & Income
##########################################################################################
#Only retain relevant variables
datemploy <- subset(dat0, 
              select= c(assessment_id,vista_lastname,
                        demo_income_group,
                        demo_education,
                        demo_workstatus,
                        demo_hours,
                        demo_occupation,
                        
                        demo_income_none,
                        demo_income_wrk,
                        demo_income_unemp,
                        demo_income_dis,
                        demo_income_gi,
                        demo_income_retire,
                        demo_income_other,
                        demo_income_spec
                        
                        
              ))

#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
write.csv( datemploy, "~/Biobank/12_DEMO/Demographic_employment_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")

#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 13, exposures and scoring functions
##########################################################################################
#Only retain relevant variables
datexpo <- subset(dat0, 
              select= c(assessment_id,vista_lastname,
                        serv_exposed,
                        serv_exp_none,
                        serv_exp_chemical,
                        serv_exp_bio,
                        serv_exp_jp8,
                        serv_exp_asbestos,
                        serv_exp_nerve,
                        serv_exp_radio,
                        serv_exp_sand,
                        serv_exp_uranium,
                        serv_exp_industrial,
                        serv_exp_fumes,
                        serv_exp_paint,
                        serv_exp_bite,
                        serv_exp_burn,
                        serv_exp_pest,
                        serv_exp_other,
                        serv_exp_oth1spec,
                        serv_exp_other,
                        serv_exp_oth2spec,
                        
                        serv_animal_bite,
                        serv_animal_blood,
                        serv_animal_bat,
                        
                        serv_combat,
                        serv_comb_none,
                        serv_comb_attack,
                        serv_comb_fire,
                        serv_comb_hand,
                        serv_comb_wounded,
                        serv_comb_interro,
                        serv_comb_rocket,
                        serv_comb_seebody,
                        serv_comb_clear,
                        serv_comb_ship,
                        serv_comb_detain,
                        serv_comb_recdfire,
                        serv_comb_handbody,
                        serv_comb_killed,
                        serv_comb_enemy,
                        Exposures.formula
              ))
#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
write.csv( datexpo, "~/Biobank/13_exposures/exposures_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")

 #########################################################################################
 # Last Date modified: 02/02/2018
 # Author: Katy Torres
 # Description: Subset of question 14, GAD7 scoring functions
 ##########################################################################################
 
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
 gad7 <- function(x){
   
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
   
   score_interpretation_gad7<- "1"
   if(!(is.na(mild_anxiety))){
     if(mild_anxiety==1)
     {
       score_interpretation_gad7 <- "2"}else{}
   }else{score_interpretation_gad7<-NA}
   
   if(!(is.na(moderate_anxiety))){
     if(moderate_anxiety==1){
       score_interpretation_gad7 <- "3"} else{}
   }else{score_interpretation_gad7<-NA}
   
   if(!(is.na(severe_anxiety))){
     if(severe_anxiety==1){
       score_interpretation_gad7 <- "4"}else{}
   }else{score_interpretation_gad7<-NA}
   
   # if((severe_anxiety==0) & (moderate_anxiety==0) & (mild_anxiety==0)){
   #   score_interpretation<-0
   # }else{}
   
   
   #treatment action if total score is greater than 14
   
   if (is.na(gad7_total))
   {
     gad7_greater_than_cut_off <- NA
   } else if (gad7_total < 10)
   {
     gad7_greater_than_cut_off <- 0
   } else if (gad7_total >= 10)
   {
     gad7_greater_than_cut_off <- 1
   }
   
   gadscores <- data.frame(gad7_total, gad7_greater_than_cut_off, score_interpretation_gad7, gad7_incomplete,gad7_poss_dx,data_complete_gad)
   
   return(gadscores)
 }
 
 
 #Calculate summary scores in data 
 gad7_scores <- adply(datgad, 1, gad7)
 
 
 #Export data
 write.csv( gad7_scores, "~/Biobank/14_GAD7/gad7_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")
 
 
#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 15, ISI
##########################################################################################
#Only retain relevant variables
datisi <- subset(dat0, 
              select= c(assessment_id,vista_lastname,
                        sleep1a_falling,
                        sleep1b_staying,
                        sleep1c_waking,
                        sleep2_satisfied,
                        sleep3_interfere,
                        sleep4_noticeable,
                        sleep5_worried,
                        sleep_score
              ))
#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------
#Scoring function defined
insomnia <- function(x)
{
  
  #attach(x)
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
  #summary score is the summation of items 1-7
  #Note: This function is not designed to handle NA values (subject must have complete data)
  
  
  insomnia_total <-   sleep1a_falling+
  sleep1b_staying+
  sleep1c_waking+
  sleep2_satisfied+
  sleep3_interfere+
  sleep4_noticeable+
  sleep5_worried+
  sleep_score
  
  not_clinically_significant_insomnia <- as.numeric(insomnia_total <= 7)
  subthreshold_insomnia <- as.numeric(insomnia_total >= 8 & insomnia_total <= 14)
  moderate_severity_insomnia <- as.numeric(insomnia_total >= 15 & insomnia_total <= 21)
  severe_insomnia <- as.numeric(insomnia_total >= 22)
  
  
  insomnia_incomplete <- sum(c(sleep1a_falling,
                             sleep1b_staying,
                             sleep1c_waking,
                             sleep2_satisfied,
                             sleep3_interfere,
                             sleep4_noticeable,
                             sleep5_worried,
                             sleep_score),na.rm=T)

  insomnia_data_complete <- as.numeric(
    sum(
      is.na(
        c(sleep1a_falling,
          sleep1b_staying,
          sleep1c_waking,
          sleep2_satisfied,
          sleep3_interfere,
          sleep4_noticeable,
          sleep5_worried,
          sleep_score
        )
      )
    ) == 0
  )
  
  scoresisi <- data.frame(insomnia_total, not_clinically_significant_insomnia,  subthreshold_insomnia, moderate_severity_insomnia, severe_insomnia , insomnia_data_complete)
  
  return(scoresisi)
}


#Calculate summary scores in data
insomnia_scores <- adply(datisi, 1, insomnia)

#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
write.csv( insomnia_scores, "~/Biobank/15_ISI/ISI_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")

#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 16, ISI Medication Question
##########################################################################################
#Only retain relevant variables
datisi2 <- subset(dat0, 
              select= c(assessment_id,vista_lastname,
                        ISI_Medications, ISI_WhatMeds, ISI_numberofdays
              ))
#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
write.csv( datisi2, "~/Biobank/16_ISI_MedQuestion/ISI_MedQuestion_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")

#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 17, LEC-5 (lifetime)
##########################################################################################
#Only retain relevant variables
datleclife <- subset(dat0, select= c(assessment_id,vista_lastname,
                              LEC_5_1_natdis,
                              LEC_5_2_fire,
                              LEC_5_3_accid,
                              
                              LEC_5_4_seriousacc,
                              LEC_5_5_expos,
                              LEC_5_6_physass,
                              LEC_5_7_assweap,
                              LEC_5_8_sexass,
                              
                              LEC_5_9_otherunw,
                              LEC_5_10_combat,
                              LEC_5_11_captiv,
                              LEC_5_12_life.threat,
                              LEC_5_13_severehum,
                              
                              LEC_5_14_suddviol,
                              LEC_5_15_suddacci,
                              LEC_5_16_seriousinj,
                              LEC_5_17_anyother
                        
              ))
#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
write.csv( datleclife, "~/Biobank/17_LEC-5_lifetime/LEC-5_lifetime_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")

#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 18,LEC-5/ PCL-5(lifetime)
##########################################################################################
#Only retain relevant variables
datlecpcl <- subset(dat0, select= c(assessment_id,vista_lastname,
                              LEC_5_18_MostSevere
                        
              ))
#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
write.csv( datlecpcl, "~/Biobank/18_LEC-5_PCL-5/LEC5_PCL5_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")


#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 19, MST 2016
##########################################################################################
#Only retain relevant variables
datmst <- subset(dat0, 
              select= c(assessment_id,vista_lastname,
                        MST_2016_Q1_2,
                        MST_2016_consult
              ))

#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
write.csv( datmst, "~/Biobank/19_MST2016/MST2016_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")


#########################################################################################
# Last Date modified: 1/23/2018
# Author: Katy Torres
# Description: Subset of question 20, PCL Current
##########################################################################################

#Load plyr library
library(plyr)

#To the user: Set path to where data is stored
setwd('C:/Users/Psychiatry Lab/Documents/Biobank/data')

#Read all data
dat0 <- read.csv('joined_data_export_20180111.csv',header=T,na.strings=c(NA,999))

#Only retain relevant variables
datpclcurr <- subset(dat0, 
                     select= c(assessment_id,vista_lastname, 
                               pcl5_m_1_memories,
                               pcl5_m_2_dream,
                               pcl5_m_3_acting,
                               pcl5_m_4_upset,
                               pcl5_m_5_physical,
                               pcl5_m_6_avoid,
                               pcl5_m_7_external,
                               pcl5_m_8_trouble,
                               pcl5_m_9_negbelief,
                               pcl5_m_10_blame,
                               pcl5_m_11_fear,
                               pcl5_m_12_interest,
                               pcl5_m_13_distant,
                               pcl5_m_14_posfeel,
                               pcl5_m_15_irritable,
                               pcl5_m_16_risk,
                               pcl5_m_17_superalert,
                               pcl5_m_18_jumpy,
                               pcl5_m_19_concentrate,
                               pcl5_m_20_sleep))


#Scoring function defined
pcl_5_current <- function(x)
{
  
  #attach(x)
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
  #PCL summary score is just the summation of all items 1-20
  #Note: This function is not designed to handle NA values (subject must have complete data)
  
  
  #sum of items 1-5            
  pcl_b <- pcl5_m_1_memories +
    pcl5_m_2_dream +
    pcl5_m_3_acting +
    pcl5_m_4_upset +
    pcl5_m_5_physical 
  
  #sum of items 6 and 7
  pcl_c <- pcl5_m_6_avoid +
    pcl5_m_7_external 
  
  #sum of items 8-14    
  pcl_d <-  pcl5_m_8_trouble +
    pcl5_m_9_negbelief +
    pcl5_m_10_blame +
    pcl5_m_11_fear +
    pcl5_m_12_interest +
    pcl5_m_13_distant +
    pcl5_m_14_posfeel 
  
  #sum of items 15-20
  pcl_e <- pcl5_m_15_irritable +
    pcl5_m_16_risk +
    pcl5_m_17_superalert +
    pcl5_m_18_jumpy +
    pcl5_m_19_concentrate +
    pcl5_m_20_sleep
  
  #total symptom severity score (range - 0-80). Obtained by summing the scores for each of the 20 items
  pcl_total <- pcl_b + pcl_c + pcl_d + pcl_e
  
  #sum of all non-na entries for questions 1-20  
  pcl_incomplete <- sum(pcl5_m_1_memories,
                        pcl5_m_2_dream,
                        pcl5_m_3_acting,
                        pcl5_m_4_upset,
                        pcl5_m_5_physical,
                        pcl5_m_6_avoid,
                        pcl5_m_7_external,
                        pcl5_m_8_trouble,
                        pcl5_m_9_negbelief,
                        pcl5_m_10_blame,
                        pcl5_m_11_fear,
                        pcl5_m_12_interest,
                        pcl5_m_13_distant,
                        pcl5_m_14_posfeel,
                        pcl5_m_15_irritable,
                        pcl5_m_16_risk,
                        pcl5_m_17_superalert,
                        pcl5_m_18_jumpy,
                        pcl5_m_19_concentrate,
                        pcl5_m_20_sleep,na.rm=T)
  
  #A PCL-5 cutpoint score of 33 appears to be a reasonable value to propose until further psychometric work is available
  pcl_33 <- as.numeric(pcl_total >= 33)
  
  if(pcl_incomplete >= 33)
  {
    pcl_33 <- 1
  }
  
  
  ##PCL B 
  #Assign TRUE to each PCL B item score that is >= 2
  pcl_5_b_gt2 <- c(pcl5_m_1_memories,pcl5_m_2_dream,pcl5_m_3_acting, pcl5_m_4_upset, pcl5_m_5_physical) >= 2
  
  #Assign TRUE if at least one PCL B is >= 2
  pcl_5_b_dsm5 <- sum(pcl_5_b_gt2) >= 1
  
  
  ##PCL C
  #Assign TRUE to each PCL C item score that is >= 2
  pcl_5_c_gt2 <- c(pcl5_m_6_avoid, pcl5_m_7_external ) >= 2
  
  #Assign TRUE if at least one PCL C is >= 2
  pcl_5_c_dsm5 <- sum(pcl_5_c_gt2) >= 1
  
  
  ##PCL D 
  #Assign TRUE to each PCL D item score that is >= 2
  pcl_5_d_gt2 <- c(pcl5_m_8_trouble ,
                   pcl5_m_9_negbelief ,
                   pcl5_m_10_blame ,
                   pcl5_m_11_fear ,
                   pcl5_m_12_interest ,
                   pcl5_m_13_distant ,
                   pcl5_m_14_posfeel  ) >= 2
  
  #Assign TRUE if at least two PCL Ds are >= 2
  pcl_5_d_dsm5 <- sum(pcl_5_d_gt2) >= 2
  
  
  ##PCL E 
  #Assign TRUE to each PCL E item score that is >= 2
  pcl_5_e_gt2 <- c(pcl5_m_15_irritable ,
                   pcl5_m_16_risk ,
                   pcl5_m_17_superalert ,
                   pcl5_m_18_jumpy ,
                   pcl5_m_19_concentrate ,
                   pcl5_m_20_sleep  ) >= 2
  
  #Assign TRUE if at least two PCL Es are >= 2
  pcl_5_e_dsm5 <- sum(pcl_5_e_gt2) >= 2
  
  
  #DSM-5 symptom cluster severity scores can be obtained by summing the
  #scores for the items within a given cluster
  #Assign TRUE if all PCL sub-symptoms are TRUE
  pcl_5_dsm <- as.numeric(
    sum(pcl_5_b_dsm5, pcl_5_c_dsm5, pcl_5_d_dsm5, pcl_5_e_dsm5) == 4
  )
  
  #flag checks if data has been entered for all 20 questions
  data_complete_pcl_curr <- as.numeric( 
    sum(
      is.na(
        c(pcl5_m_1_memories,
          pcl5_m_2_dream,
          pcl5_m_3_acting,
          pcl5_m_4_upset,
          pcl5_m_5_physical,
          pcl5_m_6_avoid,
          pcl5_m_7_external,
          pcl5_m_8_trouble,
          pcl5_m_9_negbelief,
          pcl5_m_10_blame,
          pcl5_m_11_fear,
          pcl5_m_12_interest,
          pcl5_m_13_distant,
          pcl5_m_14_posfeel,
          pcl5_m_15_irritable,
          pcl5_m_16_risk,
          pcl5_m_17_superalert,
          pcl5_m_18_jumpy,
          pcl5_m_19_concentrate,
          pcl5_m_20_sleep)
      )
    ) == 0
  )
  
  
  
  ###Infer DSM if data is incomplete
  ##PCL B
  #Assign TRUE to each PCL B item score that is >= 2
  pcl_5_b_gt2_infer <- na.omit(c(pcl5_m_1_memories,pcl5_m_2_dream,pcl5_m_3_acting, pcl5_m_4_upset, pcl5_m_5_physical)) >= 2
  
  #Assign TRUE if at least one PCL B is >= 2
  pcl_5_b_dsm5_infer <- sum(pcl_5_b_gt2_infer) >= 1
  
  ##PCL C
  #Assign TRUE to each PCL C item score that is >= 2
  pcl_5_c_gt2_infer <- na.omit(c(pcl5_m_6_avoid, pcl5_m_7_external )) >= 2
  
  #Assign TRUE if at least one PCL C is >= 2
  pcl_5_c_dsm5_infer <- sum(pcl_5_c_gt2_infer) >= 1
  
  ##PCL D
  #Assign TRUE to each PCL D item score that is >= 2
  pcl_5_d_gt2_infer <- na.omit(c(pcl5_m_8_trouble ,
                                 pcl5_m_9_negbelief ,
                                 pcl5_m_10_blame ,
                                 pcl5_m_11_fear ,
                                 pcl5_m_12_interest ,
                                 pcl5_m_13_distant ,
                                 pcl5_m_14_posfeel  )) >= 2
  
  #Assign TRUE if at least two PCL Ds are >= 2
  pcl_5_d_dsm5_infer <- sum(pcl_5_d_gt2_infer) >= 2
  
  ##PCL E
  #Assign TRUE to each PCL E item score that is >= 2
  pcl_5_e_gt2_infer <- na.omit(c(pcl5_m_15_irritable ,
                                 pcl5_m_16_risk ,
                                 pcl5_m_17_superalert ,
                                 pcl5_m_18_jumpy ,
                                 pcl5_m_19_concentrate ,
                                 pcl5_m_20_sleep  )) >= 2
  
  
  
  #Assign TRUE if at least two PCL Es are >= 2
  pcl_5_e_dsm5_infer <- sum(pcl_5_e_gt2_infer) >= 2
  
  #Assign TRUE if all PCL sub-symptoms are TRUE
  pcl_5_dsm_infer <- as.numeric(
    sum(pcl_5_b_dsm5_infer, pcl_5_c_dsm5_infer, pcl_5_d_dsm5_infer, pcl_5_e_dsm5_infer) == 4
  )
  # if(pcl_5_dsm_infer == TRUE)
  # {
  #  pcl_5_dsm = 1
  # }    
  # 
  
  scores <- data.frame(pcl_b,pcl_c,pcl_d,pcl_e,pcl_total,pcl_33,pcl_5_dsm,data_complete_pcl_curr, pcl_5_dsm_infer)
  
  return(scores)
}


#Calculate summary scores in data 
pcl_5_scorescurr <- adply(datpclcurr, 1, pcl_5_current)

#Export data
write.csv( pcl_5_scorescurr, "C:/Users/Psychiatry Lab/Documents/Biobank/20_21_PCL_5/pcl5_current_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")


#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 20, PCL LIFETIME
##########################################################################################
#Only retain relevant variables
 datpcllife <- subset(dat0, 
               select= c(assessment_id,vista_lastname, 
                         pcl5_entirelife_1_memories,
                    pcl5_entirelife_2_dream,
                    pcl5_entirelife_3_acting,
                    pcl5_entirelife_4_upset,
                    pcl5_entirelife_5_physical,
                    pcl5_entirelife_6_avoid,
                    pcl5_entirelife_7_external,
                    pcl5_entirelife_8_trouble,
                    pcl5_entirelife_9_negbelief,
                    pcl5_entirelife_10_blame,
                    pcl5_entirelife_11_fear,
                    pcl5_entirelife_12_interest,
                    pcl5_entirelife_13_distant,
                    pcl5_entirelife_14_posfeel,
                   pcl5_entirelife_15_irritable,
                    pcl5_entirelife_16_risk,
                    pcl5_entirelife_17_superalert,
                    pcl5_entirelife_18_jumpy,
                    pcl5_entirelife_19_concentrate,
                    pcl5_entirelife_20_sleep))

             
#Scoring function defined
pcl_5_entire_life <- function(x)
{

    #attach(x)
    for (v in 1:length(x)) assign(names(x)[v], x[[v]])
    
	#PCL summary score is just the summation of all items 1-20
	#Note: This function is not designed to handle NA values (subject must have complete data)

                
                
	pcl_b <- pcl5_entirelife_1_memories +
        pcl5_entirelife_2_dream +
        pcl5_entirelife_3_acting +
        pcl5_entirelife_4_upset +
        pcl5_entirelife_5_physical 
        
    pcl_c <- pcl5_entirelife_6_avoid +
        pcl5_entirelife_7_external 
        
    pcl_d <-  pcl5_entirelife_8_trouble +
        pcl5_entirelife_9_negbelief +
        pcl5_entirelife_10_blame +
        pcl5_entirelife_11_fear +
        pcl5_entirelife_12_interest +
        pcl5_entirelife_13_distant +
        pcl5_entirelife_14_posfeel 
        
    pcl_e <-pcl5_entirelife_15_irritable +
        pcl5_entirelife_16_risk +
        pcl5_entirelife_17_superalert +
        pcl5_entirelife_18_jumpy +
        pcl5_entirelife_19_concentrate +
        pcl5_entirelife_20_sleep
    
    pcl_total <- pcl_b + pcl_c + pcl_d + pcl_e
    

                    
                    
   pcl_33 <- as.numeric(pcl_total >= 33)
    
   pcl_incomplete <- sum(pcl5_entirelife_1_memories,
                pcl5_entirelife_2_dream,
                pcl5_entirelife_3_acting,
                pcl5_entirelife_4_upset,
                pcl5_entirelife_5_physical,
                pcl5_entirelife_6_avoid,
                pcl5_entirelife_7_external,
                pcl5_entirelife_8_trouble,
                pcl5_entirelife_9_negbelief,
                pcl5_entirelife_10_blame,
                pcl5_entirelife_11_fear,
                pcl5_entirelife_12_interest,
                pcl5_entirelife_13_distant,
                pcl5_entirelife_14_posfeel,
               pcl5_entirelife_15_irritable,
                pcl5_entirelife_16_risk,
                pcl5_entirelife_17_superalert,
                pcl5_entirelife_18_jumpy,
                pcl5_entirelife_19_concentrate,
                pcl5_entirelife_20_sleep,na.rm=T)
                
    if(pcl_incomplete >= 33)
    {
     pcl_33 <- 1
    }
   
    
    ##PCL B 
    #Assign TRUE to each PCL B item score that is >= 2
    pcl_5_b_gt2 <- c(pcl5_entirelife_1_memories,pcl5_entirelife_2_dream,pcl5_entirelife_3_acting, pcl5_entirelife_4_upset, pcl5_entirelife_5_physical) >= 2
    
    #Assign TRUE if at least one PCL B is >= 2
    pcl_5_b_dsm5 <- sum(pcl_5_b_gt2) >= 1
    
    ##PCL C
    #Assign TRUE to each PCL C item score that is >= 2
    pcl_5_c_gt2 <- c(pcl5_entirelife_6_avoid, pcl5_entirelife_7_external ) >= 2
    
    #Assign TRUE if at least one PCL C is >= 2
    pcl_5_c_dsm5 <- sum(pcl_5_c_gt2) >= 1
    
    ##PCL D 
    #Assign TRUE to each PCL D item score that is >= 2
    pcl_5_d_gt2 <- c(pcl5_entirelife_8_trouble ,
        pcl5_entirelife_9_negbelief ,
        pcl5_entirelife_10_blame ,
        pcl5_entirelife_11_fear ,
        pcl5_entirelife_12_interest ,
        pcl5_entirelife_13_distant ,
        pcl5_entirelife_14_posfeel  ) >= 2
    
    #Assign TRUE if at least two PCL Ds are >= 2
    pcl_5_d_dsm5 <- sum(pcl_5_d_gt2) >= 2
    
    ##PCL E 
    #Assign TRUE to each PCL E item score that is >= 2
    pcl_5_e_gt2 <- c(pcl5_entirelife_15_irritable ,
        pcl5_entirelife_16_risk ,
        pcl5_entirelife_17_superalert ,
        pcl5_entirelife_18_jumpy ,
        pcl5_entirelife_19_concentrate ,
        pcl5_entirelife_20_sleep  ) >= 2
      

    
    #Assign TRUE if at least two PCL Es are >= 2
    pcl_5_e_dsm5 <- sum(pcl_5_e_gt2) >= 2
    
    #Assign TRUE if all PCL sub-symptoms are TRUE
    pcl_5_dsm <- as.numeric(
                    sum(pcl_5_b_dsm5, pcl_5_c_dsm5, pcl_5_d_dsm5, pcl_5_e_dsm5) == 4
                        )
    
    ###Infer DSM if data is incomplete
    ##PCL B 
    #Assign TRUE to each PCL B item score that is >= 2
    pcl_5_b_gt2_infer <- na.omit(c(pcl5_entirelife_1_memories,pcl5_entirelife_2_dream,pcl5_entirelife_3_acting, pcl5_entirelife_4_upset, pcl5_entirelife_5_physical)) >= 2
    
    #Assign TRUE if at least one PCL B is >= 2
    pcl_5_b_dsm5_infer <- sum(pcl_5_b_gt2_infer) >= 1
    
    ##PCL C
    #Assign TRUE to each PCL C item score that is >= 2
    pcl_5_c_gt2_infer <- na.omit(c(pcl5_entirelife_6_avoid, pcl5_entirelife_7_external )) >= 2
    
    #Assign TRUE if at least one PCL C is >= 2
    pcl_5_c_dsm5_infer <- sum(pcl_5_c_gt2_infer) >= 1
    
    ##PCL D 
    #Assign TRUE to each PCL D item score that is >= 2
    pcl_5_d_gt2_infer <- na.omit(c(pcl5_entirelife_8_trouble ,
        pcl5_entirelife_9_negbelief ,
        pcl5_entirelife_10_blame ,
        pcl5_entirelife_11_fear ,
        pcl5_entirelife_12_interest ,
        pcl5_entirelife_13_distant ,
        pcl5_entirelife_14_posfeel  )) >= 2
    
    #Assign TRUE if at least two PCL Ds are >= 2
    pcl_5_d_dsm5_infer <- sum(pcl_5_d_gt2_infer) >= 2
    
    ##PCL E 
    #Assign TRUE to each PCL E item score that is >= 2
    pcl_5_e_gt2_infer <- na.omit(c(pcl5_entirelife_15_irritable,
        pcl5_entirelife_16_risk ,
        pcl5_entirelife_17_superalert ,
        pcl5_entirelife_18_jumpy ,
        pcl5_entirelife_19_concentrate ,
        pcl5_entirelife_20_sleep  )) >= 2
      

    
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

    
    data_complete_pcl_life <- as.numeric( 
                sum(
                    is.na(
                    c(pcl5_entirelife_1_memories,
                    pcl5_entirelife_2_dream,
                    pcl5_entirelife_3_acting,
                    pcl5_entirelife_4_upset,
                    pcl5_entirelife_5_physical,
                    pcl5_entirelife_6_avoid,
                    pcl5_entirelife_7_external,
                    pcl5_entirelife_8_trouble,
                    pcl5_entirelife_9_negbelief,
                    pcl5_entirelife_10_blame,
                    pcl5_entirelife_11_fear,
                    pcl5_entirelife_12_interest,
                    pcl5_entirelife_13_distant,
                    pcl5_entirelife_14_posfeel,
                   pcl5_entirelife_15_irritable,
                    pcl5_entirelife_16_risk,
                    pcl5_entirelife_17_superalert,
                    pcl5_entirelife_18_jumpy,
                    pcl5_entirelife_19_concentrate,
                    pcl5_entirelife_20_sleep)
                    )
                ) == 0
                )
                
    scorespcllife <- data.frame(pcl_b,pcl_c,pcl_d,pcl_e,pcl_total,pcl_33,pcl_5_dsm,data_complete_pcl_life)
    
	return(scorespcllife)
}


#Calculate summary scores in data 
 pcl_5_scores <- adply(datpcllife, 1, pcl_5_entire_life)

#Export data
 write.csv( pcl_5_scores, "C:/Users/Psychiatry Lab/Documents/Biobank/20_21_PCL_5/pcl5_entire_life_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")


#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 22, PHQ15_22
##########################################################################################
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
write.csv( score_datphq15, "~/Biobank/22_PHQ-15/PHQ15_22_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")


 #########################################################################################
 # Last Date modified: 1/22/2018
 # Author: Katy Torres
 # Description: Subset of question 23, PHQ9
 ##########################################################################################
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
   for (v in 1:length(x)) assign(names(x)[v], x[[v]])
   
   #PHQ-9 summary score is the summation of items 1-9 (not 10)
   #Note: This function is not designed to handle NA values (subject must have complete data)
   
   #total score              
   phq9_total <- dep1_interest +
     dep2_down +
     dep3_sleep +
     dep4_tired +
     dep5_appetite +
     dep6_feelbad +
     dep7_concentrate +
     dep8_moveslow +
     dep9_dead
   
   
   
   #Diagnosis based on score, one diagnosis per column 
   
   minimal_depression <- as.numeric(phq9_total <= 4)
   
   mild_depression <- as.numeric(phq9_total >= 5 & phq9_total <= 9)
   
   moderate_depression <- as.numeric(phq9_total >= 10 & phq9_total <= 14)
   
   moderately_severe_depression <- as.numeric(phq9_total >= 15 & phq9_total <= 19)
   
   severe_depression <- as.numeric(phq9_total >= 20)
   
   
   
   #One column for Diagnosis based on score
   
   if(!(is.na(minimal_depression))){
     if(minimal_depression==1)
     {
       score_interpretation_phq9 <- "1"}else{}
   }else{score_interpretation_phq9 <-0}
   
   if(!(is.na(mild_depression))){
     if(mild_depression==1){
       score_interpretation_phq9 <- "2"} else{}
   }else{score_interpretation_phq9 <-0}
   
   if(!(is.na(moderate_depression))){
     if(moderate_depression==1){
       score_interpretation_phq9 <- "3"}else{}
   }else{score_interpretation_phq9 <-0}
   
   if(!(is.na(moderately_severe_depression))){
     if(moderately_severe_depression==1){
       score_interpretation_phq9 <- "4"}else{}
   }else{score_interpretation_phq9 <-0}
   
   if(!(is.na(severe_depression))){
     if(severe_depression==1){
       score_interpretation_phq9 <- "5"}else{}
   }else{score_interpretation_phq9 <-0}
   
   
   
   #score if any items are missing
   phq9_incomplete <- sum(c(dep1_interest ,
                            dep2_down ,
                            dep3_sleep ,
                            dep4_tired ,
                            dep5_appetite ,              
                            dep6_feelbad ,
                            dep7_concentrate ,
                            dep8_moveslow ,
                            dep9_dead),na.rm=T)
   
   
   
   #treatment action if total score is greater than 14
   
   if (is.na(phq9_total))
   {
     phq9_treatment <- NA
   } else if (phq9_total <= 14)
   {
     phq9_treatment <- 0
   } else if (phq9_total > 14)
   {
     phq9_treatment <- 1
   }
   
   
   #flag if all entires are complete. 0 if incomplete data
   data_complete_phq9 <- as.numeric( 
     sum(
       is.na(
         c(dep1_interest, dep2_down, dep3_sleep, dep4_tired, dep5_appetite,
           dep6_feelbad, dep7_concentrate, dep8_moveslow, dep9_dead, dep10_difficult)
       )
     ) == 0
   )
   
   scoresphq <- data.frame(phq9_total, score_interpretation_phq9, phq9_treatment,data_complete_phq9)
   
   return(scoresphq)
   
 }
 
 
 #Calculate summary scores in data 
 phq9_scores <- adply(datphq9, 1, phq9)
 
 #Export data
 write.csv( phq9_scores, "~/Biobank/23_PHQ9/phq9_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")
 
 
 
#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 24, Positive and Negative Affect Schedule
##########################################################################################
#Only retain relevant variables
datpanas <- subset(dat0, select= c(assessment_id,vista_lastname,
                        panas1_interest,
                        panas2_distress,
                        panas3_excite,
                        panas4_upset,
                        panas5_strong,
                        
                        panas6_guilt,
                        panas7_scare,
                        panas8_host,
                        panas9_enth,
                        panas10_proud,
                        
                        panas11_irri,
                        panas12_alert,
                        panas13_asham,
                        panas14_insp,
                        panas15_nerv,
                        
                        panas16_deter,
                        panas17_atten,
                        panas18_jitt,
                        panas19_act,
                        panas20_afraid,
                        
                        Panas.Positive,
                        Positive.Affect.Score,
                        Negative.Affect.Score,
                        PANAS.Positive,
                        Liz.s.Formula
                        
              ))
#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
write.csv( datpanas, "~/Biobank/24_PANAS/PANAS_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")

#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 25, PROMIS PAIN INTENSITY
##########################################################################################
#Only retain relevant variables
datpain <- subset(dat0, 
              select= c(assessment_id,vista_lastname,
                        pain_level,
                        pain_intensity,
                        pain_average,
                        
                        pain_interfere_life,
                        pain_interfere_conc,
                        pain_interfere_day,
                        pain_interfere_rec,
                        pain_interfere_task,
                        
                        pain_interfere_social,
                        pain_score_intensity,
                        pain_score_interference
              ))
#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
write.csv( datpain, "~/Biobank/25_PROMIS_Pain_Intensity/PROMIS_Pain_Intensity_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")

#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 26, Research ID and scoring functions
##########################################################################################
#Only retain relevant variables
datid <- subset(dat0, 
              select= c(assessment_id,vista_lastname,
                        
                        Guilt_Visit,
                        Guilt_Comments,
                        
                        Research_VisitDate
              ))

#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
write.csv( datid, "~/Biobank/26_Research_ID/Research_ID_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")

#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 27, service history and scoring functions
##########################################################################################
#Only retain relevant variables
datservice <- subset(dat0, 
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
#Export data
#----------------------------------------------------------------------------------------
write.csv( datservice, "~/Biobank/27_service_history/service_history_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")

#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 28, Demographic: Social Environment
##########################################################################################
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
#Export data
#----------------------------------------------------------------------------------------
write.csv( datdemosocial, "~/Biobank/28_Demo_Social/Demographic_social_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")

#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 29, Tinnitus Screener
##########################################################################################
#Only retain relevant variables
dattinnitus <- subset(dat0, 
              select= c(assessment_id,vista_lastname,
                        Tinnitus.1_2.3mins,
                        Tinnitus.2_6months,
                        Tinnitus.3_quietroom,
                        Tinnitus.4_recentevents,
                        Tinnitus.5_comeandgo,
                        Tinnitus.6_experience
                        
              ))

#________________________________________________________________________________________              
# SCORING Functions Defined
#----------------------------------------------------------------------------------------
#Scoring function defined
tinnitus <- function(x)
{
  
  #attach(x)
  for (v in 1:length(x)) assign(names(x)[v], x[[v]])
  
  #summary score is the summation of items 1-7
  #Note: This function is not designed to handle NA values (subject must have complete data)
  
  
  tinnitus_total <-   Tinnitus.1_2.3mins+
  Tinnitus.2_6months+
  Tinnitus.3_quietroom+
  Tinnitus.4_recentevents+
  Tinnitus.5_comeandgo+
  Tinnitus.6_experience
  
#SCORING of categories
  
  no_tinnitus<- as.numeric(!Tinnitus.1_2.3mins)
  
  
  if(!is.na(Tinnitus.1_2.3mins) & !is.na(Tinnitus.5_comeandgo) &  !is.na(Tinnitus.4_recentevents)) {
    if(Tinnitus.4_recentevents ==1 & Tinnitus.5_comeandgo == 1 & Tinnitus.1_2.3mins==1) { tinnitus_temporary_only<- 1} 
    else{ tinnitus_temporary_only<- 0} 
  }
  else { tinnitus_temporary_only<- NA } 
  
  
  if(!is.na(Tinnitus.6_experience) & !is.na(Tinnitus.1_2.3mins)) {
    if(Tinnitus.6_experience == 2 & Tinnitus.1_2.3mins == 1) { tinnitus_occassional<- 1}
    else{ tinnitus_occassional <- 0}
  } 
  else { tinnitus_occassional <- NA } 
  
  
  
  if(!is.na(Tinnitus.1_2.3mins) & !is.na(Tinnitus.2_6months)) {
    if(Tinnitus.2_6months==0 & Tinnitus.1_2.3mins==1) { tinnitus_acute<- 1} 
    else{ tinnitus_acute<- 0} 
  }
  else { tinnitus_acute <- NA } 
  
  
  if(!is.na(Tinnitus.1_2.3mins) & !is.na(Tinnitus.2_6months)) {
    if(Tinnitus.1_2.3mins == 1 & Tinnitus.2_6months == 1) { tinnitus_chronic<- 1} 
    else if(Tinnitus.1_2.3mins == 1 & Tinnitus.2_6months == 0) { tinnitus_chronic<- 0} 
    else if(Tinnitus.1_2.3mins == 0) { tinnitus_chronic <- 0} 
  }
  else { tinnitus_chronic <- NA } 

  
  tinnitus_data_complete <- as.numeric(
    sum(
      is.na(
        c(Tinnitus.1_2.3mins,
          Tinnitus.2_6months,
          Tinnitus.3_quietroom,
          Tinnitus.4_recentevents,
          Tinnitus.5_comeandgo,
          Tinnitus.6_experience
        )
      )
    ) == 0
  )
  
  scores <- data.frame(tinnitus_total, no_tinnitus, tinnitus_temporary_only, tinnitus_occassional,
                       tinnitus_acute, tinnitus_chronic,
                       tinnitus_data_complete)
  
  return(scores)
}


#Calculate summary scores in data
tinnitus_scores <- adply(dattinnitus, 1, tinnitus)

#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
write.csv( tinnitus_scores, "~/Biobank/29_Tinnitus_Screener/tinnitus_screener_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")

#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 30, Treatment History
##########################################################################################
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
write.csv( dattreatment, "~/Biobank/30_treatment_history/TreatHis_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")


#########################################################################################
# Last Date modified: 12/21/2017
# Author: Katy Torres
# Description: Subset of question 31, WHODAS and scoring functions
##########################################################################################
#Only retain relevant variables
datwhodas <- subset(dat0, 
                    select= c(assessment_id,vista_lastname,
                              whodas1_1_concentrate,
                              whodas1_2_remember,
                              whodas1_3_solution,
                              whodas1_4_new,
                              whodas1_5_understand,
                              whodas1_6_conversation,
                              
                              whodas2_1_stand,
                              whodas2_2_standup,
                              whodas2_3_move,
                              whodas2_4_getout,
                              whodas2_5_walk,
                              
                              whodas3_1_wash,
                              whodas3_2_dressed,
                              whodas3_3_eat,
                              whodas3_4_stay,
                              
                              whodas4_1_deal,
                              whodas4_2_friend,
                              whodas4_3_getalong,
                              whodas4_4_newfriend,
                              whodas4_5_sexual,
                              
                              whodas5_1_housecare,
                              whoda5_2_housetask,
                              whodas5_3_housedone,
                              whodas5_4_housequickly,
                              whodas_work,
                              
                              whodas5_5_daily,
                              whodas5_6_workwell,
                              whodas5_7_workdone,
                              whodas5_8_workquickly,
                              
                              whodas6_1_community,
                              whodas6_2_barriers,
                              whodas6_3_dignity,
                              whodas6_4_time,
                              whodas6_5_emotion,
                              whodas6_6_finance,
                              whodas6_7_family,
                              whodas6_8_relax,
                              
                              whodas_understand_mean,
                              whodas_understand_score,
                              
                              whodas_mobility_mean,
                              whodas_mobility_score,
                              
                              whodas_selfcare_mean,
                              whodas_selfcare_score,
                              
                              whodas_people_mean,
                              whodas_people_score,
                              
                              whodas_household_mean,
                              whodas_household_score,
                              
                              whodas_work_mean,
                              whodas_work_score,
                              
                              whodas_society_mean,
                              whodas_society_score
                              
                    ))
#________________________________________________________________________________________
# Missing Data Imputation function defined
#----------------------------------------------------------------------------------------
# In all other situations where one or two items are missing, the mean score across all items within
# the domain should be assigned to the missing items. This method should not be used if more
# than two items are missing. In addition, if domain-wise scores are being computed for domains,
# the two missing items should not come from the same domain.

MissingDat <- function(x)
{
  #Define Domains as subsets of data
  D1<-x[,c("whodas1_1_concentrate" , "whodas1_2_remember" , "whodas1_3_solution" , "whodas1_4_new" , "whodas1_5_understand" , "whodas1_6_conversation")]
  D2<-x[,c("whodas2_1_stand" , "whodas2_2_standup" , "whodas2_3_move" , "whodas2_4_getout" , "whodas2_5_walk")]
  D3<-x[,c("whodas3_1_wash" , "whodas3_2_dressed" , "whodas3_3_eat" , "whodas3_4_stay")]
  D4<-x[,c("whodas4_1_deal" , "whodas4_2_friend" , "whodas4_3_getalong" , "whodas4_4_newfriend" , "whodas4_5_sexual")]
  D51<-x[,c("whodas5_1_housecare" , "whoda5_2_housetask" , "whodas5_3_housedone" , "whodas5_4_housequickly")]
  D52<-x[,c("whodas5_5_daily","whodas5_6_workwell","whodas5_7_workdone","whodas5_8_workquickly")]
  D6<-x[,c("whodas6_1_community" , "whodas6_2_barriers" , "whodas6_3_dignity" , "whodas6_4_time" , "whodas6_5_emotion" , "whodas6_6_finance" , "whodas6_7_family" , "whodas6_8_relax")]
  
  dfList<- list(D1,D2,D3,D4,D51,D52,D6)
  
  #loop each domain and have it count the number of NA's and deal with missing data
  #make a data frame of data frames
  ttt <- lapply(dfList, function(y) {
    #count number of NA's in each domain
    na_count <- apply(y, 1, function(i) sum(is.na(i)))
    
    #if less than 2, make NA's be the mean of other items in domain
    if(na_count <= 2)
    {
      
      mn<- rowMeans(y, na.rm=TRUE) 
      y[is.na(y)] <- mn
      if(na_count >= 1)
      {
        y$imputed <- 1
        print("imputed row")
      } 
      else{
        y$imputed <- 0
      }
      names(y)[which(names(y) == "imputed")] <- paste(names(y)[1],"imputed",sep="_")
    }
    return(y)
  })
  
  #list.cbind(dfList)
  newdat<-do.call(cbind,ttt)
  whodasscores <- data.frame(newdat)
  
  return(whodasscores)
}



# Imputation function called
#test with subject I know needs imputation
#whodas_missing_dealt <- adply(dat[dat$assessment_id==8583,], 1, MissingDat)
whodas_missing_dealt <- adply(datwhodas, 1, MissingDat)

#----------------------------------------------------------------------------------------
#generate new dataset based on original dataset but with columns added for new columns
datawhodas1<-datwhodas
#add imputed column to original data set, stating if the data was imputed or complete
imputed_columns <- whodas_missing_dealt[,grepl( "imputed" , names(whodas_missing_dealt) ) ]
datawhodas1$imputed_data=apply(imputed_columns,1,function(x) ifelse(any(x==1),'1','0'))
datawhodas1$imputed_data[is.na(datawhodas1$imputed_data)] <- 0
#________________________________________________________________________________________              
# SCORING DEFINED
#----------------------------------------------------------------------------------------
#Calculate summary scores in data using the imputed data and add this to original data frame
#NOTE: For summation purposes we will remove all NA's/treat them as zeros



#Deal with non-working individuals
#If the respondent is not working and has given responses to the 32-item WHODAS 2.0, the score
#can be used as it is, and will be comparable to that of the full 36-item version.

#ACTUAL DATA SCORES
#----------------------------------------------------------------------------------------
datawhodas1$whodas_work_score2 <- datawhodas1$whodas_work_score
datawhodas1$whodas_work_score2[is.na(datawhodas1$whodas_work_score2)] <- 0
#add all fields taking into account that some peopel do not work
datawhodas1$score_sum_actual<- as.numeric(rowSums(datawhodas1[,c("whodas_selfcare_score",
                                                                 "whodas_understand_score",
                                                                 "whodas_society_score",
                                                                 "whodas_household_score",
                                                                 "whodas_work_score2",
                                                                 "whodas_mobility_score",
                                                                 "whodas_people_score")],na.rm=FALSE))


#IMPUTED DATA SCORES
#----------------------------------------------------------------------------------------
whodas_missing_dealt$whodas_work_score2 <- whodas_missing_dealt$whodas_work_score

#SUM THE DOMAIN SCORES WITH IMPUTED DATA
whodas_missing_dealt$D1_IMPUTED_SCORE<-as.numeric(rowSums(whodas_missing_dealt [,c("whodas1_1_concentrate" , "whodas1_2_remember" , "whodas1_3_solution" , "whodas1_4_new" , "whodas1_5_understand" , "whodas1_6_conversation")],na.rm=FALSE))
whodas_missing_dealt$D2_IMPUTED_SCORE<-as.numeric(rowSums(whodas_missing_dealt [,c("whodas2_1_stand" , "whodas2_2_standup" , "whodas2_3_move" , "whodas2_4_getout" , "whodas2_5_walk")],na.rm=FALSE))
whodas_missing_dealt$D3_IMPUTED_SCORE<-as.numeric(rowSums(whodas_missing_dealt [,c("whodas3_1_wash" , "whodas3_2_dressed" , "whodas3_3_eat" , "whodas3_4_stay")],na.rm=FALSE))
whodas_missing_dealt$D4_IMPUTED_SCORE<-as.numeric(rowSums(whodas_missing_dealt [,c("whodas4_1_deal" , "whodas4_2_friend" , "whodas4_3_getalong" , "whodas4_4_newfriend" , "whodas4_5_sexual")],na.rm=FALSE))
whodas_missing_dealt$D51_IMPUTED_SCORE<-as.numeric(rowSums(whodas_missing_dealt [,c("whodas5_1_housecare" , "whoda5_2_housetask" , "whodas5_3_housedone" , "whodas5_4_housequickly")],na.rm=FALSE))
whodas_missing_dealt$D52_IMPUTED_SCORE<-as.numeric(rowSums(whodas_missing_dealt [,c("whodas5_5_daily","whodas5_6_workwell","whodas5_7_workdone","whodas5_8_workquickly")],na.rm=FALSE))
whodas_missing_dealt$D6_IMPUTED_SCORE<-as.numeric(rowSums(whodas_missing_dealt [,c("whodas6_1_community" , "whodas6_2_barriers" , "whodas6_3_dignity" , "whodas6_4_time" , "whodas6_5_emotion" , "whodas6_6_finance" , "whodas6_7_family" , "whodas6_8_relax")],na.rm=FALSE))


#convert NAs due to not working into zeros
whodas_missing_dealt$D52_IMPUTED_SCORE[is.na(whodas_missing_dealt$D52_IMPUTED_SCORE)] <- 0

#add all fields taking into account that some people do not work
datawhodas1$score_sum_imputed <- as.numeric(rowSums(whodas_missing_dealt[,c("D1_IMPUTED_SCORE",
                                                                            "D2_IMPUTED_SCORE",
                                                                            "D3_IMPUTED_SCORE",
                                                                            "D4_IMPUTED_SCORE",
                                                                            "D51_IMPUTED_SCORE",
                                                                            "D52_IMPUTED_SCORE",
                                                                            "D6_IMPUTED_SCORE")],na.rm=FALSE))
#remove temporary variable
whodas_missing_dealt$whodas_work_score2 <- NULL


#Score taking into account people who don't work
#----------------------------------------------------------------------------------------

for (i in 1:nrow(datawhodas1)){
  if (datawhodas1$whodas_work_score2[i] == 0){
    datawhodas1$summaryscore_imputed[i]<- datawhodas1$score_sum_imputed[i]*100/166
    datawhodas1$summaryscore_actual[i]<- datawhodas1$score_sum_actual[i]*100/166
  } else {
    datawhodas1$summaryscore_imputed[i]<- datawhodas1$score_sum_imputed[i]*100/180
    datawhodas1$summaryscore_actual[i]<- datawhodas1$score_sum_actual[i]*100/180
  }}

#remove temporary variable
datawhodas1$whodas_work_score2 <- NULL

#________________________________________________________________________________________ 
#Export data
#----------------------------------------------------------------------------------------
write.csv( datawhodas1, "~/Biobank/31_WHODAS/WHODAS_reduced_data_export_20180212.csv",quote=T,row.names=F,na="#N/A")
