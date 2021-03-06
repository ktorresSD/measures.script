#Load plyr library
 library(plyr)

#To the user: Set path to where data is stored
 setwd("~/Biobank/data")

#Read all data
 dat0 <- read.csv('joined_data_export_20171218.csv',header=T,na.strings=c(NA,999))

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
 write.csv( datcurrent, "~/Biobank/9_current_treatments/CurrTx_reduced_data_export_20171218.csv",quote=T,row.names=F,na="#N/A")


