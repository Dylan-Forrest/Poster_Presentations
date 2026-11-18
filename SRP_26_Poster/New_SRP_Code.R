#####################
## New SRP Project ##
#####################

###### Research Aims ######

## Aim 1: Does the amount of exposure to ACEs predict later psychopathology?
  
## Aim 2: Does frequent usage of Reappraisal or Suppression predict later psychopathology?
  
## Aim 3: Does the amount of exposure to ACEs predict the development of ER strategies (Reappraisal and Suppression)?
  
## Aim 4: Do patterns of frontoparietal network functional connectivity appear to underlie these relationships?
  # 4a: Does functional connectivity relate to psychopathology
  # 4b: Does functional connectivity relate to ER strategy usage 


library(tidyverse)
library(tidyverse)

library(ggplot2)
library(ggplot2)

library(psych)
library(psych)


# Set to ABCD data directory
datapath <- "C:/Users/dylan/OneDrive - The University of Texas at Dallas/Damme, Katherine Steffen's files - R21MH136408_PLEs_Menarche/"


# Demographics 
Demo <- read_tsv(paste0(datapath, "ABCD_6.0/Data/abcd_general/ab_g_stc.tsv"),
                 na=c("NA","999", "777", "888", "n/a","N/A"))

# Ages 
Ages <- read_tsv(paste0(datapath, "ABCD_6.0/Data/abcd_general/ab_g_dyn.tsv"),
                 na=c("NA","999", "777", "888", "n/a","N/A"))

# ACEs 
ACEs <- read.csv(paste0(datapath, "AdditionalABCDMeasures/ABCD Adverse Childhood Experiences/ABCD_5.0/Outputs/ACES_baseline_output.csv"),
                 na=c("NA","999", "777", "888", "n/a","N/A"))

# Emotion Regulation 
ERQ <- read_tsv(paste0(datapath, "ABCD_6.0/Data/Mental_Health/mh_y_erq.tsv"),
                na=c("NA","999", "777", "888", "n/a","N/A"))

# P Factor Measures 
Psychopathology <- read_tsv(paste0(datapath, 'ABCD_6.0/Data/Mental_Health/mh_y_bpm.tsv'),
                            na=c("NA","999", "777", "888", "n/a","N/A"))

# Psychotic-Like Experiences 
PLEs_y <- read_tsv(paste0(datapath, 'ABCD_6.0/Data/Mental_Health/mh_y_pps.tsv'),
                   na=c("NA","999", "777", "888", "n/a","N/A")) 

# FPN-Amygdala 
RS_FC_FPN_Amygdala <- read_tsv(paste0(datapath, "ABCD_6.0/Data/Imaging/RestingState_fMRI/mr_y_rsfmri__corr__gpnet__aseg.tsv"),
                               na=c("NA","999", "777", "888", "n/a","N/A"))

# FPN-FPN 
RS_FC_FPN_FPN <- read_tsv(paste0(datapath, "ABCD_6.0/Data/Imaging/RestingState_fMRI/mr_y_rsfmri__corr__gpnet.tsv"),
                          na=c("NA","999", "777", "888", "n/a","N/A"))

# MRI Scanner ID 
Scanner_ID <- read_tsv(paste0(datapath, "ABCD_6.0/Data/abcd_general/ab_g_dyn.tsv"),
                       na=c("NA","999", "777", "888", "n/a","N/A"))

# fMRI Quality Check
fMRI_QC <- read_tsv(paste0(datapath, "ABCD_6.0/Data/Imaging/QC/mr_y_qc__raw__rsfmri.tsv"),
                       na=c("NA","999", "777", "888", "n/a","N/A"))


#########################
##  Demographics Data  ##
#########################

# Pull participant sex #
Participant_Sex <- Demo %>%
  select(participant_id, ab_g_stc__cohort_sex) %>%
  rename(sex = ab_g_stc__cohort_sex)


#################
##  ACEs Data  ##
#################

# Convert baseling age from months to years #
ACEs_clean <- ACEs %>%
  mutate('baseline_age' = interview_age / 12)

# Keep Relevant Columns and Rename 5.0 -> 6.0 #
ACEs_clean <- ACEs_clean %>%
  select(src_subject_id, rel_family_id, baseline_age, demo_comb_income_hrcode, race_ethnicity_hrcode, Abuse_ACEs, Neglect_ACEs, ACEs_Total) %>%
  mutate(src_subject_id = str_replace(src_subject_id, "NDAR_INV", "sub-")) %>%
  rename(participant_id = src_subject_id)

# Set relevant variables as factor # 
ACEs_clean$demo_comb_income_hrcode <- as.factor(ACEs_clean$demo_comb_income_hrcode)
ACEs_clean$race_ethnicity_hrcode <- as.factor(ACEs_clean$race_ethnicity_hrcode)

## ACEs dataframe already contains demographics data, participant sex is replaced because of changes 
## to how the ABCD study displays information about intersex participants in the 6.0 update

# Create ACEs yes/no flag and seperate column for total number of ACEs for graphing #
ACEs_clean <- ACEs_clean %>% 
  mutate(ACEs_Flag = if_else(ACEs_Total > 0, 1, 0)) %>%
  mutate(ACEs_Count = ACEs_Total)

# Convert ACEs data into Z-scores #
ACEs_clean <- ACEs_clean %>% 
  mutate(ACEs_Total = (ACEs_Total - mean(ACEs_Total)) / sd(ACEs_Total))



###############################
##  Emotion Regulation Data  ##
###############################

# Select relevant columns and rename for readability #
ERQ_clean <- ERQ %>%
  select(participant_id, session_id, mh_y_erq_age, mh_y_erq__reapp_mean, mh_y_erq__suppr_mean) %>%
  rename('ERQ_age' = mh_y_erq_age, 'Reappraisal_Mean' = mh_y_erq__reapp_mean, 'Suppression_Mean' = mh_y_erq__suppr_mean, "ERQ_session_id" = session_id)

# Pull participants first ERQ session #
ERQ_clean <- ERQ_clean %>%
  mutate(ses_priority = match(ERQ_session_id, c("ses-03A", "ses-04A", "ses-05A", "ses-06A"))) %>%
  arrange(participant_id, ses_priority) %>%
  group_by(participant_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(-ses_priority)

# Remove rows with no ERQ mean data #
ERQ_clean <- ERQ_clean %>%
  filter(Reappraisal_Mean != "n/a") %>%
  drop_na(Reappraisal_Mean) %>%
  filter(Suppression_Mean != "n/a") %>%
  drop_na(Suppression_Mean)

# Make ERQ mean scores #
ERQ_clean <- ERQ_clean %>%
  mutate(Reappraisal_Mean = as.numeric(Reappraisal_Mean)) %>%
  mutate(Suppression_Mean = as.numeric(Suppression_Mean))

# Convert ERQ data into Z-scores #
ERQ_clean <- ERQ_clean %>% 
  mutate(Reappraisal_Mean = (Reappraisal_Mean - mean(Reappraisal_Mean)) / sd(Reappraisal_Mean)) %>%
  mutate(Suppression_Mean = (Suppression_Mean - mean(Suppression_Mean)) / sd(Suppression_Mean))


###################################
#  General Psychopathology  Data  #   # Partly Shreeyalaxhmee Rao's Code #
###################################

# PLEs dataframe #
PLEs_clean <- PLEs_y %>%
  select(participant_id,
         session_id,
         mh_y_pps__severity_score) # severity score summary

# Attention dataframe #
Attention_clean <- Psychopathology %>%
  select(participant_id, session_id, 
         mh_y_bpm__attn_sum)  %>% #attention summary
  mutate(mh_y_bpm__attn_sum = if_else(mh_y_bpm__attn_sum =="n/a", NA, as.numeric(mh_y_bpm__attn_sum)))

# Externalizing dataframe #
Externalizing_clean <- Psychopathology %>%
  select(participant_id, session_id, 
         mh_y_bpm__ext_sum) %>% #externalizing summary
  mutate(mh_y_bpm__ext_sum = if_else(mh_y_bpm__ext_sum=="n/a", NA, as.numeric(mh_y_bpm__ext_sum))) 

# Internalizing dataframe # 
Internalizing_clean <- Psychopathology %>%
  select(participant_id, session_id, 
         mh_y_bpm__int_sum) %>% #internalizing summary
  mutate(mh_y_bpm__int_sum = if_else(mh_y_bpm__int_sum=="n/a", NA, as.numeric(mh_y_bpm__int_sum)))

# Merge PLE severity scores to one number #
PLEs_wide <- PLEs_clean %>%
  pivot_wider(names_from = "session_id", values_from = "mh_y_pps__severity_score") %>%
  rowwise() %>%
  mutate(PLEs_total = sum(c(`ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`), na.rm = TRUE)) %>%
  select('participant_id', 'PLEs_total')

# Merge Attention to one number #
Attention_wide <- Attention_clean %>%
  pivot_wider(names_from = "session_id", values_from = "mh_y_bpm__attn_sum") %>%
  rowwise() %>%
  mutate(Attention_total = sum(c(`ses-00M`, `ses-01A`, `ses-01M`, `ses-02A`, `ses-02M`, `ses-03A`, `ses-03M`, `ses-04A`, `ses-05A`, `ses-06A`), na.rm = TRUE)) %>% 
  select('participant_id', 'Attention_total')

# Merge Internalizing to one number #
Internalizing_wide <- Internalizing_clean %>%
  pivot_wider(names_from = "session_id", values_from = "mh_y_bpm__int_sum") %>%
  rowwise() %>%
  mutate(Internalizing_total = sum(c(`ses-00M`, `ses-01A`, `ses-01M`, `ses-02A`, `ses-02M`, `ses-03A`, `ses-03M`, `ses-04A`, `ses-05A`, `ses-06A`), na.rm = TRUE)) %>%
  select('participant_id', 'Internalizing_total')

# Merge Externalizing to one number #
Externalizing_wide <- Externalizing_clean %>%
  pivot_wider(names_from = "session_id", values_from = "mh_y_bpm__ext_sum") %>%
  rowwise() %>%
  mutate(Externalizing_total = sum(c(`ses-00M`, `ses-01A`, `ses-01M`, `ses-02A`, `ses-02M`, `ses-03A`, `ses-03M`, `ses-04A`, `ses-05A`, `ses-06A`), na.rm = TRUE)) %>%
  select('participant_id', 'Externalizing_total')

# Merge PLE and General Psychopathology dataframe #
GeneralPsychopathology_clean <- PLEs_wide
GeneralPsychopathology_clean <- GeneralPsychopathology_clean %>%
  full_join (Attention_wide, by = 'participant_id') %>%
  full_join (Internalizing_wide, by = 'participant_id') %>%
  full_join (Externalizing_wide, by = 'participant_id') %>%
  drop_na()

# Create General Psychopathology score #
GeneralPsychopathology_clean <- GeneralPsychopathology_clean %>%
  mutate(P_Factor_Total = PLEs_total + Externalizing_total + Internalizing_total + Attention_total)

# Create P_Factor Z-score #
GeneralPsychopathology_zscores <- GeneralPsychopathology_clean %>%
  mutate(P_Factor = (P_Factor_Total - mean(P_Factor_Total)) / sd(P_Factor_Total)) %>%
  select(participant_id, P_Factor)


####################
##  rs-fMRI Data  ##
####################

# Clean FPN-Amygdala #
RS_FC_FPN_Amygdala_clean <- RS_FC_FPN_Amygdala %>%
  select(participant_id, session_id, mr_y_rsfmri__corr__gpnet__aseg__frp__ag__lh_mean, mr_y_rsfmri__corr__gpnet__aseg__frp__ag__rh_mean) %>%
  rename('FPN_Amygdala_LH'= mr_y_rsfmri__corr__gpnet__aseg__frp__ag__lh_mean, 'FPN_Amygdala_RH' = mr_y_rsfmri__corr__gpnet__aseg__frp__ag__rh_mean,
         'fMRI_session_id' = session_id)

# Clean FPN-FPN #
RS_FC_FPN_FPN_clean <- RS_FC_FPN_FPN %>%
  select(participant_id, session_id, mr_y_rsfmri__corr__gpnet__frp__frp_mean) %>%
  rename('FPN_FPN' = mr_y_rsfmri__corr__gpnet__frp__frp_mean,'fMRI_session_id' = session_id)

# Merge FPN-Amygdala and FPN-FPN data #
rs_fMRI_clean <- RS_FC_FPN_Amygdala_clean %>%
  left_join(RS_FC_FPN_FPN_clean, by=c("participant_id", "fMRI_session_id"))

# Create average FPN_Amygdala RS-FC across hemispheres #
rs_fMRI_clean <- rs_fMRI_clean %>%
  mutate('FPN_Amygdala' = (FPN_Amygdala_LH + FPN_Amygdala_RH)/2) %>%
  select(-FPN_Amygdala_LH, FPN_Amygdala_RH)

# Filter for timepoints closest to ERQ timepoint (ses-02A, followed by ses-04A) #
rs_fMRI_clean <- rs_fMRI_clean %>%
  mutate(ses_priority = match(fMRI_session_id, c("ses-02A", "ses-04A", "ses-00A"))) %>%
  arrange(participant_id, ses_priority) %>%
  group_by(participant_id) %>%
  slice(1) %>%
  ungroup() %>%
  select(-ses_priority)

# Convert fMRI data into Z-scores #
rs_fMRI_clean <- rs_fMRI_clean %>%
  mutate(FPN_Amygdala = scale(FPN_Amygdala)) %>%
  mutate(FPN_FPN = scale(FPN_FPN))


###########################
##  MRI Scanner ID Data  ##
###########################

# Clean Scanner ID information #
Scanner_ID_clean <- Scanner_ID %>%
  select(participant_id, session_id, ab_g_dyn__design_mr__model) %>%   # Scanner model
  rename('Scanner_ID' = ab_g_dyn__design_mr__model, 'fMRI_session_id' = session_id)

# Match Scanner ID to fMRI data #
rs_fMRI_clean <- rs_fMRI_clean %>%
  left_join(Scanner_ID_clean, by = c("participant_id", "fMRI_session_id"))


#####################
##  fMRI Age Data  ##
#####################

# Pull fMRI age data #
Ages <- Ages %>%
  select(participant_id, session_id, ab_g_dyn__visit_age) %>%
  rename(fMRI_session_id = session_id, fMRI_age = ab_g_dyn__visit_age)

# Create fMRI-age column #
rs_fMRI_clean <- rs_fMRI_clean %>%
  left_join(Ages, by = c("participant_id", "fMRI_session_id"))


####################
##  fMRI QC Data  ##
####################

# Clean fMRI QC data #
fMRI_QC_clean <- fMRI_QC %>% 
  select(participant_id, session_id, mr_y_qc__raw__rsfmri__mot__comp_mean, mr_y_qc__raw__rsfmri__aqc_indicator) %>%  
  rename('MFWD' = mr_y_qc__raw__rsfmri__mot__comp_mean, 'QC_Flag' = mr_y_qc__raw__rsfmri__aqc_indicator, 'fMRI_session_id' = session_id)

# Only Keep RS-FC Data that Passed QC #
fMRI_QC_clean <- fMRI_QC_clean %>%
  filter(QC_Flag == 1)
  
# Join QC Info In # 
rs_fMRI_clean <- rs_fMRI_clean %>%
  left_join(fMRI_QC_clean, by = c("participant_id", "fMRI_session_id"))


##################
##  Merge Data  ##
##################

# Create dataframe for just behavioral data #
SRP_Behav_Data <- ACEs_clean %>%
  left_join(ERQ_clean, by = "participant_id") %>%
  left_join(GeneralPsychopathology_zscores, by = "participant_id") %>%
  left_join(Participant_Sex, by = "participant_id")

# Create dataframe that includes fMRI data
SRP_Brain_Data <- SRP_Behav_Data %>%
  left_join(rs_fMRI_clean, by = "participant_id")   


table(SRP_Brain_Data$fMRI_session_id)  # ses-0 = 11,268, ses-2 = 7923, ses-4 = 6274, ses-6 = 4059
table(SRP_Brain_Data$ERQ_session_id) # ses-3 = 27167, ses-4 = 811, ses-5 = 168, ses-6 = 51 


# Ensure participants only have 1 row each #
SRP_Behav_Data <- SRP_Behav_Data %>%
  distinct(participant_id, .keep_all = TRUE)

SRP_Brain_Data <- SRP_Brain_Data %>%
  distinct(participant_id, .keep_all = TRUE)

# Remove particpants without relevant data # 
SRP_Behav_Data <- SRP_Behav_Data %>%
  drop_na()

SRP_Brain_Data <- SRP_Brain_Data %>%
  drop_na()

# 11,868 -> 8,636 (with only behavioral data) -> 8,535 (with neuroimaging as well) participants for final analyses


#########################
##  Linear Model Time  ##
#########################

## Aim 1: Does Adverse Childhood Experience and Emotion Regulation style score predict later psychopathology 
## Aim 2: Does Emotion Regulation style predict later psychopathology 

P_Factor_Behavioral_Model <- lm(P_Factor ~ Suppression_Mean*ACEs_Total + Reappraisal_Mean*ACEs_Total + sex + demo_comb_income_hrcode + race_ethnicity_hrcode, data = SRP_Behav_Data)
summary(P_Factor_Behavioral_Model)
  # Suppression: t = 21.223, p<.001 ***
  # Reappraisal: t = -6.136, p<.001 ***
  # ACEs: t = 17.020, p<.001 ***

## Aim 3: Does Adverse Childhood Experience Score predict later Emotion Regulation Strategy Usage (Suppression and Reappraisal)

ACEs_Suppression_Model <- lm(Suppression_Mean ~ ACEs_Total + sex + demo_comb_income_hrcode + race_ethnicity_hrcode, data = SRP_Behav_Data)
summary(ACEs_Suppression_Model)
  # ACEs: t = 5.972, p<.001 ***

#ACEs_Suppression_Model_Sens <- lm(Suppression_Mean ~ ACEs_Total + Reappraisal_Mean + sex + demo_comb_income_hrcode + race_ethnicity_hrcode, data = SRP_Behav_Data)
#summary(ACEs_Suppression_Model)

ACEs_Reappraisal_Model <- lm(Reappraisal_Mean ~ ACEs_Total + sex + demo_comb_income_hrcode + race_ethnicity_hrcode, data = SRP_Behav_Data)
summary(ACEs_Reappraisal_Model)
  # ACEs: t = -0.543, p = 0.587


## Aim 4a: Do patterns of Resting State Functional Connectivity in our selected networks (FPN-Amygdala, FPN-FPN) and ACE exposure predict psychopathology

P_Factor_RS_FC_Model <- lm(P_Factor ~ FPN_Amygdala * ACEs_Total + FPN_FPN * ACEs_Total + Scanner_ID + MFWD + sex + demo_comb_income_hrcode + race_ethnicity_hrcode, data = SRP_Brain_Data)
summary(P_Factor_RS_FC_Model)
  # FPN_Amygdala: t = 2.577, p = 0.009981 ** 
  # ACEs: t = 17.461, p<.001 ***
  # FPN_FPN: t = -0.209, p = 0.834745 


## Aim 4b: Do our selected Resting State Networks (FPN-Amygdala, FPN-FPN) appear to underlie the usage of Emotion Regulation Strategies

Reappraisal_RS_FC_Model <- lm(Reappraisal_Mean ~ FPN_Amygdala * ACEs_Total + FPN_FPN * ACEs_Total + Scanner_ID + MFWD + sex + demo_comb_income_hrcode + race_ethnicity_hrcode, data = SRP_Brain_Data)
summary(Reappraisal_RS_FC_Model)
  # FPN_Amygdala: t = -0.760, p = 0.44730    
  # ACEs: t = -0.365, p = 0.71503    
  # FPN_FPN: t = 0.483, p = 0.62888 


Suppression_RS_FC_Model <- lm(Suppression_Mean ~ FPN_Amygdala * ACEs_Total + FPN_FPN * ACEs_Total + Scanner_ID + MFWD + sex + demo_comb_income_hrcode + race_ethnicity_hrcode, data = SRP_Brain_Data)
summary(Suppression_RS_FC_Model)
  # FPN_Amygdala: t = 1.164, p = 0.24451    
  # ACEs_Total: t = 5.872, p<.001 ***
  # FPN_FPN: t = 1.306, p = 0.19152 

# remove ACEs





## Due to non-significant interactive effects, Aim 4a and 4b report linear relationships between       
## functional connectivity and psychopathology (4a) as well as emotion regulation strategy usage (4b)  


################
##  Graphing  ##
################

## Aim 4b was not graphed on the poster ##
    # Results from the model were insignificant
    # I ran out of space and got cranky about trying to make another graph fit

## Aim 1: ACEs and Psychopathology plot ##
ggplot(SRP_Behav_Data, aes(x = ACEs_Total)) +
  geom_smooth(aes(y = P_Factor), method = "lm", color = "black", size = 3) +
  coord_cartesian(xlim = c(-1,5), ylim = c(-0.5, 1.5)) +
  labs(x = "Exposure to ACEs",
       y = "General Psychopathology")


## Aim 2: Emotion Regulation and Psychopathology ##
ggplot(SRP_Behav_Data, aes(x = P_Factor)) +
  geom_smooth(aes(y = Reappraisal_Mean), method = "lm", color = "green", size = 3) +
  geom_smooth(aes(y = Reappraisal_Mean), method = "lm", color = "green", size = 3, linetype = "dotted") +
  geom_smooth(aes(y = Suppression_Mean), method = "lm", color = "orange", size = 3) +
  coord_cartesian(xlim = c(-1.5,7.5), ylim = c(-0.5, 2)) +
  labs(x = "General Psychopathology",
       y = "Emotion Regulation Strategy Usage")


## Aim 3: Emotion Regulation and ACEs plot ##
ggplot(SRP_Behav_Data, aes(x = ACEs_Total)) +
  geom_smooth(aes(y = Reappraisal_Mean), method = "lm", color = "green", size = 3) +
  geom_smooth(aes(y = Reappraisal_Mean), method = "lm", color = "green", size = 3, linetype = "dotted") +
  geom_smooth(aes(y = Suppression_Mean), method = "lm", color = "orange", size = 3) +
  coord_cartesian(xlim = c(-1,5), ylim = c(-0.2, 0.6)) +
  labs(x = "Exposure to ACEs",
       y = "Emotion Regulation Strategy Usage")


## Aim 4a: Functional Connectivity * ACEs and Psychopathology Plot ##
ggplot(SRP_Brain_Data, aes(x = P_Factor)) +
  geom_smooth(aes(y = FPN_FPN), method = "lm", color = "black", size = 3) +
  geom_smooth(aes(y = FPN_FPN), method = "lm", color = "gray80", size = 3, linetype = "dotted") +
  geom_smooth(aes(y = FPN_Amygdala), method = "lm", color = "gray5", size = 3) +
  coord_cartesian(xlim = c(-1, 4), ylim = c(-0.5, 0.5)) +
  labs(x = "General Psychopathology",
       y = "Functional Connectivity")



#########################
## Final Sanity Checks ##
#########################

#describe(SRP_Behav_Data)
#describe(SRP_Brain_Data)



# "     _|_     "
# "    /o o\    "   # goodbye! 
# "    \___/    "
# "      |/     "
# "     /|      "
# "     / \     "