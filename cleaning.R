# Cleaning for Openness & CTN
# May 9 2023 10.30am - 26 May 2023, 5pm

library(tidyverse)
library(lubridate)
# ctrl + shift + m = pipe %>% 


data <-
     read_csv("DATA_personality_ctn_June_1_2023.csv")

#remove first two rows
data <-
     data[-(1:2),]

# Remove test subjects - anything that was recorded before May 9 2023, 10.30am
# Make sure Start Date is read as a date
data <-
     data %>% 
     mutate(StartDate = ymd_hms(StartDate, tz = "America/Los_Angeles"))
# It is also important to make sure the time zone is set to the right time so when you remove participants they will be in relation to the time YOU actually started the study (not their local time)
# to see all the timezones that you might need use: OlsonNames()

str(data$StartDate) #checking the structure of the date
# The StartDate variable will now show as POSIxct, which is the code R uses for a unicode data


#filter out particpants' rows before start date
data <-
     data %>% 
     filter(StartDate > "2023-05-09 10:30:00 UTC")
# Note: You'll need to place your date/time in " " for R to read it and make sure to end with UTC


#how many people started the study
#N = 303
length(data$id)

# Attrition
# remove people that did not finish the study
data <-
     data %>% 
     filter(Progress == "100")

#check on our N
# N = 275
# 28 people did not finish the study
length(data$id)



# Time outliers -> +- 3SD mean_time

# check to see if variable (Duration (in seconds)) is the right structure
str(data$`Duration (in seconds)`)

#saved as character -> we will need to change to numeric
data <-
     data %>% 
     mutate(time = as.numeric(`Duration (in seconds)`)) #mutate -> new variable = what you want to do this variable

str(data$time)

# look at the distribution of time taken using ggplot()
data %>% 
     ggplot() +
     geom_freqpoly(aes(time))

range(data$time)

# change to minutes
data <-
     data %>% 
     mutate(time = time/60)

range(data$time)

# re-run graph after converting the time to minutes
data %>% 
     ggplot() +
     geom_freqpoly(aes(time))

# create mean, sd, and 3+sd so we can filter out the outliers
m_time <- mean(data$time) #generates the mean time
sd_time <- sd(data$time) #generates the standard deviation of time
upper_time <- m_time + (3*sd_time) #creating an upper limit that is 3xSD +  mean

# look at who the outliers are
data %>% 
     filter(time >= upper_time) %>% 
     select(id, time) #selects rows that we want to see

# filter out the outlier (1 person)
data <-
     data %>% 
     filter(time < upper_time)

# count how many participants are left after filter for time outliers
length(data$id)
# 1 person removed because they took over 3sd above the mean in time
# 274


## Objective Attention checks
# ATT_1_agree_a_little -> select "4" to pass
# att_2_2 -> select "2" to pass
# ATT_3_stronglydisagree -> select "1" to pass
# we will keep anyone who passed all three or only failed 1 attention check

# all attention checks are recorded as character
# change to numeric
# and combine to one attention score

# using ifelse() function to code attention scores as 1 (pass) or 0 (fail) 
# more instructions in getting started with data analysis document in google drive

data <-
     data %>% 
     mutate(att_1 = ifelse(as.numeric(ATT_1_agree_a_little) != 4, 0, 1),
            att_2 = ifelse(as.numeric(att_2_2) != 2, 0, 1),
            att_3 = ifelse(as.numeric(Att_3_stronglydisagree) != 1, 0, 1),
            att_total = (att_1 + att_2 + att_3))

# keep people who scored 2 or 3 in att_total
data <-
  data %>% 
  filter(att_total >=2)


length(data$id)
# 267
# 7 people failed 1 or more attention check and were removed

# Subjective Validity Check
#subjective_att

# stored as character, need to change to numeric
data <-
     data %>% 
     mutate(subjective_att = as.numeric(subjective_att))

#only keep people who scored 1 or 2 
data %>% 
     group_by(subjective_att) %>% 
     summarise(n = length(id))

data <-
     data %>% 
     filter(subjective_att <= 2)

length(data$id)
# 262
# 5 people reported that they did not pay attention or answer honestly in the study

### Setting up Variables
# tackle each scale at a time
# make sure they are the correct structure (numeric or factor)
# reverse score where needed
# combine to single score for multi-item scales
# standardize scores (turn into a z score)


# Big 5 Openness Scale
# BFI_o19, BFI_o20, BFI_o21, BFI_o22, BFI_o23, BFI_o24, BFI_o25, BFI_o26, BFI_o27, BFI_028
# check structure - do we need to make it numeric
data <-
     data %>% 
     mutate(BFI_O19 = as.numeric(BFI_O19), #changing to a numeric structure as they are all character
            BFI_O20 = as.numeric(BFI_O20),
            BFI_O21 = as.numeric(BFI_O21),
            BFI_O22 = as.numeric(BFI_O22),
            BFI_O23 = as.numeric(BFI_O23),
            BFI_O24 = as.numeric(BFI_O24),
            BFI_O25 = as.numeric(BFI_O25),
            BFI_O26 = as.numeric(BFI_O26),
            BFI_O27 = as.numeric(BFI_O27),
            BFI_O28 = as.numeric(BFI_O28))

#reverse score items BFI_o25 & BFI_o27
data$BFI_O25 <-
     dplyr::recode(data$BFI_O25, #use dplyer::recode, to call the package dplyer and use its recode function
                   "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)
## Deb to check what is going on with Recode - it might need to be replaced with case_match!! Figure this out

data$BFI_O27 <-
     dplyr::recode(data$BFI_O27,
                   "1" = 5, "2" = 4, "3" = 3, "4" = 2, "5" = 1)


#combine to a single variable and standardize
data <-
     data %>% 
     mutate(openness = BFI_O19 + BFI_O20 + BFI_O21 + BFI_O22 + BFI_O23 +
                 BFI_O24 + BFI_O25 + BFI_O26 + BFI_O27 + BFI_O28,
            openness_z = (openness - mean(openness))/sd(openness)) 



round(mean(data$openness_z),3)
sd(data$openness_z)

mean(data$openness)

#check on internal consistency for Openness
# alpha > .70 indicates good internal consistency
# you should report the cronbachs alpha when you talk about each scale
data %>% 
     dplyr::select(BFI_O19, BFI_O20, BFI_O21, BFI_O22, BFI_O23,
                 BFI_O24, BFI_O25, BFI_O26, BFI_O27, BFI_O28) %>% 
     ltm::cronbach.alpha()



# DEEP CTN Scale - Overall
# check structure - do we need to make it numeric
data <-
     data %>% 
     mutate(DEEPCTN_s_1 = as.numeric(DEEPCTN_s_1), #Deep CTN Sub-scale
            DEEPCTN_s_2 = as.numeric(DEEPCTN_s_2),
            DEEPCTN_s_3 = as.numeric(DEEPCTN_s_3),
            DEEPCTN_s_4 = as.numeric(DEEPCTN_s_4),
            DEEPCTN_s_5 = as.numeric(DEEPCTN_s_5),
            DEEPCTN_s_6 = as.numeric(DEEPCTN_s_6),
            DEEPCTN_s_7 = as.numeric(DEEPCTN_s_7),
            DEEPCTN_s_8 = as.numeric(DEEPCTN_s_8),
            DEEPCTN_s_9 = as.numeric(DEEPCTN_s_9),
            DEEPCTN_s_10 = as.numeric(DEEPCTN_s_10),
            
            DEEPCTN_ex_1 = as.numeric(DEEPCTN_ex_1), #Experiential CTN Sub-scale
            DEEPCTN_ex_2 = as.numeric(DEEPCTN_ex_2),
            DEEPCTN_ex_3 = as.numeric(DEEPCTN_ex_3),
            DEEPCTN_ex_4 = as.numeric(DEEPCTN_ex_4),
            DEEPCTN_ex_5 = as.numeric(DEEPCTN_ex_5),
            DEEPCTN_ex_6 = as.numeric(DEEPCTN_ex_6),
            DEEPCTN_ex_7 = as.numeric(DEEPCTN_ex_7),
            DEEPCTN_ex_8 = as.numeric(DEEPCTN_ex_8),
            
            DEEPCTN_em_1 = as.numeric(DEEPCTN_em_1), #Emotional CTN Sub-scale
            DEEPCTN_em_2 = as.numeric(DEEPCTN_em_2),
            DEEPCTN_em_3 = as.numeric(DEEPCTN_em_3),
            DEEPCTN_em_4 = as.numeric(DEEPCTN_em_4),
            DEEPCTN_em_6 = as.numeric(DEEPCTN_em_6),
            
            DEEPCTN_m_1 = as.numeric(DEEPCTN_m_1), #Presence CTN Sub-scale
            DEEPCTN_m_2 = as.numeric(DEEPCTN_m_2),
            DEEPCTN_m_3 = as.numeric(DEEPCTN_m_3),
            DEEPCTN_m_4 = as.numeric(DEEPCTN_m_4),
            DEEPCTN_m_5 = as.numeric(DEEPCTN_m_5),
            DEEPCTN_m_6 = as.numeric(DEEPCTN_m_6))

# combine to single score
# standardize (z score)

data <-
     data %>% 
     mutate(CTN_overall =
           DEEPCTN_s_1 + DEEPCTN_s_2 + DEEPCTN_s_3 + DEEPCTN_s_4 +
           DEEPCTN_s_6 + DEEPCTN_s_7 + DEEPCTN_s_8 + DEEPCTN_s_9 + DEEPCTN_s_10 +
           
           DEEPCTN_ex_1 + DEEPCTN_ex_2 + DEEPCTN_ex_3 + DEEPCTN_ex_4 +
           DEEPCTN_ex_5 + DEEPCTN_ex_6 + DEEPCTN_ex_7 + DEEPCTN_ex_8 +
           
           DEEPCTN_em_1 + DEEPCTN_em_2 + DEEPCTN_em_3 + DEEPCTN_em_4 +
           DEEPCTN_em_6 +
          
           DEEPCTN_m_1 + DEEPCTN_m_2 + DEEPCTN_m_3 + DEEPCTN_m_4 +
           DEEPCTN_m_5 + DEEPCTN_m_6,
           
             CTN_overall_z = (CTN_overall - mean(CTN_overall))/sd(CTN_overall),
            
            CTN_Deep = DEEPCTN_s_1 + DEEPCTN_s_2 + DEEPCTN_s_3 + DEEPCTN_s_4 +  
                 DEEPCTN_s_6 + DEEPCTN_s_7 + DEEPCTN_s_8 + DEEPCTN_s_9 + DEEPCTN_s_10,
            CTN_Deep_z = (CTN_Deep - mean(CTN_Deep))/sd(CTN_Deep),
            
            CTN_Exp = DEEPCTN_ex_1 + DEEPCTN_ex_2 + DEEPCTN_ex_3 + DEEPCTN_ex_4 + 
                 DEEPCTN_ex_5 + DEEPCTN_ex_6 + DEEPCTN_ex_7 + DEEPCTN_ex_8,
            CTN_Exp_z = (CTN_Exp - mean(CTN_Exp))/sd(CTN_Exp),
            
            CTN_Emo = DEEPCTN_em_1 + DEEPCTN_em_2 + DEEPCTN_em_3 + DEEPCTN_em_4 + DEEPCTN_em_6,
            CTN_Emo_z = (CTN_Emo - mean(CTN_Emo))/sd(CTN_Emo),
            
            CTN_Pres =  DEEPCTN_m_1 + DEEPCTN_m_2 + DEEPCTN_m_3 + 
                 DEEPCTN_m_4 + DEEPCTN_m_5 + DEEPCTN_m_6,
            CTN_Pres_z = (CTN_Pres - mean(CTN_Pres))/sd(CTN_Pres))



#check on internal consistency for all the ctn scales
# alpha > .70 indicates good internal consistency
# you should report the cronbachs alpha when you talk about each scale
data %>% 
     dplyr::select(DEEPCTN_s_1, DEEPCTN_s_2, DEEPCTN_s_3, DEEPCTN_s_4, #overall CTN
                 DEEPCTN_s_6, DEEPCTN_s_7, DEEPCTN_s_8, DEEPCTN_s_9, DEEPCTN_s_10,
                 DEEPCTN_ex_1, DEEPCTN_ex_2, DEEPCTN_ex_3, DEEPCTN_ex_4,
                 DEEPCTN_ex_5, DEEPCTN_ex_6, DEEPCTN_ex_7, DEEPCTN_ex_8,
                 DEEPCTN_em_1, DEEPCTN_em_2, DEEPCTN_em_3, DEEPCTN_em_4, DEEPCTN_em_6,
                 DEEPCTN_m_1, DEEPCTN_m_2, DEEPCTN_m_3,
                 DEEPCTN_m_4, DEEPCTN_m_5, DEEPCTN_m_6) %>% 
     ltm::cronbach.alpha()
#why are Deb & Cameron getting different Alpha values


data %>% 
     dplyr::select(DEEPCTN_s_1, DEEPCTN_s_2, DEEPCTN_s_3, DEEPCTN_s_4,  #deep CTN
            DEEPCTN_s_6, DEEPCTN_s_7, DEEPCTN_s_8, DEEPCTN_s_9, DEEPCTN_s_10) %>% 
     ltm::cronbach.alpha()

data %>% 
     dplyr::select(DEEPCTN_ex_1, DEEPCTN_ex_2, DEEPCTN_ex_3, DEEPCTN_ex_4, #experiential CTN
            DEEPCTN_ex_5, DEEPCTN_ex_6, DEEPCTN_ex_7, DEEPCTN_ex_8) %>% 
     ltm::cronbach.alpha()

data %>% 
     dplyr::select(DEEPCTN_em_1, DEEPCTN_em_2, DEEPCTN_em_3, DEEPCTN_em_4, DEEPCTN_em_6 #emotional ctn
            ) %>% 
     ltm::cronbach.alpha()

data %>% 
     dplyr::select(DEEPCTN_m_1, DEEPCTN_m_2, DEEPCTN_m_3, #presence CTN
            DEEPCTN_m_4, DEEPCTN_m_5, DEEPCTN_m_6) %>% 
     ltm::cronbach.alpha()


#Demographics & Descriptives
#age
data <- 
     data %>% 
     mutate(age = as.numeric(age))

mean(data$age, na.rm = T)
sd(data$age, na.rm = T)
range(data$age, na.rm = T)

#SES
data <-
     data %>% 
     mutate(SES_family = as.numeric(SES_family),
            SES_peers = as.numeric(SES_peers),
            SES = (SES_family + SES_peers)/2)

#check on internal consistency for SES
# alpha > .70 indicates good internal consistency
# you should report the cronbachs alpha when you talk about each scale
data %>% 
     dplyr::select(SES_family, SES_peers) %>% 
     ltm::cronbach.alpha()

mean(data$SES)
sd(data$SES)


#Politics
data <-
     data %>% 
     mutate(politics_overall = as.numeric(politics_overall),
            politics_economic = as.numeric(politics_economic),
            politics_social = as.numeric(politics_social),
            politics = (politics_overall + politics_economic + politics_social)/3)

#check on internal consistency for politics
# alpha > .70 indicates good internal consistency
# you should report the cronbachs alpha when you talk about each scale
data %>% 
     dplyr::select(politics_overall, politics_economic, politics_social) %>% 
     ltm::cronbach.alpha()

mean(data$politics)
sd(data$politics)

#Gender
#1 = Man; 2 = Woman, 3 = Trans man, 4 = Trans woman, 5 = Non-binary, 7 = No response 
data <-
     data %>% 
     mutate(gender = factor(gender, levels = c("1", "2", "3", "4", "5", "7"), 
                            labels = c("Man", "Woman", "Trans man", "Trans woman", 
                                       "Non-binary", "No response")))

data %>% 
     group_by(gender) %>% 
     summarise(n = length(id))

#Race
# 1 = Asian, 2 = White, 3 = Hispanic, 4 = Black, 5 = Middle Eastern, 
# 6 = Pacific Islander, 7 = Indigenous American, 8 = Mixed, 9 = Prefer not to say
data <-
     data %>% 
     mutate(ethnoracial = factor(ethnoracial, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"), 
                            labels = c("Asian", "White", "Hispanic", "Black", 
                                       "Middle Eastern", "Pacific Islander", "Indigenous American",
                                       "Mixed", "No response")))

data %>% 
     group_by(ethnoracial) %>% 
     summarise(n = length(id))




#after cleaning - save the csv file to be run in R markdown
data %>% 
write_csv("clean_data.csv")
