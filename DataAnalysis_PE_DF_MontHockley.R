#Data cleaning and analysis for Production by typing using semantically 
#similar words. Lists are presented at study
#materials from mont & hockley

#Getting the data off of Google's Firebase

#this will install the package fireData from GitHub
if (!require("devtools")) install.packages("devtools")
devtools::install_github("Kohze/fireData")
library(fireData) # https://github.com/Kohze/fireData
dataBackup(projectURL = "https://ProductionDFMontHockley-default-rtdb.firebaseio.com/",   #change
           secretKey = "p7bXx8s6R4DKWRDm0FDygznUqfVLaeF68deQsVsO",     #change
           "/Users/jackiespear/ProductionDFMontHockley.json")
#"/Users/umspear-ins/productiontyping3.json")              #change 

##############################################################################
library("tidyverse")
library(tidyr)
library(plyr)
#install.packages("jsonlite")
library("jsonlite")
data<-unlist(jsonlite::fromJSON("/Users/jackiespear/ProductionDFMontHockley.json"), recursive = FALSE, use.names = TRUE)
#install.packages("dplyr")
library(dplyr)

glimpse(data, max.level = 3, list.len = 4)
#data <- enframe(unlist(data))

data <- bind_rows(data) # this doesn't work because "before" and "after are different length dataframes. need to sort these before this

#View(data)

total_subjects_run <- length(unique(data$subject))
total_subjects_run
#dat_study[dat_study$category %in% c("produce"), ]

#data <- data[!grepl("<p style='font-size: 28px; color: red;'>", data$instruc),] #this reomves the entire row
#data$stimulus <- gsub('<span style="color: green">', "",as.character(data$word2)) #removing the html formatting from read/produce instruction
#data$stimulus <- gsub('<span style="color: red">', "",as.character(data$word2))
#data$stimulus <- gsub("</span></p>", "",as.character(data$word2))
data$response <- gsub('list\\(response \\= "', "",as.character(data$response))
data$response <- gsub('"\\)', "",as.character(data$response))
data$response <- gsub('list\\(gender \\= "', "",as.character(data$response))
data$response <- gsub('list\\(age \\= "', "",as.character(data$response))
#data$stimulus <- gsub('<p style="font-size: 48px;">', "",as.character(data$stimulus))
#data$stimulus <- gsub('</p>', "",as.character(data$stimulus))

#Counting the number of attention checks per subject
attention_checks <- data %>%
  group_by(subject) %>%
  dplyr::summarise(Number = sum(phase == "Attention_Check", na.rm=TRUE))

#data <- data %>% unnest_wider(data, names_sep = "_")

data <- unnest_wider(data = data,col = c(data, type), names_sep = "_")

data <- data %>% 
  dplyr::rename("category" = "data_category",
                "word_condition" = "type_condition",
                "practice" = "type_type",
                "data_condition" = "data_condition",
                "type2" = "data_type",
                "word" = "data_word")

data$data_condition[data$data_condition=="New"]<-"new"


dat_test <- data[data$phase %in% c("test"), ]
dat_study <- data[data$phase %in% c("study"), ]
dat_demo <- data[data$phase %in% c("Demographics"),]
dat_demo <- dat_demo[,c('subject','type_dem','response')]
dat_demo <- dat_demo %>% 
  pivot_wider(names_from = type_dem, values_from = response)

dat_test$response <- ifelse(dat_test$response =="y", 1, 0) #changing letters to a number for analysis 

#subsetting the data into the 2 between subject experiments - directed forgetting or production effect
dat_test_PE <- dat_test[dat_test$experiment %in% c("production_effect"), ]
dat_test_DF <- dat_test[dat_test$experiment %in% c("directed_forgetting"), ]

#subsetting the data into the 2 between subject conditions - presentation during or after
dat_test_during_PE <- dat_test_PE[dat_test_PE$cue_presentation %in% c("during"), ]
dat_test_after_PE <-  dat_test_PE[dat_test_PE$cue_presentation %in% c("after"), ]

dat_test_during_DF <- dat_test_DF[dat_test_DF$cue_presentation %in% c("during"), ]
dat_test_after_DF <-  dat_test_DF[dat_test_DF$cue_presentation %in% c("after"), ]

#subsetting data into the 3 between-subject conditions for the production effect, for during and after
dat_test_mixed_PE_during <- dat_test_during_PE[dat_test_during_PE$condition %in% c("mixed"), ] 
dat_test_pureread_PE_during <- dat_test_during_PE[dat_test_during_PE$condition %in% c("pureread"), ]
dat_test_pureproduce_PE_during <- dat_test_during_PE[dat_test_during_PE$condition %in% c("pureproduce"), ]

dat_test_mixed_PE_after <- dat_test_after_PE[dat_test_after_PE$condition %in% c("mixed"), ] 
dat_test_pureread_PE_after <- dat_test_after_PE[dat_test_after_PE$condition %in% c("pureread"), ]
dat_test_pureproduce_PE_after <- dat_test_after_PE[dat_test_after_PE$condition %in% c("pureproduce"), ]

#subsetting data into the 3 between-subject conditions for directed forgetting, for during and after
dat_test_mixed_DF_during <- dat_test_during_DF[dat_test_during_DF$condition %in% c("mixed"), ] 
dat_test_pureread_DF_during <- dat_test_during_DF[dat_test_during_DF$condition %in% c("pureread"), ]
dat_test_pureproduce_DF_during <- dat_test_during_DF[dat_test_during_DF$condition %in% c("pureproduce"), ]

dat_test_mixed_DF_after <- dat_test_after_DF[dat_test_after_DF$condition %in% c("mixed"), ] 
dat_test_pureread_DF_after <- dat_test_after_DF[dat_test_after_DF$condition %in% c("pureread"), ]
dat_test_pureproduce_DF_after <- dat_test_after_DF[dat_test_after_DF$condition %in% c("pureproduce"), ]


#didn't change test lists for the pure conditions, so renaming the conditions to reflect what subjects actually saw i.e. all read of all produce
dat_test_pureread_PE_after$data_condition[dat_test_pureread_PE_after$data_condition == "produce"] <- "read"
dat_test_pureread_PE_during$data_condition[dat_test_pureread_PE_during$data_condition == "produce"] <- "read"
dat_test_pureread_DF_after$data_condition[dat_test_pureread_DF_after$data_condition == "produce"] <- "read"
dat_test_pureread_DF_during$data_condition[dat_test_pureread_DF_during$data_condition == "produce"] <- "read"

dat_test_pureproduce_PE_during$data_condition[dat_test_pureproduce_PE_during$data_condition == "read"] <- "produce"
dat_test_pureproduce_PE_after$data_condition[dat_test_pureproduce_PE_after$data_condition == "read"] <- "produce"
dat_test_pureproduce_DF_during$data_condition[dat_test_pureproduce_DF_during$data_condition == "read"] <- "produce"
dat_test_pureproduce_DF_after$data_condition[dat_test_pureproduce_DF_after$data_condition == "read"] <- "produce"


#then subsetting the data into produce and read instructions from the STUDY phase for data cleaning purpose
#only for the production effect. no typing manipulatoin during directed forgetting 
dat_study_prod <- dat_study[dat_study$data_condition %in% c("produce") & dat_study$experiment == "production_effect", ]
dat_study_read <- dat_study[dat_study$data_condition %in% c("read") & dat_study$experiment == "production_effect", ] #only the read items to look at response to make sure s's are not typing the whole time


#now some data cleaning
#getting number of "yes" responses for delinquent responding during test phase
button_mashing <- dat_test %>%
  group_by(subject) %>%
  dplyr::summarise(Yes_Button_Presses = sum(response == "y", na.rm=TRUE))

#counting the number of times s's did not type when they should have (for produced items) during the study phase
not_typing_sums <- dat_study_prod %>%
  group_by(subject) %>%
  dplyr::summarise(Number = sum(response =="", na.rm=TRUE))

#number of times s's typed when they shouldn't have (for read items) during the study phase 
typing_sums <- dat_study_read %>%
  group_by(subject) %>%
  dplyr::summarise(Number = sum(response !="", na.rm=TRUE))

length(which(not_typing_sums$Number > 6)) #counting s's that did not TYPE at least 80% of the time to exclude
not_typing_sums$subject[not_typing_sums$Number > 6] #returning those subjects

length(which(not_typing_sums$Number > 6))
not_typing_sums$subject[not_typing_sums$Number > 6]

data_count_list <- aggregate(data = dat_test,         #counting how many s's in each list
                             subject ~ list,
                             function(subject) length(unique(subject)))
data_count_list

#return which experiment a to be exluded subject was in
dat_study$experiment[dat_study$subject=="k9mpwe1mz1ygdms"]
#return which condition a to be exluded subject was in
dat_study$condition[dat_study$subject=="k9mpwe1mz1ygdms"]
dat_study$cue_presentation[dat_study$subject=="k9mpwe1mz1ygdms"] 

dat_test = filter(dat_test,
                  subject != "0p3px0fxnkj7g9v" &   #watching videos - DF, mixed, during
                  subject != "uqjl49qkk9z8d6r" ,   #watching hockey - DF, PP, during
                  subject != "onwdbuxk8kjqkwg" ,   #watching soccer - DF, PR, during
                  subject != "pztrjvrl7b8hkuq" ,   #watching TV - DF, mixed, during
                  subject != "d08hm7vngj7cm3b" ,   #watching tv - PE, PR, after
                  subject != "cxg1w6f968t32s5" ,   #watching YouTube - DF, mixed, after
                  
                  subject != "sea5blthrok29jl" ,   #on phone - DF, mixed, after
                  subject != "12bnn779kxnalxs" ,   #on phone - PE, PR, after
                  subject != "kkaerrm54g92nuj" ,   #on phone - PE, mixed, during 
                  subject != "a4pm6j483yzxno2" ,   #on phone - DF, PR, after
                  subject != "yv7e4u72x2fseu4" ,   #on phone - DF, PR, during
                  subject != "oy0a4xrh0uwtzbh" ,   #on phone - DF, PR, after
                  subject != "37d7zjkrj9ccr4o" ,   #on phone - DF, mixed, after
                  
                  subject != "bcytnw2n9ltr2y9" ,   #listening to podcast - DF, PP, during
                  subject != "0v9g965xut6zhvw" ,   #listening to music - PE, PP, after
                  subject != "kwfq4vy2uxlnnh5" ,   #listening to music - DF, PP, after
                  subject != "ln5gy7skfmj0jlh" ,   #listening to music - DF, PR, after
                  subject != "j00dq1rqdav2zkh" ,   #playing games - PE, PP, during
                  subject != "qgqdo9sl84ofh67" ,   #cooking - DF, mixed, after
                  subject != "4wfd8x7c8grd58k" ,   #packing a bag - PE, PR, during
                  
                  subject != "9lhgaobjuvcs1zw" ,   #reading a comic - DF, PP, after
                  subject != "2f3kprjv0xkktqo" ,   #studying for exam - DF, PP, after
                  
                  subject != "2xse07c8318l2vg" ,   #in class - PE, mixed, during
                  subject != "a0yacofy8tu4eos" ,   #in class - PE, PR, after
                  subject != "aofy27d5u68f3ez" ,   #in class - PE, mixed, after
                  subject != "b1rfgz4pf2gocj3" ,   #in class - PE, mixed, during
                  subject != "bxr2z4kp7oyu4e7" ,   #in class - PE, mixed, during
                  subject != "dtf0c6mp66u06tn" ,   #in class - PE, mixed during
                  subject != "gztlc2rt3v8jbpb" ,   #in class - DF, PP, during
                  subject != "uce9py8wh3sz15z" ,   #in class - DF, mixed, during
                  subject != "x2l0phs4ypppvs2" ,   #in class - DF, mixed, after
                  subject != "zfor39eytbtp477" ,   #in class - DF, PR, during
                  
                  subject != "0z6rl42o4wfx448",    #talking - DF, PR, after
                  subject != "e239v8wy8sbl58b" ,   #talking - PE, PR, during
                  subject != "fqsyenqhgk8dax0" ,   #talking - PE, mixed, after
                  subject != "rnwade8mh0yfdrz" ,   #talking - DF, PP, after
                  subject != "k9mpwe1mz1ygdms" ,   #talking - DF, PR, during
                  subject != "4deflyzdtaw90hl" ,   #talking - PE, PR, during
                  subject != "4ghgtf6jbhc2e8r" ,   #talking - PE, PR, during
                  subject != "k9mpwe1mz1ygdms" ,   #talking - DF, PR, during
)

data_count_list <- aggregate(data = dat_test,       #counting how many s's in each list
                             subject ~ list,
                             function(subject) length(unique(subject)))
data_count_list

#counting the number of subjects in each of the 12 conditions
Mix_PE_during_ss <- length(unique(dat_test_mixed_PE_during$subject)) #aiming for at least 51 per condition
Mix_PE_after_ss <- length(unique(dat_test_mixed_PE_after$subject))
Mix_DF_during_ss <- length(unique(dat_test_mixed_DF_during$subject))
Mix_DF_after_ss <- length(unique(dat_test_mixed_DF_after$subject))

PP_PE_during_ss <- length(unique(dat_test_pureproduce_PE_during$subject))
PP_PE_after_ss <- length(unique(dat_test_pureproduce_PE_after$subject))
PP_DF_during_ss <- length(unique(dat_test_pureproduce_DF_during$subject))
PP_DF_after_ss <- length(unique(dat_test_pureproduce_DF_after$subject))

PR_PE_during_ss <- length(unique(dat_test_pureread_PE_during$subject))
PR_PE_after_ss <- length(unique(dat_test_pureread_PE_after$subject))
PR_DF_during_ss <- length(unique(dat_test_pureread_DF_during$subject))
PR_DF_after_ss <- length(unique(dat_test_pureread_DF_after$subject))

condition_num <- 1:12
condition_name <- c("Mixed_PE_during", "Mixed_PE_after", "Mixed_DF_during", "Mixed_DF_after",
                    "PP_PE_during", "PP_PE_after", "PP_DF_during", "PP_DF_after",
                    "PR_PE_during", "PR_PE_after", "PR_DF_during", "PR_DF_after")
condition_num_ss <- c(Mix_PE_during_ss, Mix_PE_after_ss, Mix_DF_during_ss, Mix_DF_after_ss,
                      PP_PE_during_ss, PP_PE_after_ss, PP_DF_during_ss, PP_DF_after_ss,
                      PR_PE_during_ss, PR_PE_after_ss, PR_DF_during_ss, PR_DF_after_ss
                      )
subject_count <- data.frame(condition_num, condition_name, condition_num_ss)

#########################################################################################################
#mean(full_data_test$response == "y")
#########################################################################################################

dat_test_mixed_PE_during <- dat_test_mixed_PE_during %>%
  mutate(TargetLureType = case_when((data_condition == 'produce') & (type2 == 'old') ~ 'produce_hit',
                                    (data_condition == 'produce') & (type2 == 'new') ~ 'produce_FA',
                                    (data_condition == 'read') & (type2 == 'old') ~ 'read_hit',
                                    (data_condition == 'read') & (type2 == 'new') ~ 'read_FA',
                                    (data_condition == 'new') & (type2 == 'new') ~ 'unrel_FA'))

dat_test_mixed_PE_after <- dat_test_mixed_PE_after %>%
  mutate(TargetLureType = case_when((data_condition == 'produce') & (type2 == 'old') ~ 'produce_hit',
                                    (data_condition == 'produce') & (type2 == 'new') ~ 'produce_FA',
                                    (data_condition == 'read') & (type2 == 'old') ~ 'read_hit',
                                    (data_condition == 'read') & (type2 == 'new') ~ 'read_FA',
                                    (data_condition == 'new') & (type2 == 'new') ~ 'unrel_FA'))

dat_test_mixed_DF_during <- dat_test_mixed_DF_during %>%
  mutate(TargetLureType = case_when((data_condition == 'produce') & (type2 == 'old') ~ 'produce_hit',
                                    (data_condition == 'produce') & (type2 == 'new') ~ 'produce_FA',
                                    (data_condition == 'read') & (type2 == 'old') ~ 'read_hit',
                                    (data_condition == 'read') & (type2 == 'new') ~ 'read_FA',
                                    (data_condition == 'new') & (type2 == 'new') ~ 'unrel_FA'))

dat_test_mixed_DF_after <- dat_test_mixed_DF_after %>%
  mutate(TargetLureType = case_when((data_condition == 'produce') & (type2 == 'old') ~ 'produce_hit',
                                    (data_condition == 'produce') & (type2 == 'new') ~ 'produce_FA',
                                    (data_condition == 'read') & (type2 == 'old') ~ 'read_hit',
                                    (data_condition == 'read') & (type2 == 'new') ~ 'read_FA',
                                    (data_condition == 'new') & (type2 == 'new') ~ 'unrel_FA'))


dat_test_pureproduce_PE_during <- dat_test_pureproduce_PE_during %>%
  mutate(TargetLureType = case_when((data_condition == 'produce') & (type2 == 'old') ~ 'produce_hit',
                                    (data_condition == 'produce') & (type2 == 'new') ~ 'produce_FA',
                                    (data_condition == 'new') & (type2 == 'new') ~ 'unrel_FA'))

dat_test_pureproduce_PE_after <- dat_test_pureproduce_PE_after %>%
  mutate(TargetLureType = case_when((data_condition == 'produce') & (type2 == 'old') ~ 'produce_hit',
                                    (data_condition == 'produce') & (type2 == 'new') ~ 'produce_FA',
                                    (data_condition == 'new') & (type2 == 'new') ~ 'unrel_FA'))

dat_test_pureproduce_DF_during <- dat_test_pureproduce_DF_during %>%
  mutate(TargetLureType = case_when((data_condition == 'produce') & (type2 == 'old') ~ 'produce_hit',
                                    (data_condition == 'produce') & (type2 == 'new') ~ 'produce_FA',
                                    (data_condition == 'new') & (type2 == 'new') ~ 'unrel_FA'))

dat_test_pureproduce_DF_after <- dat_test_pureproduce_DF_after %>%
  mutate(TargetLureType = case_when((data_condition == 'produce') & (type2 == 'old') ~ 'produce_hit',
                                    (data_condition == 'produce') & (type2 == 'new') ~ 'produce_FA',
                                    (data_condition == 'new') & (type2 == 'new') ~ 'unrel_FA'))

dat_test_pureread_PE_during <- dat_test_pureread_PE_during %>%
  mutate(TargetLureType = case_when((data_condition == 'read') & (type2 == 'old') ~ 'read_hit',
                                    (data_condition == 'read') & (type2 == 'new') ~ 'read_FA',
                                    (data_condition == 'new') & (type2 == 'new') ~ 'unrel_FA'))

dat_test_pureread_PE_after <- dat_test_pureread_PE_after %>%
  mutate(TargetLureType = case_when((data_condition == 'read') & (type2 == 'old') ~ 'read_hit',
                                    (data_condition == 'read') & (type2 == 'new') ~ 'read_FA',
                                    (data_condition == 'new') & (type2 == 'new') ~ 'unrel_FA'))

dat_test_pureread_DF_during <- dat_test_pureread_DF_during %>%
  mutate(TargetLureType = case_when((data_condition == 'read') & (type2 == 'old') ~ 'read_hit',
                                    (data_condition == 'read') & (type2 == 'new') ~ 'read_FA',
                                    (data_condition == 'new') & (type2 == 'new') ~ 'unrel_FA'))

dat_test_pureread_DF_after <- dat_test_pureread_DF_after %>%
  mutate(TargetLureType = case_when((data_condition == 'read') & (type2 == 'old') ~ 'read_hit',
                                    (data_condition == 'read') & (type2 == 'new') ~ 'read_FA',
                                    (data_condition == 'new') & (type2 == 'new') ~ 'unrel_FA'))

data_count_list <- aggregate(data = dat_test,       #counting how many s's in each list
                             subject ~ list + condition,
                             function(subject) length(unique(subject)))
data_count_list

library(plotrix) #for standard error

# result <- aggregate(response ~ TargetLureType, dat_test_mixed_PE_during, mean)
# print(result)

Step1_mixed_PE_during <- dat_test_mixed_PE_during %>% #getting hits and FAs for subjects
  group_by(subject, TargetLureType) %>%
  dplyr::summarise(
    means = mean(response, na.rm = TRUE),
    sds = sd(response, na.rm = TRUE))

Step2_mixed_PE_during <- Step1_mixed_PE_during %>% group_by(TargetLureType) %>% #getting overall hits and FAs
  dplyr::summarise(
    means = mean(means, na.rm = TRUE),
    sds = sd(sds, na.rm = TRUE))

Step1_mixed_PE_after <- dat_test_mixed_PE_after %>% #getting hits and FAs for subjects
  group_by(subject, TargetLureType) %>%
  dplyr::summarise(
    means = mean(response, na.rm = TRUE),
    sds = sd(response, na.rm = TRUE))

Step2_mixed_PE_after <- Step1_mixed_PE_after %>% group_by(TargetLureType) %>% #getting overall hits and FAs
  dplyr::summarise(
    means = mean(means, na.rm = TRUE),
    sds = sd(sds, na.rm = TRUE))

Step1_mixed_DF_during <- dat_test_mixed_DF_during %>% #getting hits and FAs for subjects
  group_by(subject, TargetLureType) %>%
  dplyr::summarise(
    means = mean(response, na.rm = TRUE),
    sds = sd(response, na.rm = TRUE))

Step2_mixed_DF_during <- Step1_mixed_DF_during %>% group_by(TargetLureType) %>% #getting overall hits and FAs
  dplyr::summarise(
    means = mean(means, na.rm = TRUE),
    sds = sd(sds, na.rm = TRUE))

Step1_mixed_DF_after <- dat_test_mixed_DF_after %>% #getting hits and FAs for subjects
  group_by(subject, TargetLureType) %>%
  dplyr::summarise(
    means = mean(response, na.rm = TRUE),
    sds = sd(response, na.rm = TRUE))

Step2_mixed_DF_after <- Step1_mixed_DF_after %>% group_by(TargetLureType) %>% #getting overall hits and FAs
  dplyr::summarise(
    means = mean(means, na.rm = TRUE),
    sds = sd(sds, na.rm = TRUE))
###########################################

Step1_pureproduce_PE_during <- dat_test_pureproduce_PE_during %>% #getting hits and FAs for subjects
  group_by(subject, TargetLureType) %>%
  dplyr::summarise(
    means = mean(response, na.rm = TRUE),
    sds = sd(response, na.rm = TRUE))

Step2_pureproduce_PE_during <- Step1_pureproduce_PE_during %>% group_by(TargetLureType) %>% #getting overall hits and FAs
  dplyr::summarise(
    means = mean(means, na.rm = TRUE),
    sds = sd(sds, na.rm = TRUE))

Step1_pureproduce_PE_after <- dat_test_pureproduce_PE_after %>% #getting hits and FAs for subjects
  group_by(subject, TargetLureType) %>%
  dplyr::summarise(
    means = mean(response, na.rm = TRUE),
    sds = sd(response, na.rm = TRUE))

Step2_pureproduce_PE_after <- Step1_pureproduce_PE_after %>% group_by(TargetLureType) %>% #getting overall hits and FAs
  dplyr::summarise(
    means = mean(means, na.rm = TRUE),
    sds = sd(sds, na.rm = TRUE))

Step1_pureproduce_DF_during <- dat_test_pureproduce_DF_during %>% #getting hits and FAs for subjects
  group_by(subject, TargetLureType) %>%
  dplyr::summarise(
    means = mean(response, na.rm = TRUE),
    sds = sd(response, na.rm = TRUE))

Step2_pureproduce_DF_during <- Step1_pureproduce_DF_during %>% group_by(TargetLureType) %>% #getting overall hits and FAs
  dplyr::summarise(
    means = mean(means, na.rm = TRUE),
    sds = sd(sds, na.rm = TRUE))

Step1_pureproduce_DF_after <- dat_test_pureproduce_DF_after %>% #getting hits and FAs for subjects
  group_by(subject, TargetLureType) %>%
  dplyr::summarise(
    means = mean(response, na.rm = TRUE),
    sds = sd(response, na.rm = TRUE))

Step2_pureproduce_DF_after <- Step1_pureproduce_DF_after %>% group_by(TargetLureType) %>% #getting overall hits and FAs
  dplyr::summarise(
    means = mean(means, na.rm = TRUE),
    sds = sd(sds, na.rm = TRUE))
###############################################

Step1_pureread_PE_during <- dat_test_pureread_PE_during %>% #getting hits and FAs for subjects
  group_by(subject, TargetLureType) %>%
  dplyr::summarise(
    means = mean(response, na.rm = TRUE),
    sds = sd(response, na.rm = TRUE))

Step2_pureread_PE_during <- Step1_pureread_PE_during %>% group_by(TargetLureType) %>% #getting overall hits and FAs
  dplyr::summarise(
    means = mean(means, na.rm = TRUE),
    sds = sd(sds, na.rm = TRUE))

Step1_pureread_PE_after <- dat_test_pureread_PE_after %>% #getting hits and FAs for subjects
  group_by(subject, TargetLureType) %>%
  dplyr::summarise(
    means = mean(response, na.rm = TRUE),
    sds = sd(response, na.rm = TRUE))

Step2_pureread_PE_after <- Step1_pureread_PE_after %>% group_by(TargetLureType) %>% #getting overall hits and FAs
  dplyr::summarise(
    means = mean(means, na.rm = TRUE),
    sds = sd(sds, na.rm = TRUE))

Step1_pureread_DF_during <- dat_test_pureread_DF_during %>% #getting hits and FAs for subjects
  group_by(subject, TargetLureType) %>%
  dplyr::summarise(
    means = mean(response, na.rm = TRUE),
    sds = sd(response, na.rm = TRUE))

Step2_pureread_DF_during <- Step1_pureread_DF_during %>% group_by(TargetLureType) %>% #getting overall hits and FAs
  dplyr::summarise(
    means = mean(means, na.rm = TRUE),
    sds = sd(sds, na.rm = TRUE))

Step1_pureread_DF_after <- dat_test_pureread_DF_after %>% #getting hits and FAs for subjects
  group_by(subject, TargetLureType) %>%
  dplyr::summarise(
    means = mean(response, na.rm = TRUE),
    sds = sd(response, na.rm = TRUE))

Step2_pureread_DF_after <- Step1_pureread_DF_after %>% group_by(TargetLureType) %>% #getting overall hits and FAs
  dplyr::summarise(
    means = mean(means, na.rm = TRUE),
    sds = sd(sds, na.rm = TRUE))

vec <- c("produce_hit", "read_hit", "produce_FA", "read_FA",
         "unrel_FA")
vec2 <- c("produce_hit", "produce_FA",
          "unrel_FA")
vec3 <- c("read_hit", "read_FA",
          "unrel_FA")

Step2_mixed_PE_during_reordered <- Step2_mixed_PE_during[match(vec, Step2_mixed_PE_during$TargetLureType), ]
Step2_mixed_PE_after_reordered <- Step2_mixed_PE_after[match(vec, Step2_mixed_PE_after$TargetLureType), ] 
Step2_mixed_DF_during_reordered <- Step2_mixed_DF_during[match(vec, Step2_mixed_DF_during$TargetLureType), ]
Step2_mixed_DF_after_reordered <- Step2_mixed_DF_after[match(vec, Step2_mixed_DF_after$TargetLureType), ]

Step2_pureproduce_PE_during_reordered <- Step2_pureproduce_PE_during[match(vec2, Step2_pureproduce_PE_during$TargetLureType), ]
Step2_pureproduce_PE_after_reordered <- Step2_pureproduce_PE_after[match(vec2, Step2_pureproduce_PE_after$TargetLureType), ]
Step2_pureproduce_DF_during_reordered <- Step2_pureproduce_DF_during[match(vec2, Step2_pureproduce_DF_during$TargetLureType), ]
Step2_pureproduce_DF_after_reordered <- Step2_pureproduce_DF_after[match(vec2, Step2_pureproduce_DF_after$TargetLureType), ]

Step2_pureread_PE_during_reordered <- Step2_pureread_PE_during[match(vec3, Step2_pureread_PE_during$TargetLureType), ]
Step2_pureread_PE_after_reordered <- Step2_pureread_PE_after[match(vec3, Step2_pureread_PE_after$TargetLureType), ]
Step2_pureread_DF_during_reordered <- Step2_pureread_DF_during[match(vec3, Step2_pureread_DF_during$TargetLureType), ]
Step2_pureread_DF_after_reordered <- Step2_pureread_DF_after[match(vec3, Step2_pureread_DF_after$TargetLureType), ]

#color_hex <- c("#0E1428","#0E1428", "#E7DFC6", "#E7DFC6", "#E7DFC6")

G1 <- ggplot(Step2_mixed_PE_during_reordered, aes(x=factor(TargetLureType, level=c('produce_hit', 'read_hit', 'produce_FA', 'read_FA', 'unrel_FA')), y = means)) +
  geom_bar(stat = "identity", color="black", fill = c("#80c841","#ED6A5A", "#839d6c", "#b69591", "#AEAEAE")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()
  )

G1 <- G1 + ggtitle("Mixed - PE - During") +
  xlab("Condition") + ylab("Proportion Old") +
  ylim(-0.04,1) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(vjust = 3)) +
  geom_vline(xintercept=2.5,linetype=3) +
  #geom_col() +
  scale_x_discrete(labels=c('Produced \n Hits', 'Read \n Hits', 'Produce \n FA', 'Read \n FA', 'Unrelated \n FA')) +
  geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2,
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(size = 8),
        axis.ticks.x = element_blank()) 

G2 <- ggplot(Step2_mixed_DF_during_reordered, aes(x=factor(TargetLureType, level=c('produce_hit', 'read_hit', 'produce_FA', 'read_FA', 'unrel_FA')), y = means)) +
  geom_bar(stat = "identity", color="black", fill = c("#80c841","#ED6A5A", "#839d6c", "#b69591", "#AEAEAE")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()
  )

G2 <- G2 + ggtitle("Mixed - DF - During") +
  xlab("Condition") + ylab("Proportion Old") +
  ylim(-0.04,1) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(vjust = 3)) +
  geom_vline(xintercept=2.5,linetype=3) +
  #geom_col() +
  scale_x_discrete(labels=c('Remember \n Hits', 'Forget \n Hits', 'Remember \n FA', 'Forget \n FA', 'Unrelated \n FA')) +
  geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2,
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(size = 8),
        axis.ticks.x = element_blank())

G3 <- ggplot(Step2_mixed_PE_after_reordered, aes(x=factor(TargetLureType, level=c('produce_hit', 'read_hit', 'produce_FA', 'read_FA', 'unrel_FA')), y = means)) +
  geom_bar(stat = "identity", color="black", fill = c("#80c841","#ED6A5A", "#839d6c", "#b69591", "#AEAEAE")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()
  )

G3 <- G3 + ggtitle("Mixed - PE - After") +
  xlab("Condition") + ylab("Proportion Old") +
  ylim(-0.04,1) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(vjust = 3)) +
  geom_vline(xintercept=2.5,linetype=3) +
  #geom_col() +
  scale_x_discrete(labels=c('Produced \n Hits', 'Read \n Hits', 'Produce \n FA', 'Read \n FA', 'Unrelated \n FA')) +
  geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2,
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(size = 8),
        axis.ticks.x = element_blank())

G4 <- ggplot(Step2_mixed_DF_after_reordered, aes(x=factor(TargetLureType, level=c('produce_hit', 'read_hit', 'produce_FA', 'read_FA', 'unrel_FA')), y = means)) +
  geom_bar(stat = "identity", color="black", fill = c("#80c841","#ED6A5A", "#839d6c", "#b69591", "#AEAEAE")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()
  )

G4 <- G4 + ggtitle("Mixed - DF - After") +
  xlab("Condition") + ylab("Proportion Old") +
  ylim(-0.04,1) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(axis.text.x = element_text(vjust = 3)) +
  geom_vline(xintercept=2.5,linetype=3) +
  #geom_col() +
  scale_x_discrete(labels=c('Remember \n Hits', 'Forget \n Hits', 'Remember \n FA', 'Forget \n FA', 'Unrelated \n FA')) +
  geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2,
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(size = 8),
        axis.ticks.x = element_blank())
###################################

G5 <- ggplot(Step2_pureproduce_PE_during_reordered, aes(x=factor(TargetLureType, level=c('produce_hit', 'read_hit', 'produce_FA', 'read_FA', 'unrel_FA')), y = means)) +
  geom_bar(stat = "identity", color="black", fill = c("#80c841", "#839d6c", "#AEAEAE")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

G5 <- G5 + ggtitle("Pure Produce - PE - During") +
  xlab("Condition") + ylab("Proportion Old") +
  ylim(-0.01,1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(vjust = 3)) +
  geom_vline(xintercept=1.5,linetype=3) +
  #geom_col() +
  scale_x_discrete(labels=c('\n Produce \n Hit', '\n Produce \n FA', '\n Unrelated \n FA')) +
  geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2,
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(size = 8),
        axis.ticks.x = element_blank()) 

G6 <- ggplot(Step2_pureproduce_DF_during_reordered, aes(x=factor(TargetLureType, level=c('produce_hit', 'read_hit', 'produce_FA', 'read_FA', 'unrel_FA')), y = means)) +
  geom_bar(stat = "identity", color="black", fill = c("#80c841", "#839d6c", "#AEAEAE")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

G6 <- G6 + ggtitle("Pure Remember - DF - During") +
  xlab("Condition") + ylab("Proportion Old") +
  ylim(-0.01,1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(vjust = 3)) +
  geom_vline(xintercept=1.5,linetype=3) +
  #geom_col() +
  scale_x_discrete(labels=c('\n Remember \n Hit', '\n Remember \n FA', '\n Unrelated \n FA')) +
  geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2,
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(size = 8),
        axis.ticks.x = element_blank()) 

G7 <- ggplot(Step2_pureproduce_PE_after_reordered, aes(x=factor(TargetLureType, level=c('produce_hit', 'read_hit', 'produce_FA', 'read_FA', 'unrel_FA')), y = means)) +
  geom_bar(stat = "identity", color="black", fill = c("#80c841", "#839d6c", "#AEAEAE")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

G7 <- G7 + ggtitle("Pure Produce - PE - After") +
  xlab("Condition") + ylab("Proportion Old") +
  ylim(-0.01,1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(vjust = 3)) +
  geom_vline(xintercept=1.5,linetype=3) +
  #geom_col() +
  scale_x_discrete(labels=c('\n Produce \n Hit', '\n Produce \n FA', '\n Unrelated \n FA')) +
  geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2,
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(size = 8),
        axis.ticks.x = element_blank()) 

G8 <- ggplot(Step2_pureproduce_DF_after_reordered, aes(x=factor(TargetLureType, level=c('produce_hit', 'read_hit', 'produce_FA', 'read_FA', 'unrel_FA')), y = means)) +
  geom_bar(stat = "identity", color="black", fill = c("#80c841", "#839d6c", "#AEAEAE")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

G8 <- G8 + ggtitle("Pure Remember - DF - After") +
  xlab("Condition") + ylab("Proportion Old") +
  ylim(-0.01,1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(vjust = 3)) +
  geom_vline(xintercept=1.5,linetype=3) +
  #geom_col() +
  scale_x_discrete(labels=c('\n Remember \n Hit', '\n Remember \n FA', '\n Unrelated \n FA')) +
  geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2,
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(size = 8),
        axis.ticks.x = element_blank())
##################################

G9 <- ggplot(Step2_pureread_PE_during_reordered, aes(x = reorder(TargetLureType, -means), y = means)) +
  geom_bar(stat = "identity", color="black", fill = c("#ED6A5A", "#b69591", "#AEAEAE")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()
  )

G9 <- G9 + ggtitle("Pure Read - PE - During") +
  xlab("Condition") + ylab("Proportion Old") +
  ylim(-0.01,1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(vjust = 3)) +
  #geom_col() +
  geom_vline(xintercept= 1.5, linetype=3) +
  scale_x_discrete(labels=c('\n Read \n Hits', '\n Read \n FA', '\n Unrelated \n FA')) +
  geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2,
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(size = 8),
        axis.ticks.x = element_blank())

G10 <- ggplot(Step2_pureread_DF_during_reordered, aes(x = reorder(TargetLureType, -means), y = means)) +
  geom_bar(stat = "identity", color="black", fill = c("#ED6A5A", "#b69591", "#AEAEAE")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()
  )

G10 <- G10 + ggtitle("Pure Forget - DF - During") +
  xlab("Condition") + ylab("Proportion Old") +
  ylim(-0.01,1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(vjust = 3)) +
  #geom_col() +
  geom_vline(xintercept= 1.5, linetype=3) +
  scale_x_discrete(labels=c('\n Forget \n Hits', '\n Forget \n FA', '\n Unrelated \n FA')) +
  geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2,
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(size = 8),
        axis.ticks.x = element_blank())

G11 <- ggplot(Step2_pureread_PE_after_reordered, aes(x = reorder(TargetLureType, -means), y = means)) +
  geom_bar(stat = "identity", color="black", fill = c("#ED6A5A", "#b69591", "#AEAEAE")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()
  )

G11 <- G11 + ggtitle("Pure Read - PE - After") +
  xlab("Condition") + ylab("Proportion Old") +
  ylim(-0.01,1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(vjust = 3)) +
  #geom_col() +
  geom_vline(xintercept= 1.5, linetype=3) +
  scale_x_discrete(labels=c('\n Read \n Hits', '\n Read \n FA', '\n Unrelated \n FA')) +
  geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2,
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(size = 8),
        axis.ticks.x = element_blank())

G12 <- ggplot(Step2_pureread_DF_after_reordered, aes(x = reorder(TargetLureType, -means), y = means)) +
  geom_bar(stat = "identity", color="black", fill = c("#ED6A5A", "#b69591", "#AEAEAE")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()
  )

G12 <- G12 + ggtitle("Pure Forget - DF - After") +
  xlab("Condition") + ylab("Proportion Old") +
  ylim(-0.01,1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(vjust = 3)) +
  #geom_col() +
  geom_vline(xintercept= 1.5, linetype=3) +
  scale_x_discrete(labels=c('\n Forget \n Hits', '\n Forget \n FA', '\n Unrelated \n FA')) +
  geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2,
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(size = 8),
        axis.ticks.x = element_blank())





library("gridExtra")
library("grid")
grid.arrange(G1, G2, G3, G4,
             G5, G6, G7, G8,
             G9, G10, G11, G12, nrow = 3, ncol = 4)

print(subject_count)


# library("ggsignif")
# G3 <- G3 + geom_signif(y_position=c(0.9), 
#                        xmin=c("produce", 1.8), 
#                        xmax=c("read", 2.2),annotation=c("*", "NS"), 
#                        tip_length=0.025)

dat_test$condition <- as.character(dat_test$condition)
dat_test$type2 <- as.character(dat_test$type2)

ANOVA1 <- aov(response ~ data_condition * TargetLureType, data = dat_test_mixed_PE_during)
ANOVA1_summary <- summary(ANOVA1)
anova_table <- ANOVA1_summary[[1]]
library(knitr)
library(kableExtra)
anova_table$`Pr(>F)` <- format(round(anova_table$`Pr(>F)`, digits = 4), nsmall = 4)

mixed_PE_during_pretty_aov <- kable(anova_table, "html", digits = 2,caption = "Mixed-list production effect during presentation") %>%
  kable_styling(full_width = FALSE) %>%
  column_spec(1, bold = TRUE)
#run the contrasts

ANOVA2 <- aov(response ~ data_condition * TargetLureType, data = dat_test_mixed_PE_after)
summary(ANOVA2)
ANOVA3 <- aov(response ~ data_condition * TargetLureType, data = dat_test_mixed_DF_during)
summary(ANOVA3)
ANOVA4 <- aov(response ~ data_condition * TargetLureType, data = dat_test_mixed_DF_after)
summary(ANOVA4)

ANOVA5 <- aov(response ~ data_condition * TargetLureType, data = dat_test_pureproduce_PE_during)
summary(ANOVA5)
ANOVA6 <- aov(response ~ data_condition * TargetLureType, data = dat_test_pureproduce_PE_after)
summary(ANOVA6)
ANOVA7 <- aov(response ~ data_condition * TargetLureType, data = dat_test_pureproduce_DF_during)
summary(ANOVA7)
ANOVA8 <- aov(response ~ data_condition * TargetLureType, data = dat_test_pureproduce_DF_after)
summary(ANOVA8)

ANOVA9 <- aov(response ~ data_condition * TargetLureType, data = dat_test_pureread_PE_during)
summary(ANOVA9)
ANOVA10 <- aov(response ~ data_condition * TargetLureType, data = dat_test_pureread_PE_after)
summary(ANOVA10)
ANOVA11 <- aov(response ~ data_condition * TargetLureType, data = dat_test_pureread_DF_during)
summary(ANOVA11)
ANOVA12 <- aov(response ~ data_condition * TargetLureType, data = dat_test_pureread_DF_after)
summary(ANOVA12)

#################################################################################################
#contrasts for PE during
dat_test_mixed_PE_during2 <- (dat_test_mixed_PE_during[,-c(1:8,10:13,15:28)]) 
table(dat_test_mixed_PE_during2$TargetLureType)

library(reshape2)
data_wide <- dcast(dat_test_mixed_PE_during2, subject ~ TargetLureType, value.var="response",fun.aggregate=mean)

mean(data_wide$produce_FA)
mean(data_wide$read_FA)
mean(data_wide$unrel_FA)

data_wide <- as.matrix(data_wide[,-1])   #delete subject column for contrasts
#reoder columns for contrasts

col_order <- c("produce_hit", "read_hit", "produce_FA", "read_FA", "unrel_FA")
data_wide <- data_wide[, col_order]
#conduct contrasts
t_test_old_new1 <- t.test(as.matrix(data_wide) %*% c(1.5,1.5,-1,-1,-1)) #old vs. new
t_test_summary1 <- data.frame(
  condition = c("Mixed-list PE during"),
  comparison = c("Old vs. new"),
  t_statistic = t_test_old_new1[[1]],
  df = t_test_old_new1[[2]],
  p_value = t_test_old_new1[[3]],
  std_err = t_test_old_new1[[7]])

t_test_prod_read_hits <- t.test(as.matrix(data_wide) %*% c(1, -1,0,0,0)) #produce vs. read hits
t_test_summary2 <- data.frame(
  condition = c("Mixed-list PE during"),
  comparison = c("Prod vs. read hits"),
  t_statistic = t_test_prod_read_hits[[1]],
  df = t_test_prod_read_hits[[2]],
  p_value = t_test_prod_read_hits[[3]],
  std_err = t_test_prod_read_hits[[7]])

t_test_rel_unrel <- t.test(as.matrix(data_wide) %*% c(0,0,1,1, -2)) #related lures vs. unrelated lures
t_test_summary3 <- data.frame(
  condition = c("Mixed-list PE during"),
  comparison = c("Related vs. unrel FAs"),
  t_statistic = t_test_rel_unrel[[1]],
  df = t_test_rel_unrel[[2]],
  p_value = t_test_rel_unrel[[3]],
  std_err = t_test_rel_unrel[[7]])

t_test_prodFA_readFA <- t.test(as.matrix(data_wide) %*% c(0,0,1,-1, 0)) #produce FA vs. read FA
t_test_summary4 <- data.frame(
  condition = c("Mixed-list PE during"),
  comparison = c("Prod vs. read FAs"),
  t_statistic = t_test_prodFA_readFA[[1]],
  df = t_test_prodFA_readFA[[2]],
  p_value = t_test_prodFA_readFA[[3]],
  std_err = t_test_prodFA_readFA[[7]])

####################################
#between subjects t-test for pure conditions
dat_test_pureproduce2 <- (dat_test_pureproduce_PE_during[,-c(1,3:8,10:13,15:22,24:28)])
dat_test_pureread2 <- (dat_test_pureread_PE_during[,-c(1,3:8,10:13,15:22,24:28)])
dat_test_pure_all <- rbind(dat_test_pureproduce2, dat_test_pureread2)
dat_test_pure_all$condition <- as.character(dat_test_pure_all$condition)

#aggregate data to test the between subjects PE
Between_ProdEff_AOV <- dat_test_pure_all %>% 
  group_by(subject, condition) %>%
  dplyr::summarise(
    hits = mean(response, na.rm = FALSE))

ANOVA_pure2 <- aov(hits ~ condition, data = Between_ProdEff_AOV)
summary(ANOVA_pure2)

#run the pure-list production effect t-test
t_test_pure_PE_during <- t.test(hits ~ condition, data = Between_ProdEff_AOV, paired = FALSE)
t_test_summary5 <- data.frame(
  condition = c("Pure-list PE during"),
  comparison = c("Prod vs. read hits"),
  t_statistic = t_test_pure_PE_during[[1]],
  df = t_test_pure_PE_during[[2]],
  p_value = t_test_pure_PE_during[[3]],
  std_err = t_test_pure_PE_during[[7]])

#run further pure list contrasts
#pure produce
data_wide_pure_produce <- dcast(dat_test_pureproduce2, subject ~ TargetLureType, value.var="response",fun.aggregate=mean)
data_wide_pure_produce <- (data_wide_pure_produce[,-c(1)])

t_test_pureproduce_PE_during <- t.test(as.matrix(data_wide_pure_produce) %*% c(1,0,-1)) #realted lures vs. unrelated lures
t_test_summary6 <- data.frame(
  condition = c("Pure-produce PE during"),
  comparison = c("Prod vs. unrel FAs"),
  t_statistic = t_test_pureproduce_PE_during[[1]],
  df = t_test_pureproduce_PE_during[[2]],
  p_value = t_test_pureproduce_PE_during[[3]],
  std_err = t_test_pureproduce_PE_during[[7]])

t_test_pureproduce_PE_during2 <- t.test(as.matrix(data_wide_pure_produce) %*% c(1,-0.5,-0.5)) #old vs. new
t_test_summary7 <- data.frame(
  condition = c("Pure-produce PE during"),
  comparison = c("Old vs. new"),
  t_statistic = t_test_pureproduce_PE_during2[[1]],
  df = t_test_pureproduce_PE_during2[[2]],
  p_value = t_test_pureproduce_PE_during2[[3]],
  std_err = t_test_pureproduce_PE_during2[[7]])

#pure read contrasts
data_wide_pure_read <- dcast(dat_test_pureread2, subject ~ TargetLureType, value.var="response",fun.aggregate=mean)
data_wide_pure_read <- (data_wide_pure_read[,-c(1)])

t_test_pureread_PE_during <- t.test(as.matrix(data_wide_pure_read) %*% c(1,0,-1)) #realted lures vs. unrelated lures
t_test_summary8 <- data.frame(
  condition = c("Pure-read PE during"),
  comparison = c("Prod vs. unrel FAs"),
  t_statistic = t_test_pureread_PE_during[[1]],
  df = t_test_pureread_PE_during[[2]],
  p_value = t_test_pureread_PE_during[[3]],
  std_err = t_test_pureread_PE_during[[7]])

t_test_pureread_PE_during2 <- t.test(as.matrix(data_wide_pure_read) %*% c(1,-0.5,-0.5)) #old vs. new
t_test_summary9 <- data.frame(
  condition = c("Pure-read PE during"),
  comparison = c("Old vs. new"),
  t_statistic = t_test_pureread_PE_during2[[1]],
  df = t_test_pureread_PE_during2[[2]],
  p_value = t_test_pureread_PE_during2[[3]],
  std_err = t_test_pureread_PE_during2[[7]])

contrasts_pe_during <- rbind(t_test_summary1, t_test_summary2, t_test_summary3, t_test_summary4, 
                                   t_test_summary5, t_test_summary6, t_test_summary7, t_test_summary8, 
                                   t_test_summary9)

contrasts_pe_during[, c(3,6)] <- round(contrasts_pe_during[, c(3,6)], digits = 2)
contrasts_pe_during$p_value <- sprintf("%.4f", contrasts_pe_during$p_value)

# Display the data frame  -move this to bottom eventually
contrasts_pe_during

################################################################################################

#aggregate subject score to be two each first, pureRead_hit, pureProduce_hit, pureRead_FA, pureProduce_FA
dat_test_pure_all$data_condition <- as.character(dat_test_pure_all$data_condition)
# 
dat_test_pure_all$data_condition[dat_test_pure_all$data_condition == 'produce'] <- 'hit'
dat_test_pure_all$data_condition[dat_test_pure_all$data_condition == 'read'] <- 'hit'
# 
dat_test_pure_all$data_condition <- as.factor(dat_test_pure_all$data_condition)
# 
# to the the interaction in the pure list conditoin hits vs. FAs. differences of the differnnces
ANOVA_pure <- aov(response ~ condition * data_condition, data = dat_test_pure_all) 
summary(ANOVA_pure)

##########################################################

#################################################################################################
#contrasts for PE after condition
dat_test_mixed_PE_after2 <- (dat_test_mixed_PE_after[,-c(1:8,10:13,15:28)]) 
table(dat_test_mixed_PE_after2$TargetLureType)

data_wide <- dcast(dat_test_mixed_PE_after2, subject ~ TargetLureType, value.var="response",fun.aggregate=mean)

mean(data_wide$produce_FA)
mean(data_wide$read_FA)
mean(data_wide$unrel_FA)

data_wide <- as.matrix(data_wide[,-1])   #delete subject column for contrasts
#reoder columns for contrasts

col_order <- c("produce_hit", "read_hit", "produce_FA", "read_FA", "unrel_FA")
data_wide <- data_wide[, col_order]
#conduct contrasts
t_test_old_new1 <- t.test(as.matrix(data_wide) %*% c(1.5,1.5,-1,-1,-1)) #old vs. new
t_test_summary1 <- data.frame(
  condition = c("Mixed-list PE during"),
  comparison = c("Old vs. new"),
  t_statistic = t_test_old_new1[[1]],
  df = t_test_old_new1[[2]],
  p_value = t_test_old_new1[[3]],
  std_err = t_test_old_new1[[7]])

t_test_prod_read_hits <- t.test(as.matrix(data_wide) %*% c(1, -1,0,0,0)) #produce vs. read hits
t_test_summary2 <- data.frame(
  condition = c("Mixed-list PE during"),
  comparison = c("Prod vs. read hits"),
  t_statistic = t_test_prod_read_hits[[1]],
  df = t_test_prod_read_hits[[2]],
  p_value = t_test_prod_read_hits[[3]],
  std_err = t_test_prod_read_hits[[7]])

t_test_rel_unrel <- t.test(as.matrix(data_wide) %*% c(0,0,1,1, -2)) #related lures vs. unrelated lures
t_test_summary3 <- data.frame(
  condition = c("Mixed-list PE during"),
  comparison = c("Related vs. unrel FAs"),
  t_statistic = t_test_rel_unrel[[1]],
  df = t_test_rel_unrel[[2]],
  p_value = t_test_rel_unrel[[3]],
  std_err = t_test_rel_unrel[[7]])

t_test_prodFA_readFA <- t.test(as.matrix(data_wide) %*% c(0,0,1,-1, 0)) #produce FA vs. read FA
t_test_summary4 <- data.frame(
  condition = c("Mixed-list PE during"),
  comparison = c("Prod vs. read FAs"),
  t_statistic = t_test_prodFA_readFA[[1]],
  df = t_test_prodFA_readFA[[2]],
  p_value = t_test_prodFA_readFA[[3]],
  std_err = t_test_prodFA_readFA[[7]])

####################################
#between subjects t-test for pure conditions
dat_test_pureproduce2 <- (dat_test_pureproduce_PE_after[,-c(1,3:8,10:13,15:28)])
dat_test_pureread2 <- (dat_test_pureread_PE_after[,-c(1,3:8,10:13,15:28)])
dat_test_pure_all <- rbind(dat_test_pureproduce2, dat_test_pureread2)
dat_test_pure_all$type <- as.character(dat_test_pure_all$condition)

#aggregate data to test the between subjects PE
Between_ProdEff_AOV <- dat_test_pure_all %>% 
  group_by(subject, condition) %>%
  dplyr::summarise(
    hits = mean(response, na.rm = FALSE))

ANOVA_pure2 <- aov(hits ~ condition, data = Between_ProdEff_AOV)
summary(ANOVA_pure2)

#run the pure-list production effect t-test
t_test_pure_PE_during <- t.test(hits ~ condition, data = Between_ProdEff_AOV, paired = FALSE)
t_test_summary5 <- data.frame(
  condition = c("Pure-list PE during"),
  comparison = c("Prod vs. read hits"),
  t_statistic = t_test_pure_PE_during[[1]],
  df = t_test_pure_PE_during[[2]],
  p_value = t_test_pure_PE_during[[3]],
  std_err = t_test_pure_PE_during[[7]])

#run further pure list contrasts
#pure produce
data_wide_pure_produce <- dcast(dat_test_pureproduce2, subject ~ TargetLureType, value.var="response",fun.aggregate=mean)
data_wide_pure_produce <- (data_wide_pure_produce[,-c(1)])

t_test_pureproduce_PE_during <- t.test(as.matrix(data_wide_pure_produce) %*% c(1,0,-1)) #realted lures vs. unrelated lures
t_test_summary6 <- data.frame(
  condition = c("Pure-produce PE during"),
  comparison = c("Prod vs. unrel FAs"),
  t_statistic = t_test_pureproduce_PE_during[[1]],
  df = t_test_pureproduce_PE_during[[2]],
  p_value = t_test_pureproduce_PE_during[[3]],
  std_err = t_test_pureproduce_PE_during[[7]])

t_test_pureproduce_PE_during2 <- t.test(as.matrix(data_wide_pure_produce) %*% c(1,-0.5,-0.5)) #old vs. new
t_test_summary7 <- data.frame(
  condition = c("Pure-produce PE during"),
  comparison = c("Old vs. new"),
  t_statistic = t_test_pureproduce_PE_during2[[1]],
  df = t_test_pureproduce_PE_during2[[2]],
  p_value = t_test_pureproduce_PE_during2[[3]],
  std_err = t_test_pureproduce_PE_during2[[7]])

#pure read contrasts
data_wide_pure_read <- dcast(dat_test_pureread2, subject ~ TargetLureType, value.var="response",fun.aggregate=mean)
data_wide_pure_read <- (data_wide_pure_read[,-c(1)])

t_test_pureread_PE_during <- t.test(as.matrix(data_wide_pure_read) %*% c(1,0,-1)) #realted lures vs. unrelated lures
t_test_summary8 <- data.frame(
  condition = c("Pure-read PE during"),
  comparison = c("Prod vs. unrel FAs"),
  t_statistic = t_test_pureread_PE_during[[1]],
  df = t_test_pureread_PE_during[[2]],
  p_value = t_test_pureread_PE_during[[3]],
  std_err = t_test_pureread_PE_during[[7]])

t_test_pureread_PE_during2 <- t.test(as.matrix(data_wide_pure_read) %*% c(1,-0.5,-0.5)) #old vs. new
t_test_summary9 <- data.frame(
  condition = c("Pure-read PE during"),
  comparison = c("Old vs. new"),
  t_statistic = t_test_pureread_PE_during2[[1]],
  df = t_test_pureread_PE_during2[[2]],
  p_value = t_test_pureread_PE_during2[[3]],
  std_err = t_test_pureread_PE_during2[[7]])

contrasts_pe_after <- rbind(t_test_summary1, t_test_summary2, t_test_summary3, t_test_summary4, 
                             t_test_summary5, t_test_summary6, t_test_summary7, t_test_summary8, 
                             t_test_summary9)

contrasts_pe_after[, c(3,6)] <- round(contrasts_pe_after[, c(3,6)], digits = 2)
contrasts_pe_after$p_value <- sprintf("%.4f", contrasts_pe_after$p_value)

# Display the data frame  -move this to bottom eventually
contrasts_pe_after

################################################################################################

#aggregate subject score to be two each first, pureRead_hit, pureProduce_hit, pureRead_FA, pureProduce_FA
# dat_test_pure_all$condition <- as.character(dat_test_pure_all$data_condition)
# 
# dat_test_pure_all$condition[dat_test_pure_all$data_condition == 'produce'] <- 'hit'
# dat_test_pure_all$condition[dat_test_pure_all$data_condition == 'read'] <- 'hit'
# 
# dat_test_pure_all$data_condition <- as.factor(dat_test_pure_all$data_condition)
# 
# # to the the interaction in the pure list conditoin hits vs. FAs. differences of the differnnces
# ANOVA_pure <- aov(response ~ condition * data_condition, data = dat_test_pure_all) 
# summary(ANOVA_pure)

##########################################################

#################################################################################################
#contrasts for DF during
dat_test_mixed_DF_during2 <- (dat_test_mixed_DF_during[,-c(1:8,10:13,15:28)]) 
table(dat_test_mixed_DF_during2$TargetLureType)

data_wide <- dcast(dat_test_mixed_DF_during2, subject ~ TargetLureType, value.var="response",fun.aggregate=mean)

mean(data_wide$produce_FA)
mean(data_wide$read_FA)
mean(data_wide$unrel_FA)

data_wide <- as.matrix(data_wide[,-1])   #delete subject column for contrasts
#reoder columns for contrasts

col_order <- c("produce_hit", "read_hit", "produce_FA", "read_FA", "unrel_FA")
data_wide <- data_wide[, col_order]
#conduct contrasts
t_test_old_new1 <- t.test(as.matrix(data_wide) %*% c(1.5,1.5,-1,-1,-1)) #old vs. new
t_test_summary1 <- data.frame(
  condition = c("Mixed-list DF during"),
  comparison = c("Old vs. new"),
  t_statistic = t_test_old_new1[[1]],
  df = t_test_old_new1[[2]],
  p_value = t_test_old_new1[[3]],
  std_err = t_test_old_new1[[7]])

t_test_prod_read_hits <- t.test(as.matrix(data_wide) %*% c(1, -1,0,0,0)) #produce vs. read hits
t_test_summary2 <- data.frame(
  condition = c("Mixed-list DF during"),
  comparison = c("Remember vs. forget hits"),
  t_statistic = t_test_prod_read_hits[[1]],
  df = t_test_prod_read_hits[[2]],
  p_value = t_test_prod_read_hits[[3]],
  std_err = t_test_prod_read_hits[[7]])

t_test_rel_unrel <- t.test(as.matrix(data_wide) %*% c(0,0,1,1, -2)) #related lures vs. unrelated lures
t_test_summary3 <- data.frame(
  condition = c("Mixed-list DF during"),
  comparison = c("Related vs. unrel FAs"),
  t_statistic = t_test_rel_unrel[[1]],
  df = t_test_rel_unrel[[2]],
  p_value = t_test_rel_unrel[[3]],
  std_err = t_test_rel_unrel[[7]])

t_test_prodFA_readFA <- t.test(as.matrix(data_wide) %*% c(0,0,1,-1, 0)) #produce FA vs. read FA
t_test_summary4 <- data.frame(
  condition = c("Mixed-list DF during"),
  comparison = c("Remember vs. forget FAs"),
  t_statistic = t_test_prodFA_readFA[[1]],
  df = t_test_prodFA_readFA[[2]],
  p_value = t_test_prodFA_readFA[[3]],
  std_err = t_test_prodFA_readFA[[7]])

####################################
#between subjects t-test for pure conditions
dat_test_pureproduce2 <- (dat_test_pureproduce_DF_during[,-c(1,3:8,10:13,15:28)])
dat_test_pureread2 <- (dat_test_pureread_DF_during[,-c(1,3:8,10:13,15:28)])
dat_test_pure_all <- rbind(dat_test_pureproduce2, dat_test_pureread2)
dat_test_pure_all$type <- as.character(dat_test_pure_all$condition)

#aggregate data to test the between subjects PE
Between_ProdEff_AOV <- dat_test_pure_all %>% 
  group_by(subject, condition) %>%
  dplyr::summarise(
    hits = mean(response, na.rm = FALSE))

ANOVA_pure2 <- aov(hits ~ condition, data = Between_ProdEff_AOV)
summary(ANOVA_pure2)

#run the pure-list production effect t-test
t_test_pure_PE_during <- t.test(hits ~ condition, data = Between_ProdEff_AOV, paired = FALSE)
t_test_summary5 <- data.frame(
  condition = c("Pure-list DF during"),
  comparison = c("Remember vs. forget hits"),
  t_statistic = t_test_pure_PE_during[[1]],
  df = t_test_pure_PE_during[[2]],
  p_value = t_test_pure_PE_during[[3]],
  std_err = t_test_pure_PE_during[[7]])

#run further pure list contrasts
#pure produce
data_wide_pure_produce <- dcast(dat_test_pureproduce2, subject ~ TargetLureType, value.var="response",fun.aggregate=mean)
data_wide_pure_produce <- (data_wide_pure_produce[,-c(1)])

t_test_pureproduce_PE_during <- t.test(as.matrix(data_wide_pure_produce) %*% c(1,0,-1)) #realted lures vs. unrelated lures
t_test_summary6 <- data.frame(
  condition = c("Pure-remember DF during"),
  comparison = c("Related vs. unrel FAs"),
  t_statistic = t_test_pureproduce_PE_during[[1]],
  df = t_test_pureproduce_PE_during[[2]],
  p_value = t_test_pureproduce_PE_during[[3]],
  std_err = t_test_pureproduce_PE_during[[7]])

t_test_pureproduce_PE_during2 <- t.test(as.matrix(data_wide_pure_produce) %*% c(1,-0.5,-0.5)) #old vs. new
t_test_summary7 <- data.frame(
  condition = c("Pure-remember DF during"),
  comparison = c("Old vs. new"),
  t_statistic = t_test_pureproduce_PE_during2[[1]],
  df = t_test_pureproduce_PE_during2[[2]],
  p_value = t_test_pureproduce_PE_during2[[3]],
  std_err = t_test_pureproduce_PE_during2[[7]])

#pure read contrasts
data_wide_pure_read <- dcast(dat_test_pureread2, subject ~ TargetLureType, value.var="response",fun.aggregate=mean)
data_wide_pure_read <- (data_wide_pure_read[,-c(1)])

t_test_pureread_PE_during <- t.test(as.matrix(data_wide_pure_read) %*% c(1,0,-1)) #realted lures vs. unrelated lures
t_test_summary8 <- data.frame(
  condition = c("Pure-forget DF during"),
  comparison = c("Prod vs. unrel FAs"),
  t_statistic = t_test_pureread_PE_during[[1]],
  df = t_test_pureread_PE_during[[2]],
  p_value = t_test_pureread_PE_during[[3]],
  std_err = t_test_pureread_PE_during[[7]])

t_test_pureread_PE_during2 <- t.test(as.matrix(data_wide_pure_read) %*% c(1,-0.5,-0.5)) #old vs. new
t_test_summary9 <- data.frame(
  condition = c("Pure-forget DF during"),
  comparison = c("Old vs. new"),
  t_statistic = t_test_pureread_PE_during2[[1]],
  df = t_test_pureread_PE_during2[[2]],
  p_value = t_test_pureread_PE_during2[[3]],
  std_err = t_test_pureread_PE_during2[[7]])

contrasts_df_during <- rbind(t_test_summary1, t_test_summary2, t_test_summary3, t_test_summary4, 
                             t_test_summary5, t_test_summary6, t_test_summary7, t_test_summary8, 
                             t_test_summary9)

contrasts_df_during[, c(3,6)] <- round(contrasts_df_during[, c(3,6)], digits = 2)
contrasts_df_during$p_value <- sprintf("%.4f", contrasts_df_during$p_value)

# Display the data frame  -move this to bottom eventually
contrasts_df_during

################################################################################################

#aggregate subject score to be two each first, pureRead_hit, pureProduce_hit, pureRead_FA, pureProduce_FA
# dat_test_pure_all$condition <- as.character(dat_test_pure_all$data_condition)
# 
# dat_test_pure_all$condition[dat_test_pure_all$data_condition == 'produce'] <- 'hit'
# dat_test_pure_all$condition[dat_test_pure_all$data_condition == 'read'] <- 'hit'
# 
# dat_test_pure_all$data_condition <- as.factor(dat_test_pure_all$data_condition)
# 
# # to the the interaction in the pure list conditoin hits vs. FAs. differences of the differnnces
# ANOVA_pure <- aov(response ~ condition * data_condition, data = dat_test_pure_all) 
# summary(ANOVA_pure)

##########################################################

#################################################################################################
#contrasts for DF after
dat_test_mixed_DF_after2 <- (dat_test_mixed_DF_after[,-c(1:8,10:13,15:28)]) 
table(dat_test_mixed_DF_after2$TargetLureType)

data_wide <- dcast(dat_test_mixed_DF_after2, subject ~ TargetLureType, value.var="response",fun.aggregate=mean)

mean(data_wide$produce_FA)
mean(data_wide$read_FA)
mean(data_wide$unrel_FA)

data_wide <- as.matrix(data_wide[,-1])   #delete subject column for contrasts
#reoder columns for contrasts

col_order <- c("produce_hit", "read_hit", "produce_FA", "read_FA", "unrel_FA")
data_wide <- data_wide[, col_order]
#conduct contrasts
t_test_old_new1 <- t.test(as.matrix(data_wide) %*% c(1.5,1.5,-1,-1,-1)) #old vs. new
t_test_summary1 <- data.frame(
  condition = c("Mixed-list DF after"),
  comparison = c("Old vs. new"),
  t_statistic = t_test_old_new1[[1]],
  df = t_test_old_new1[[2]],
  p_value = t_test_old_new1[[3]],
  std_err = t_test_old_new1[[7]])

t_test_prod_read_hits <- t.test(as.matrix(data_wide) %*% c(1, -1,0,0,0)) #produce vs. read hits
t_test_summary2 <- data.frame(
  condition = c("Mixed-list DF after"),
  comparison = c("Remember vs. forget hits"),
  t_statistic = t_test_prod_read_hits[[1]],
  df = t_test_prod_read_hits[[2]],
  p_value = t_test_prod_read_hits[[3]],
  std_err = t_test_prod_read_hits[[7]])

t_test_rel_unrel <- t.test(as.matrix(data_wide) %*% c(0,0,1,1, -2)) #related lures vs. unrelated lures
t_test_summary3 <- data.frame(
  condition = c("Mixed-list DF after"),
  comparison = c("Related vs. unrel FAs"),
  t_statistic = t_test_rel_unrel[[1]],
  df = t_test_rel_unrel[[2]],
  p_value = t_test_rel_unrel[[3]],
  std_err = t_test_rel_unrel[[7]])

t_test_prodFA_readFA <- t.test(as.matrix(data_wide) %*% c(0,0,1,-1, 0)) #produce FA vs. read FA
t_test_summary4 <- data.frame(
  condition = c("Mixed-list DF after"),
  comparison = c("Remember vs. forget FAs"),
  t_statistic = t_test_prodFA_readFA[[1]],
  df = t_test_prodFA_readFA[[2]],
  p_value = t_test_prodFA_readFA[[3]],
  std_err = t_test_prodFA_readFA[[7]])

####################################
#between subjects t-test for pure conditions
dat_test_pureproduce2 <- (dat_test_pureproduce_DF_after[,-c(1,3:8,10:13,15:28)])
dat_test_pureread2 <- (dat_test_pureread_DF_after[,-c(1,3:8,10:13,15:28)])
dat_test_pure_all <- rbind(dat_test_pureproduce2, dat_test_pureread2)
dat_test_pure_all$type <- as.character(dat_test_pure_all$condition)

#aggregate data to test the between subjects PE
Between_ProdEff_AOV <- dat_test_pure_all %>% 
  group_by(subject, condition) %>%
  dplyr::summarise(
    hits = mean(response, na.rm = FALSE))

ANOVA_pure2 <- aov(hits ~ condition, data = Between_ProdEff_AOV)
summary(ANOVA_pure2)

#run the pure-list production effect t-test
t_test_pure_PE_during <- t.test(hits ~ condition, data = Between_ProdEff_AOV, paired = FALSE)
t_test_summary5 <- data.frame(
  condition = c("Pure-list DF after"),
  comparison = c("Remember vs. forget hits"),
  t_statistic = t_test_pure_PE_during[[1]],
  df = t_test_pure_PE_during[[2]],
  p_value = t_test_pure_PE_during[[3]],
  std_err = t_test_pure_PE_during[[7]])

#run further pure list contrasts
#pure produce
data_wide_pure_produce <- dcast(dat_test_pureproduce2, subject ~ TargetLureType, value.var="response",fun.aggregate=mean)
data_wide_pure_produce <- (data_wide_pure_produce[,-c(1)])

t_test_pureproduce_PE_during <- t.test(as.matrix(data_wide_pure_produce) %*% c(1,0,-1)) #realted lures vs. unrelated lures
t_test_summary6 <- data.frame(
  condition = c("Pure-remember DF after"),
  comparison = c("Related vs. unrel FAs"),
  t_statistic = t_test_pureproduce_PE_during[[1]],
  df = t_test_pureproduce_PE_during[[2]],
  p_value = t_test_pureproduce_PE_during[[3]],
  std_err = t_test_pureproduce_PE_during[[7]])

t_test_pureproduce_PE_during2 <- t.test(as.matrix(data_wide_pure_produce) %*% c(1,-0.5,-0.5)) #old vs. new
t_test_summary7 <- data.frame(
  condition = c("Pure-remember DF after"),
  comparison = c("Old vs. new"),
  t_statistic = t_test_pureproduce_PE_during2[[1]],
  df = t_test_pureproduce_PE_during2[[2]],
  p_value = t_test_pureproduce_PE_during2[[3]],
  std_err = t_test_pureproduce_PE_during2[[7]])

#pure read contrasts
data_wide_pure_read <- dcast(dat_test_pureread2, subject ~ TargetLureType, value.var="response",fun.aggregate=mean)
data_wide_pure_read <- (data_wide_pure_read[,-c(1)])

t_test_pureread_PE_during <- t.test(as.matrix(data_wide_pure_read) %*% c(1,0,-1)) #realted lures vs. unrelated lures
t_test_summary8 <- data.frame(
  condition = c("Pure-forget DF after"),
  comparison = c("Prod vs. unrel FAs"),
  t_statistic = t_test_pureread_PE_during[[1]],
  df = t_test_pureread_PE_during[[2]],
  p_value = t_test_pureread_PE_during[[3]],
  std_err = t_test_pureread_PE_during[[7]])

t_test_pureread_PE_during2 <- t.test(as.matrix(data_wide_pure_read) %*% c(1,-0.5,-0.5)) #old vs. new
t_test_summary9 <- data.frame(
  condition = c("Pure-forget DF after"),
  comparison = c("Old vs. new"),
  t_statistic = t_test_pureread_PE_during2[[1]],
  df = t_test_pureread_PE_during2[[2]],
  p_value = t_test_pureread_PE_during2[[3]],
  std_err = t_test_pureread_PE_during2[[7]])

contrasts_df_after <- rbind(t_test_summary1, t_test_summary2, t_test_summary3, t_test_summary4, 
                             t_test_summary5, t_test_summary6, t_test_summary7, t_test_summary8, 
                             t_test_summary9)

contrasts_df_after[, c(3,6)] <- round(contrasts_df_after[, c(3,6)], digits = 2)
contrasts_df_after$p_value <- sprintf("%.4f", contrasts_df_after$p_value)

# Display the data frame  -move this to bottom eventually
contrasts_df_after

################################################################################################

#aggregate subject score to be two each first, pureRead_hit, pureProduce_hit, pureRead_FA, pureProduce_FA
# dat_test_pure_all$condition <- as.character(dat_test_pure_all$data_condition)
# 
# dat_test_pure_all$condition[dat_test_pure_all$data_condition == 'produce'] <- 'hit'
# dat_test_pure_all$condition[dat_test_pure_all$data_condition == 'read'] <- 'hit'
# 
# dat_test_pure_all$data_condition <- as.factor(dat_test_pure_all$data_condition)
# 
# # to the the interaction in the pure list conditoin hits vs. FAs. differences of the differnnces
# ANOVA_pure <- aov(response ~ condition * data_condition, data = dat_test_pure_all) 
# summary(ANOVA_pure)

##########################################################





























#dat_test <- dat_test[,-16]
#write.csv(dat_test, "/Users/jackiespear/productionTypingMontHockley.csv", row.names=FALSE)


#write.csv(data_wide, "/Users/jackiespear/productionTypingMontHockley.csv", row.names=FALSE)



