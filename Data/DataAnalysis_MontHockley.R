#Data cleaning and analysis for Production by typing using semantically 
#similar words. Lists are presented at study
#materials from mont & hockley

#Getting the data off of Google's Firebase

#this will install the package fireData from GitHub
if (!require("devtools")) install.packages("devtools")
devtools::install_github("Kohze/fireData")
library(fireData) # https://github.com/Kohze/fireData
dataBackup(projectURL = "https://ProductionTypingMontHockley-default-rtdb.firebaseio.com/",   #change
           secretKey = "DcHwrKOfo9aFRyn1aKYUHFpIB200X2AK0miQJEKi",     #change
           "/Users/jackiespear/productiontypingmonthockley.json")
#"/Users/umspear-ins/productiontyping3.json")              #change 

##############################################################################
library("tidyverse")
library(tidyr)
library(plyr)
#install.packages("jsonlite")
library("jsonlite")
data<-unlist(jsonlite::fromJSON("/Users/jackiespear/productiontypingmonthockley.json"), recursive = FALSE, use.names = TRUE)
#install.packages("dplyr")
library(dplyr)
data <- bind_rows(data)

#View(data)
data<-flatten(x = data, recursive = TRUE)

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

data <- data %>% 
  dplyr::rename("category" = "data.category",
                "data_condition" = "data.condition",
                "type2" = "data.type",
                "word" = "data.word")

data$data_condition[data$data_condition=="New"]<-"new"


dat_test <- data[data$phase %in% c("test"), ]
dat_study <- data[data$phase %in% c("study"), ]
dat_demo <- data[data$phase %in% c("Demographics"),]
dat_demo <- dat_demo[,c('subject','type','response')]
#dat_demo< - dat_demo %>% 
#  pivot_wider(names_from = type, values_from = response)

dat_study_prod <- dat_study[dat_study$data_condition %in% c("produce"), ] #only the produced items to look at response to make sure s's are typing
dat_study_read <- dat_study[dat_study$data_condition %in% c("read"), ] #only the read items to look at response to make sure s's are not typing the whole time


#dat_study_prod <- dat_study[dat_study$condition %in% c("phono_produced", "semantic_produced"), ]
#dat_study_read <- dat_study[dat_study$condition %in% c("phono_read", "semantic_read"), ]

#getting number of "yes" responses for delinquent responding during test phase
button_mashing <- dat_test %>%
  group_by(subject) %>%
  dplyr::summarise(Yes_Button_Presses = sum(response == "y", na.rm=TRUE))

#counting the number of times s's did not type when they should have (for produced items)
not_typing_sums <- dat_study_prod %>%
  group_by(subject) %>%
  dplyr::summarise(Number = sum(response =="", na.rm=TRUE))

#number of times s's typed when they shouldn't have (for read items)
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

dat_test = filter(dat_test, 
                  subject != "0s0eg3n4eepm7km" & #not typing
                  subject != "1z5w5gep8a2gdtl", #not typing
                  subject != "4a9z4627majxe1s", #not typing
                  subject != "eblveoakvy1qh6q", #not typing
                  subject != "mx7ft0pge6u9wnk", #not typing
                  subject != "nk3r99cuqr9u78a", #not typing
                  subject != "s5c3w1la2ns8kn4", #not typing
                  subject != "u111r752g3ul2lz", #not typing
                  subject != "s5c3w1la2ns8kn4", #button mashing
                  subject != "pg88ebko8l7umcl", #button mashing
                  subject != "vlm6d0hcbz212mv" #button mashing
 )

data_count_list <- aggregate(data = dat_test,       #counting how many s's in each list
                             subject ~ list,
                             function(subject) length(unique(subject)))
data_count_list



#########################################################################################################
#mean(full_data_test$response == "y")
#########################################################################################################
dat_test$response <- ifelse(dat_test$response =="y", 1, 0)

dat_test_mixed <- dat_test[dat_test$condition %in% c("mixed"), ]
dat_test_pureread <- dat_test[dat_test$condition %in% c("pureread"), ]
dat_test_pureproduce <- dat_test[dat_test$condition %in% c("pureproduce"), ]

#didn't change test lists for the pure conditions, so renaming the conditions to reflect what subjects actually saw
dat_test_pureread$data_condition[dat_test_pureread$data_condition == "produce"] <- "read"
dat_test_pureproduce$data_condition[dat_test_pureproduce$data_condition == "read"] <- "produce"

dat_test_mixed <- dat_test_mixed %>%
  mutate(TargetLureType = case_when((data_condition == 'produce') & (type2 == 'old') ~ 'produce_hit',
                                    (data_condition == 'produce') & (type2 == 'new') ~ 'produce_FA',
                                    (data_condition == 'read') & (type2 == 'old') ~ 'read_hit',
                                    (data_condition == 'read') & (type2 == 'new') ~ 'read_FA',
                                    (data_condition == 'new') & (type2 == 'new') ~ 'unrel_FA'))

dat_test_pureproduce <- dat_test_pureproduce %>%
  mutate(TargetLureType = case_when((data_condition == 'produce') & (type2 == 'old') ~ 'produce_hit',
                                    (data_condition == 'produce') & (type2 == 'new') ~ 'produce_FA',
                                    (data_condition == 'new') & (type2 == 'new') ~ 'unrel_FA'))

dat_test_pureread <- dat_test_pureread %>%
  mutate(TargetLureType = case_when((data_condition == 'read') & (type2 == 'old') ~ 'read_hit',
                                    (data_condition == 'read') & (type2 == 'new') ~ 'read_FA',
                                    (data_condition == 'new') & (type2 == 'new') ~ 'unrel_FA'))


#counting the number of subjects in each condition
length(unique(dat_test_mixed$subject))
length(unique(dat_test_pureproduce$subject))
length(unique(dat_test_pureread$subject))

data_count_list <- aggregate(data = dat_test,       #counting how many s's in each list
                             subject ~ list + condition,
                             function(subject) length(unique(subject)))
data_count_list

library(plotrix) #for standard error

Step1_mixed <- dat_test_mixed %>% #getting hits and FAs for subjects
  group_by(subject, TargetLureType) %>%
  dplyr::summarise(
    means = mean(response, na.rm = TRUE),
    sds = sd(response, na.rm = TRUE))

Step2_mixed <- Step1_mixed %>% group_by(TargetLureType) %>% #getting overall hits and FAs
  dplyr::summarise(
    means = mean(means, na.rm = TRUE),
    sds = sd(sds, na.rm = TRUE))

Step1_pureproduce <- dat_test_pureproduce %>% #getting hits and FAs for subjects
  group_by(subject, TargetLureType) %>%
  dplyr::summarise(
    means = mean(response, na.rm = TRUE),
    sds = sd(response, na.rm = TRUE))

Step2_pureproduce <- Step1_pureproduce %>% group_by(TargetLureType) %>% #getting overall hits and FAs
  dplyr::summarise(
    means = mean(means, na.rm = TRUE),
    sds = sd(sds, na.rm = TRUE))

Step1_pureread <- dat_test_pureread %>% #getting hits and FAs for subjects
  group_by(subject, TargetLureType) %>%
  dplyr::summarise(
    means = mean(response, na.rm = TRUE),
    sds = sd(response, na.rm = TRUE))

Step2_pureread <- Step1_pureread %>% group_by(TargetLureType) %>% #getting overall hits and FAs
  dplyr::summarise(
    means = mean(means, na.rm = TRUE),
    sds = sd(sds, na.rm = TRUE))

vec <- c("produce_hit", "read_hit", "produce_FA", "read_FA",
         "unrel_FA")
vec2 <- c("produce_hit", "produce_FA",
         "unrel_FA")
vec3 <- c("read_hit", "read_FA",
         "unrel_FA")

Step2_mixed_reordered <- Step2_mixed[match(vec, Step2_mixed$TargetLureType), ] 
Step2_pureproduce_reordered <- Step2_pureproduce[match(vec2, Step2_pureproduce$TargetLureType), ] 
Step2_pureread_reordered <- Step2_pureread[match(vec3, Step2_pureread$TargetLureType), ] 




G1 <- ggplot(Step2_mixed_reordered, aes(x=factor(TargetLureType, level=c('produce_hit', 'read_hit', 'produce_FA', 'read_FA', 'unrel_FA')), y = means)) +
  geom_bar(stat = "identity", color="black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()
  )

G1 <- G1 + ggtitle("Mixed") +
  xlab("Condition") + ylab("Proportion Old") +
  ylim(0,1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_col() +
  scale_x_discrete(labels=c('Produced \n Hits', 'Read \n Hits', 'Produce \n FA', 'Read \n FA', 'Unrelated \n FA')) +
  geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2,
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(size = 8)) 


G2 <- ggplot(Step2_pureproduce_reordered, aes(x=factor(TargetLureType, level=c('produce_hit', 'read_hit', 'produce_FA', 'read_FA', 'unrel_FA')), y = means)) +
  geom_bar(stat = "identity", color="black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()
  )

G2 <- G2 + ggtitle("Pure Produce") +
  xlab("Condition") + ylab("Proportion Old") +
  ylim(0,1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_col() +
  scale_x_discrete(labels=c('Produce \n Hit', 'Produce \n FA', 'Unrelated \n FA')) +
  geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2,
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(size = 8)) 


G3 <- ggplot(Step2_pureread_reordered, aes(x = reorder(TargetLureType, -means), y = means)) +
  geom_bar(stat = "identity", color="black") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()
  )

G3 <- G3 + ggtitle("Pure Read") +
  xlab("Condition") + ylab("Proportion Old") +
  ylim(0,1) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_col() +
  scale_x_discrete(labels=c('Read \n Hits', 'Read \n FA', 'Unrelated \n FA')) +
  geom_errorbar(aes(ymin=means-sds, ymax=means+sds), width=.2,
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(size = 8))

library("gridExtra")
library("grid")
grid.arrange(G1, G2, G3, ncol = 3, top=textGrob("Production by Typing with words \n Semantic \n Online"))




# library("ggsignif")
# G3 <- G3 + geom_signif(y_position=c(0.9), 
#                        xmin=c("produce", 1.8), 
#                        xmax=c("read", 2.2),annotation=c("*", "NS"), 
#                        tip_length=0.025)

dat_test$condition <- as.character(dat_test$condition)
dat_test$type2 <- as.character(dat_test$type2)

ANOVA <- aov(response ~ data_condition * type2 * TargetLureType, data = dat_test_mixed)
summary(ANOVA)

dat_test_mixed2 <- (dat_test_mixed[,-c(1:6,8:11,13:23)]) 
table(dat_test_mixed2$TargetLureType)

library(reshape2)
data_wide <- dcast(dat_test_mixed2, subject ~ TargetLureType, value.var="response",fun.aggregate=mean)

mean(data_wide$produce_FA)
mean(data_wide$unrel_FA)
mean(data_wide$read_FA)

data_wide <- as.matrix(data_wide[,-1])   #delete subject column for contrasts
#reoder columns for contrasts

col_order <- c("produce_hit", "read_hit", "produce_FA", "read_FA", "unrel_FA")
data_wide <- data_wide[, col_order]
#conduct contrasts
t.test(as.matrix(data_wide) %*% c(1, -1,0,0,0)) #produce vs. read hits
t.test(as.matrix(data_wide) %*% c(0,0,1,-1, 0)) #produce FA vs. read FA
t.test(as.matrix(data_wide) %*% c(0,0,1,1, -2)) #related lures vs. unrelated lures


##########################################################
dat_test <- dat_test[,-16]
write.csv(dat_test, "/Users/jackiespear/productionTypingMontHockley.csv", row.names=FALSE)


#write.csv(data_wide, "/Users/jackiespear/productionTypingMontHockley.csv", row.names=FALSE)



