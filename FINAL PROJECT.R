#Nathan Estrin
#Marquette University
#FINAL PROJECT R SCRIPT

# 0a. clear working directory
rm(list=ls())

# 0b. import all the necessary libraries for
#management of data, viz, creating regressions
#and CART
library(tidyr)
library(ggplot2)
library(dplyr)
library(readxl)
library(caret)
library(gains)
library(rpart)
library(rpart.plot)
library(pROC)
library(forecast)


#VIZZZZZZ
#--------------------------------------------------
# 1a. import data for bar graph viz

pd_or_nopd <- read_xlsx("C:/Users/nestr/Downloads/COSC 3570 Final Project/DATA.xlsx", 
                        sheet = "Parkinsons or Not")
nonmotor <- read_xlsx("C:/Users/nestr/Downloads/COSC 3570 Final Project/DATA.xlsx", 
                      sheet = "Non Motor Questions")

# 1b. clean up data for bar graph viz

##merged both data sets
pd_or_nopd_nonmotor <- merge(pd_or_nopd,nonmotor, by = "fox_insight_id", all = TRUE) 
View(pd_or_nopd_nonmotor)

##got rid of duplicate observations
no_duplicate <- pd_or_nopd_nonmotor[!duplicated(pd_or_nopd_nonmotor$fox_insight_id),]
View (no_duplicate)

##got ride of N/A values
no_missing_value <- na.omit(no_duplicate)
head(no_missing_value)

##used select() to only include the 5 most prevalent
##early onset symptoms
pd_or_no_pd_revised <- select(no_missing_value,fox_insight_id, age, CurrPDDiag.x , NonMoveSleep, NonMoveConstip, 
                              NonMoveSmell,NonMoveAnxious,NonMoveFeel)
head(pd_or_no_pd_revised)

##did some tidying by using the gather()
pd_or_no_pd_long <- pd_or_no_pd_revised %>%
  gather(symptom, value, NonMoveSleep, NonMoveConstip, NonMoveSmell, NonMoveAnxious, NonMoveFeel)

head(pd_or_no_pd_long)

##filtered the data to show only the undiagnosed and diagnosed participants
##experiencing these symptoms. This will help me showcase that their might be more
##people living with Parkinson's than we think
filtered_pd_data <- filter(pd_or_no_pd_long, value == 1)
View(filtered_pd_data)

# 1c. present bar graph viz
ggplot(filtered_pd_data, aes(x = symptom, fill = factor(CurrPDDiag.x))) +
  geom_bar(position = "dodge") +  # Grouped bar chart
  geom_text(position = position_dodge(width = 0.9), stat = "count", aes(label = after_stat(count), vjust = 1.5, )) +
  labs(title = "Presence of The 5 Most Common Non-Motor Symptoms \n in PD and Non-PD Patients (Yes Responses Only)",
       x = "Symptom", y = "Count of 'Yes' Responses") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  theme_minimal()



#MULTIPLE LINEAR REGRESSION
#---------------------------------------------
# 2a. import the data set
df_lin <-read_xlsx("C:/Users/nestr/Downloads/COSC 3570 Final Project/DATA.xlsx", 
                    sheet = "Linear Regression")

# 2b. clean up the data set 
df_lin1 <- df_lin %>%
  group_by(fox_insight_id) %>%
  summarize_all(function(x) ifelse(all(is.na(x)), NA, na.omit(x)[1]))

df_lin2 <- df_lin1 %>%
  mutate(AgeofDiag = ifelse(is.na(AgeofDiag), median(AgeofDiag, na.rm = TRUE), AgeofDiag))

df_lin3 <- na.omit(df_lin2)

df_lin4 <- df_lin3 %>%
  mutate_all(~ ifelse(. %in% c(2, 3, 4), 0, .))

head(df_lin4)

# 2c. create the multiple multiple linear regression models
model1 <- lm(AgeofDiag ~ toxicant + pesticide + caffeine + alcohol 
             + smoking + head + genetics, data = df_lin4)
summary(model1)

model2 <- lm(AgeofDiag ~ caffeine + smoking + head , data = df_lin4)

summary(model2)



# LOGISTIC REGRESSION
#----------------------------------------------
# 3a. Import logistic regression data
df_log <- read_xlsx("C:/Users/nestr/Downloads/COSC 3570 Final Project/DATA.xlsx", 
                     sheet = "Logistic Regression")

# 3b. Clean up data for logistic regression
df_log1 <- df_log %>%
  group_by(fox_insight_id) %>%
  summarize_all(function(x) ifelse(all(is.na(x)), NA, na.omit(x)[1]))

df_log2 <- na.omit(df_log1)

threshold_age <- 60
threshold_speech <- 3  
threshold_write <- 3  
threshold_tremor <- 3 
threshold_mobility <- 3  
threshold_care <- 3 
threshold_active <- 3
threshold_pain <- 3
threshold_anxious <- 3

df_log2$DiseaseProgression <- ifelse(df_log2$age > threshold_age &
                                    df_log2$MoveTremor > threshold_tremor |
                                    df_log2$Mobility > threshold_mobility |
                                    df_log2$Pain > threshold_pain |
                                    df_log2$Anxious > threshold_anxious |
                                    df_log2$NonMoveConcent == 1|
                                    df_log2$off_episode == 1, 1, 0)
head(df_log2)

# 3c. create the logistic regression model
logmod1 <- glm(DiseaseProgression ~ MoveSpeech + MoveWrite + MoveTremor + 
                 Mobility + Care + Active + Pain + Anxious + NonMoveForget +
                 MedsCurrPD, data = df_log2, family = binomial(link = "logit"))

summary(logmod1)

Pred <- predict(logmod1, type = "response")
Binary <- round(Pred)
100*mean(df_log2$DiseaseProgression == Binary)





# CART ANALYSIS
#-----------------------------------------------
# 4a. import data for classification tree
df_cart <- read_xlsx("C:/Users/nestr/Downloads/COSC 3570 Final Project/DATA.xlsx", 
                 sheet = "CART")
df_sex <- read_xlsx("C:/Users/nestr/Downloads/COSC 3570 Final Project/DATA.xlsx", 
                    sheet = "Sex")


# 4b. clean up data for classification tree (provide an explanation
# of all the changes we have made to the data set)

df_class1 <- merge(df_sex,df_cart, by = "fox_insight_id", all.x = TRUE, all.y = TRUE)

## i had to really clean up this data set for the cart to actually work
df_class2 <- df_class1 %>% 
  filter(diagnosis == 0)%>%
  filter(age > 20 & age < 120)%>%
  mutate(
    sex = ifelse(sex == 1, "Male", ifelse(sex == 2, "Female", sex)),
    family_hist_pd = ifelse(family_hist_pd == 2, NA, family_hist_pd),
    alcohol = ifelse(alcohol == 2, 0, alcohol),
    smoking = ifelse(smoking == 2, 0, smoking),
    pest_at_home = ifelse(pest_at_home == 2, 0, pest_at_home),
    head_injury = ifelse(head_injury == 3, 0, head_injury),
    head_injury = ifelse(head_injury == 2 , NA, ifelse(head_injury == 4, NA, head_injury)),
    smoking = ifelse(smoking == 3, NA, ifelse(smoking == 4, NA, smoking)),
    family_hist_pd = ifelse(family_hist_pd == 3, NA ,family_hist_pd),
    alcohol = ifelse(alcohol == 3, NA, alcohol),
    pest_at_home = ifelse(pest_at_home == 3, NA, pest_at_home))

## i had multiple rows of the same fox_insight_id so i had to make each row unique
## for the results to be accurate
df_class3 <- df_class2 %>% distinct(fox_insight_id, .keep_all = TRUE)

## alot of the observations were incomplete cases so I had to impute 0 and 1s
## by random to be the least bias as possible
df_class4 <- df_class3 %>%
  mutate(across(everything(), ~ifelse(is.na(.), sample(c(0, 1), sum(is.na(.)), replace = TRUE), .)))%>%
  filter(sex %in% c("Male", "Female"))

## i had to create a new variable called risk_category
##which is the dependent variable i will use for the classification tree
df_class5 <- df_class4 %>%
  mutate(
    risk_category = ifelse(
      family_hist_pd == 1 | sex == "Male" & age >= 60 | pest_at_home == 1 ,
        "High Risk",
        "Low Risk"   
      )
    )


head(df_class5)

# 4c. present classification tree

##because this is a classification model R requires
##that the outcome be a categorical data type
df_class5$risk_category <- as.factor(df_class5$risk_category)
##divide the data set into two partitions:
##training data set and validation data set
##the rpart package performs a k-fold cross-validation
##for pruning resultant decision trees, so a test set is not needed
set.seed(1)
myIndex <- createDataPartition(df_class5$risk_category, p=0.7, list=FALSE)
trainSet <- df_class5[myIndex,]
validationSet <- df_class5[-myIndex,]
##DEFAULT TREE
##rpart is now used to generate the default classification tree
##we can specify the model structure, data source, and method
default_tree <- rpart(risk_category ~ sex + age + pest_at_home + alcohol + family_hist_pd + 
                        head_injury + smoking, 
                      data = trainSet, 
                      method = "class")
##results of the default classification can be viewed
##both with the summary function and the tree visual
##the 'type' function is set to 1 to label all nodes except leaves
##the 'under' function places the number of cases under each node
summary(default_tree)
prp(default_tree, 
    type = 1, 
    extra = 1, 
    under = TRUE)

