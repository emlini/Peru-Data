#Analysis of performance on a Corsi block task
# Have two tasks, Forward and Reverse

#Predictor variables include Age at test, Age of Exposure to (sign) language, Number of years signing, Sex, and Other Disabilities

library(tidyverse)
library(ggplot2)
library(stargazer)

##ALWAYS CHECK AND SET WORKING DIR
getwd()
#setwd("[Filepath]")

Corsi <- read.csv("Peru Participant Data 180406.csv") #get data
View(Corsi) #look at data frame in RStudio
str(Corsi) #get information about variables (type, # obs, sample vals)

Corsi <- Corsi[1:37,] # There were a couple blank lines in DF

#Set variable names for easy calling

Age <- Corsi$Age.at.Test
AoE <- Corsi$Age.Exposed
Yrs <- Corsi$Number.of.Years.Signing
For <- Corsi$CORSI.1st.Resp..Overall..Forward
Rev <- Corsi$CORSI.1st.Resp..Overall..Reverse
Sex <- Corsi$Gender #sex mislabeled as Gender in original data file
Dis <- Corsi$Other.Disabilities

# Visualize the data

#look at forward and reverse scores by Age at test, Age of exposure, and number of years signing and CHECK CORRELATIONS btwn variables
ggplot(Corsi, aes(x=Age, y=For)) + geom_point() +geom_smooth(method="lm")
cor.test(Age, For) #SIGNIFICANT
ggplot(Corsi, aes(x=Age, y=Rev)) + geom_point() +geom_smooth(method="lm")
cor.test(Age, Rev) #NOT sig

ggplot(Corsi, aes(x=AoE, y=For)) + geom_point() +geom_smooth(method="lm")
ggplot(Corsi, aes(x=AoE, y=Rev)) + geom_point() +geom_smooth(method="lm")
cor.test(AoE, For) #NOT sig
cor.test(AoE, Rev) #NOT sig

ggplot(Corsi, aes(x=Yrs, y=For)) + geom_point() +geom_smooth(method="lm")
ggplot(Corsi, aes(x=Yrs, y=Rev)) + geom_point() +geom_smooth(method="lm")
cor.test(Yrs, For) #NOT sig
cor.test(Yrs, Rev) #NOT sig

#More data visualization: Does Corsi Forward or Reverse vary by Sex?
ggplot(Corsi, aes(x=Sex, y=For)) + geom_boxplot()
t.test(For~Sex, Corsi) #NOT sig

ggplot(Corsi, aes(x=Sex, y=Rev)) + geom_boxplot()
t.test(Rev~Gender, Corsi) #NOT sig (p=.05, CI includes 0)


# do they vary by other disability status 
#as.factor used bc Dis variable (coded 0-1) imported as numerical
ggplot(Corsi, aes(x=as.factor(Dis), y=For)) + geom_boxplot() 
t.test(For~Dis, Corsi) #NOT sig

ggplot(Corsi, aes(x=as.factor(Dis), y=Rev)) + geom_boxplot()
t.test(Rev~Dis, Corsi) #SIG (Grp 0 has higher mean than Grp 1)

#Last visualization: relationship between Corsi Forward and Corsi Reverse (prettified with labels for use in data presentation)
ggplot(Corsi, aes(x=For, y=Rev)) + geom_point() +geom_smooth(method="lm") + labs(title="Corsi Block Performance", x="Forward Proportion Correct", y="Reverse Proportion Correct") + theme(plot.title = element_text(hjust = 0.5))

cor.test(For, Rev) # these ARE significantly correlated (though correlation was linear, and relationship in scatterplot seems not linear--more on this later)

#MODELS PREDICTING CORSI FORWARD PERFORMANCE

CorsiFor_Age_AoE_Yrs <- glm(For ~ Age + AoE + Yrs)
summary(CorsiFor_Age_AoE_Yrs)
#all three predictors significant, AIC: -29.634
stargazer(CorsiFor_Age_AoE_Yrs, out="CorsiFor_Age_AoE_Yrs.htm")

CorsiFor_Age_AoE_Sex <- glm(For ~ Age + AoE + Sex)
summary(CorsiFor_Age_AoE_Sex)
#only Age sign predictros, AIC: -27.135
stargazer(CorsiFor_Age_AoE_Sex, out="CorsiFor_Age_AoE_Sex.htm")

CorsiFor_Age_AoE_Dis <- glm(For ~ Age + AoE + Dis)
summary(CorsiFor_Age_AoE_Dis)
#only Age sig, though Dis p value is .08, AIC: -28.494
stargazer(CorsiFor_Age_AoE_Dis, out="CorsiFor_Age_AoE_Dis.htm")

CorsiFor_Age_AoE <- glm(For ~ Age + AoE)
summary(CorsiFor_Age_AoE)
#only age sig, AIC: -26.983

#Trying model with Age, AoE, Yrs, and Dis
CorsiFor_Age_AoE_Yrs_Dis <- glm(For ~ Age + AoE + Dis + Yrs)
summary(CorsiFor_Age_AoE_Yrs_Dis)
#Not strictly kosher, bc we only have 35 data points and that's too many for the number of predictors
#BUT ALL predictors are significant, and AIC is -32.949
stargazer(CorsiFor_Age_AoE_Yrs_Dis, out="CorsiFor_Age_AoE_Yrs_Dis.htm")

#also tried several interaction models (AoE*Dis, Age*Dis, etc), none of which had better AIC than Age+AoE+Yrs or Age+AoE+Yrs+Dis models
 


#MODELS PREDICTING CORSI REVERSE PERFORMANCE

#trying same predictor set as for Forward (though don't expect it to work as these predictors were not sig related to Reverse perfornace in correlations)
CorsiRev_Age_AoE_Yrs <- glm(Rev ~ Age + AoE + Yrs)
summary(CorsiRev_Age_AoE_Yrs)
#no predictors sig, and AIC: 3.6334

CorsiRev_Age_AoE_Yrs_For <- glm(Rev ~ Age + AoE + Yrs + For)
summary(CorsiRev_Age_AoE_Yrs_For)
#only Forward scores significant predictors of Rev scores, AIC: -9.1792

CorsiRev_Age_AoE_For_int <- glm(Rev ~ Age + AoE*For)
summary(CorsiRev_Age_AoE_For_int)
#still only Forward scores sig predictor, but AIC improved: -11.176 

CorsiRev_AoE_For_int <- glm(Rev ~ AoE*For)
summary(CorsiRev_AoE_For_int)
#Forward scores only sig predictor, but AIC improved: -13.171

CorsiRev_For <- glm(Rev ~ For)
summary(CorsiRev_For)
#Model w/only Forward predicting Reverse score actually BEST of above
#AIC: -14.197, change btwn Null and Residual deviance highest in this out of all models

stargazer(CorsiRev_For, out="CorsiRev_For.htm")
stargazer(CorsiRev_AoE_For_int, out="CorsiRev_AoE_For_int.htm")
stargazer(CorsiRev_Age_AoE_Yrs_For, out="CorsiRev_Age_AoE_Yrs_For.htm")
stargazer(CorsiRev_Age_AoE_Yrs, out="CorsiRev_Age_AoE_Yrs.htm")

#BC I noticed there seems to be a non-linear relationship btwn Forward and Reverse scores, I'd like to try transforming data to try to capture linear relationship (or else use a non-linear model type, which I'm looking into--Exponential Regression seems to perhaps fit)

ggplot(Corsi, aes(x=For, y=log(Rev), label=ID)) + geom_point() +geom_smooth(method="loess") + labs(title="Corsi Block Performance", x="Forward Proportion Correct", y="Reverse Proportion Correct") + theme(plot.title = element_text(hjust = 0.5))


CorsiRev_Forlog <- glm(Rev ~ log(For))
summary(CorsiRev_Forlog)
#This results in log(For) as sig predictor but AIC down: -9.732

CorsiRev_AoE_Forlog_int <- glm(Rev ~ AoE*log(For))
summary(CorsiRev_AoE_Forlog_int)
#worse AIC: -9.146

CorsiRev_AoE_Age_Forlog_int <- glm(Rev ~ Age + AoE*log(For))
summary(CorsiRev_AoE_Age_Forlog_int)
#worse AIC: -7.155

CorsiRevlog_For <- glm(log(Rev)~For)
#THE ABOVE IS AN ACTUAL EXPONENTIAL MODEL BUT RETURNS AN ERROR bc there are '0' values in the Reverse scores, so have to remove them in a copy of the data frame

#Subset the data to include only observations for which score on Reverse Corsi is not 0
Corsi_subset <- subset(Corsi, Corsi$CORSI.1st.Resp..Overall..Reverse!=0)


#Assign new var names
Rev1 <- Corsi_subset$CORSI.1st.Resp..Overall..Reverse
For1 <- Corsi_subset$CORSI.1st.Resp..Overall..Forward
Age1 <- Corsi_subset$Age.at.Test
AoE1 <- Corsi_subset$Age.Exposed
Dis1 <- Corsi_subset$Other.Disabilities
Sex1 <- Corsi_subset$Gender

#Run a few models again using the exponential models

CorsiRevlog_For <- glm(log(Rev1)~For1)
summary(CorsiRevlog_For)
# Forward scores significant predictor, AIC way up: 47.12
# but this may allow me to better capture effect of other variables

CorsiRevlog_Age_For <- glm(log(Rev1)~For1+Age1)
summary(CorsiRevlog_Age_For)

CorsiRevlog_Age_For_AoE <- glm(log(Rev1)~For1+Age1 + AoE1)
summary(CorsiRevlog_Age_For_AoE)

CorsiRevlog_For_AoE <- glm(log(Rev1)~For1+ AoE1)
summary(CorsiRevlog_For_AoE)

CorsiRevlog_For_AoE_int <- glm(log(Rev1)~For1*AoE1)
summary(CorsiRevlog_For_AoE_int)

#NOPE--additional models all have worse AIC values

#SEEMS THAT BEST model so far is a linear model in which only Corsi Forward score predicts Corsi Reverse score

#Considering mediation models (since demographic factors predict Corsi Forward, and Corsi Forward predicts Corsi Reverse)