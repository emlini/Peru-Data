#False Belief analyses


setwd("/Volumes/YARN STASH/Dropbox/Data Analysis Work")
FBdata <- read.csv("Peru Participant Data 180413_EC.csv")
View(FBdata)
FBdata_sub <- subset(FBdata, FBdata$Uconn.ID!="PDC08" & FBdata$Excluded_FB.!="x")
FBdata_sub <- FBdata_sub[1:34,]
View(FBdata_sub)

library(ggplot2)
library(aod)

str(FBdata_sub) #check the variables

#set variables that should be as factors
FBdata_sub$AR.1 <- as.factor(FBdata_sub$AR.1)
FBdata_sub$AR.2 <- as.factor(FBdata_sub$AR.2)
FBdata_sub$UC.1 <- as.factor(FBdata_sub$UC.1)
FBdata_sub$UC.2 <- as.factor(FBdata_sub$UC.2)
FBdata_sub$Other.Disabilities <- as.factor(FBdata_sub$Other.Disabilities)
FBdata_sub$Different.sticker.prediction <- as.factor(FBdata_sub$Different.sticker.prediction)

#Name variables for use in data visualization and models

Age <- FBdata_sub$Age.at.Test
Exp <- FBdata_sub$Age.Exposed
Sex <- FBdata_sub$Gender
Yrs <- FBdata_sub$Number.of.Years.Signing
Dis <- FBdata_sub$Other.Disabilities
StickPred <- FBdata_sub$Different.sticker.prediction
AR1 <- FBdata_sub$AR.1
UC1 <- FBdata_sub$UC.1
PT1 <- FBdata_sub$PTL1
PT2 <- FBdata_sub$PTL2

#check age at test to see whether normally distributed
ggplot(FBdata_sub, aes(x=Age, y=..count..)) + geom_density()
shapiro.test(Age)

#Look at ages of kids who pass vs don't pass two FB tasks
p<-ggplot(FBdata_sub, aes(x=AR1, y=Age)) + geom_boxplot() 
q<-p+ scale_x_discrete(labels = c('Fail', 'Pass'))
r<- q + labs(title="Appearance/Reality Performance by Age", 
	x="Appearance/Reality Performance", 
	y="Age at Test (Years)")
r + theme(plot.title = element_text(hjust = 0.5))
#kids who fail AR seem more widely varied than kids who pass

ggplot(FBdata_sub, aes(x=UC1, y=Age)) + geom_boxplot()

#Are people who make different sticker prediction
	#(predict that someone might like different sticker than they like)
	# more likely to pass/fail AR or UC?
ggplot(FBdata_sub, aes(x=AR1, y=StickPred)) + geom_jitter()
ggplot(FBdata_sub, aes(x=UC1, y=StickPred)) + geom_jitter()

ggplot(FBdata_sub, aes(x=AR1, y=Sex)) + geom_jitter()
ggplot(FBdata_sub, aes(x=UC1, y=Sex)) + geom_jitter()

a<-ggplot(FBdata_sub, aes(x=AR1, y=Exp)) + geom_boxplot()
b<-a+ scale_x_discrete(labels = c('Fail', 'Pass'))
c<- b + labs(title="Appearance/Reality Performance by Age of Language Exposure", 
	x="Appearance/Reality Performance", 
	y="Age of Exposure (Years)")
c + theme(plot.title = element_text(hjust = 0.5))

d<- ggplot(FBdata_sub, aes(x=UC1, y=Exp)) + geom_boxplot()
e<-d+ scale_x_discrete(labels = c('Fail', 'Pass'))
f<- e + labs(title="Unexpected Contents Performance by Age of Language Exposure", 
	x="Unexpected Contents Performance", 
	y="Age of Exposure (Years)")
f + theme(plot.title = element_text(hjust = 0.5))

ggplot(FBdata_sub, aes(x=AR1, y=Dis)) + geom_jitter()
ggplot(FBdata_sub, aes(x=UC1, y=Dis)) + geom_jitter()


ggplot(FBdata_sub, aes(x=AR1, y=Yrs)) + geom_boxplot()
ggplot(FBdata_sub, aes(x=UC1, y=Yrs)) + geom_boxplot()


AR1ExpYrsPred <- glm(AR1~Exp+Yrs+StickPred, family="binomial")
summary(AR1ExpYrsPred)

AR1ExpAgeDis <- glm(AR1~Exp+Age+Dis, family="binomial")
summary(AR1ExpAgeDis)

AR1ExpAge <- glm(AR1~Exp+Age, family="binomial")
summary(AR1ExpAge)

AR1ExpAgeint <- glm(AR1~Exp*Age, family="binomial")
summary(AR1ExpAgeint) 
#All three terms significant, but hard to tell exactly what coefficients mean
exp(cbind(OR=coef(AR1ExpAgeint), confint(AR1ExpAgeint))) #odds ratio & CI


# WANT TO determine how earlier vs later exposed kids do on AR relative to their age
# so found median value of data using summary(Exp) and decided to split data points on that value

##NOT WORKING!!!
newcol <- c()
splitExp <- function(var){
	if (var <=7) y<-"Early"
	else print("made it to else")#y<-"Later"
	newcol <- append(newcol,y)
	return(newcol)
}
ExpSplit <- apply(Exp, 1, splitExp(Exp[]))
#FBdata_subtest <- FBdata_sub #creating dummy dataframe so don't modify orig.
#FBdata_subtest <- cbind(FBdata_sub, ExpSplit)

#raster plot of Exp and Age with AR1 as fill 
ggplot(FBdata_sub, aes(x=Exp, y=Age)) + geom_raster(fill=AR1)

AR1Exp <- glm(AR1~Exp, family="binomial")
summary(AR1Exp)




#UC models

UC1ExpYrsPred <- glm(UC1~Exp+Yrs+StickPred, family="binomial")
summary(UC1ExpYrsPred)

UC1ExpAgeDis <- glm(UC1~Exp+Age+Dis, family="binomial")
summary(UC1ExpAgeDis)

UC1ExpAge <- glm(UC1~Exp+Age, family="binomial")
summary(UC1ExpAge)

UC1ExpAgeint <- glm(UC1~Exp*Age, family="binomial")
summary(UC1ExpAgeint)


UC1Exp <- glm(UC1~Exp, family="binomial")
summary(UC1Exp) ## BEST MODEL ACCORDING TO AIC
#Though predictor Exp not significant
exp(cbind(OR=coef(UC1Exp), confint(UC1Exp))) #odds ratio and CI

UC1Age <- glm(UC1~Age, family="binomial")
summary(UC1Age)


UC1ExpDis <- glm(UC1~Exp+Dis, family="binomial")
summary(UC1ExpDis)

UC1ExpDisint <- glm(UC1~Exp*Dis, family="binomial")
summary(UC1ExpDisint)

UC1ExpSex <- glm(UC1~Exp+Sex, family="binomial")
summary(UC1ExpSex)

UC1ExpSexint <- glm(UC1~Exp*Sex, family="binomial")
summary(UC1ExpSexint)

UC1ExpPred <- glm(UC1~Exp+StickPred, family="binomial")
summary(UC1ExpPred)

UC1ExpPredint <- glm(UC1~Exp*StickPred, family="binomial")
summary(UC1ExpPredint)


UC1ExpAgeintDis <- glm(UC1~Exp*Age+Dis, family="binomial")
summary(UC1ExpAgeintDis)

UC1ExpYrsint <- glm(UC1~Exp*Yrs, family="binomial")
summary(UC1ExpYrsint)

# Are kids who pass AR1 also likely to pass UC1?
ggplot(FBdata_sub, aes(x=AR1, y=UC1)) + geom_jitter()
chisq.test(AR1, UC1)  #NOPE NO RELATIONSHIP


UC1ExpAR1 <- glm(UC1~Exp+AR1, family="binomial")
summary(UC1ExpAR1)

UC1ExpAR1int <- glm(UC1~Exp*AR1, family="binomial")
summary(UC1ExpAR1int)

#Maybe other tasks (like PT) influence performance on UC?
ggplot(FBdata_sub, aes(x=UC1, y=PT2)) + geom_boxplot()
t.test(PT2~UC1, var.equal=FALSE)
#kids who fail are not diff from kids who pass in terms of PT2 scores
ggplot(FBdata_sub, aes(x=UC1, y=PT1)) + geom_boxplot()
t.test(PT1~UC1, var.equal=FALSE)
#nor do the passers differ from failers in terms of PT1 scores


