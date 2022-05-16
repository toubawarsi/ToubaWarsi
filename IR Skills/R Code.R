test <- read.csv("C://Data Scientist Skill Assessment Data Example 2021.csv")
#Dropping rows with NA values
test <- test[complete.cases(test), ]


library(reshape2)
library(ggrepel)
library(ggplot2)
library(pscl)

# Question 1 --------------------------------------------------------------

#creating column indicating graduation success in 6 years or less
test$Grad_Status <- ifelse(rowSums(test[ , c(11,12,13)], na.rm=TRUE)==0, 
                           "Did Not Graduate", "Graduated")

#scatterplot Graduation Success by both HS GPA and SAT Total Score
ggplot(test, aes(x=HS_GPA, y=SAT_Total, color=Grad_Status))+
  geom_point(size=2, alpha=0.4)+
  ggtitle("High School GPA vs SAT Total and Graduation Success in 6 years or less")+
  xlab("High School GPA")+
  ylab("SAT Total Score")+
  labs(colour = "Graduation Status")+
  theme_bw()

#comparing average HS GPA and average SAT scores between Graduated and Did Not Graduate
#1)successfully graduated in 6 years or less
graduated <- subset(test, Grad_Status=="Graduated")
Grad_Mean_HSGPA <- round(mean(graduated$HS_GPA), digits=2)
Grad_Mean_SAT <- round(mean(graduated$SAT_Total), digits=0)

#2)did not graduate (DNG)
DNG <- subset(test, Grad_Status=="Did Not Graduate")
DNG_Mean_HSGPA <- round(mean(DNG$HS_GPA), digits =2)
DNG_Mean_SAT <- round(mean(DNG$SAT_Total), digits=0)

CompareData <- data.frame(Grad_Mean_HSGPA,Grad_Mean_SAT,DNG_Mean_HSGPA,DNG_Mean_SAT)

#Visualizing Comparison of Mean High School GPA and graduation success
HSGPA_Compare <- c(Grad_Mean_HSGPA, DNG_Mean_HSGPA)
x <- barplot (HSGPA_Compare, names.arg = c("Graduated", "Did Not Graduate"), 
              ylim=c(0,4), ylab= "GPA", col = c("green", "red"))
y <- c(Grad_Mean_HSGPA,DNG_Mean_HSGPA)
text(x,y/1.5,labels=as.character(y))
title(main = "Mean High School GPA and Graduation Success in 6 Years or Less", font.main = 4)
ylab= "GPA"

#Visualizing Comparison of Mean SAT Score and graduation success
TotalSAT_Compare <- c(Grad_Mean_SAT, DNG_Mean_SAT)
x <- barplot (TotalSAT_Compare, names.arg = c("Graduated", "Did Not Graduate"), 
              ylim=c(0,1300), ylab= "SAT Score", col = c("green", "red"))
y <- c(Grad_Mean_SAT,DNG_Mean_SAT)
text(x,y/1.5,labels=as.character(y))
title(main = "Mean Total SAT Score and Graduation Success in 6 Years or Less", font.main = 4)


#percentage graduation success by HSGPA

#HSGPA 1-2
HSGPA1 <- test %>%   filter(HS_GPA <2)
#Percent Graduated
HSGPA1_Grad <- (sum(HSGPA1$Grad_Status=="Graduated")/nrow(HSGPA1))*100

#HSGPA 2-3
HSGPA2 <- test %>%   filter(HS_GPA>2 & HS_GPA <3)
#Percent Graduated
HSGPA2_Grad <- (sum(HSGPA2$Grad_Status=="Graduated")/nrow(HSGPA2))*100

#HSGPA 3-4
HSGPA3 <- test %>%   filter(HS_GPA>3 & HS_GPA <4)
#Percent Graduated
HSGPA3_Grad <- (sum(HSGPA3$Grad_Status=="Graduated")/nrow(HSGPA3))*100

#HSGPA 4+
HSGPA4 <- test %>%   filter(HS_GPA>4)
#Percent Graduated
HSGPA4_Grad <- (sum(HSGPA4$Grad_Status=="Graduated")/nrow(HSGPA4))*100

GPA <- c("1-2","2-3","3-4","4+")
Grad_Per <- round(c(HSGPA1_Grad,HSGPA2_Grad,HSGPA3_Grad,HSGPA4_Grad),digits=1)
GPA_GradPer <- data.frame(GPA, Grad_Per)

ggplot(data=GPA_GradPer, aes(x=GPA, y=Grad_Per))+
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Grad_Per), vjust=-0.3, color="black", size=3.5)+
  ggtitle("Percent Graduated in 6 Years or Less by High School GPA")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  ylab("Percent Graduated")+
  xlab("HS GPA")+
  theme_bw()

#percentage graduation success by SAT Total Score
# 1 < SAT TOTal < 850
SAT1 <- test %>%   filter(SAT_Total <850)
#Percent Graduated
SAT1_Grad <- (sum(SAT1$Grad_Status=="Graduated")/nrow(SAT1))*100

#850 < SAT TOTal < 1100
SAT2 <- test %>%   filter(SAT_Total>850 & SAT_Total <1100)
#Percent Graduated
SAT2_Grad <- (sum(SAT2$Grad_Status=="Graduated")/nrow(SAT2))*100

#1100 < SAT TOTal < 1350
SAT3 <- test %>%   filter(SAT_Total>1100 & SAT_Total <1350)
#Percent Graduated
SAT3_Grad <- (sum(SAT3$Grad_Status=="Graduated")/nrow(SAT3))*100

#1350 < SAT TOTal < 1600
SAT4 <- test %>%   filter(SAT_Total>1350)
#Percent Graduated
SAT4_Grad <- (sum(SAT4$Grad_Status=="Graduated")/nrow(SAT4))*100


SAT <- c("1-850","850-1100","1100-1350","1350-1600")
SATGrad_Per <- round(c(SAT1_Grad,SAT2_Grad,SAT3_Grad,SAT4_Grad),digits=1)
SAT_GradPer <- data.frame(SAT, SATGrad_Per)
# make SAT_GradPer$SAT an ordered factor to avoid wonky ggplot shennanigans
SAT_GradPer$SAT <- factor(SAT_GradPer$SAT, levels = SAT_GradPer$SAT)


ggplot(data=SAT_GradPer, aes(x=SAT, y=SATGrad_Per))+
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=SATGrad_Per), vjust=-0.3, color="black", size=3.5)+
  ylim(0,14)+
  ggtitle("Percent Graduated in 6 years or less by SAT Total Score")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))+
  ylab("Percent Graduated")+
  xlab("SAT Total Score")+
  theme_bw()


#addressing gender equity with respect to graduation success
gender_gradSuccess <- table(test$Sex,test$Grad_Status)
gender_gradSuccess

#scatterplot Graduation Success by sex
GraduatedSex <- test %>%   filter(Grad_Status=="Graduated")
ggplot(GraduatedSex, aes(x=HS_GPA, y=SAT_Total, color=Sex))+
  geom_point(size=2, alpha=0.4)+
  ggtitle("High School GPA vs SAT Total and Graduation Success in 6 years by Student Sex")+
  xlab("High School GPA")+
  ylab("SAT Total Score")+
  theme_bw()

#percentage graduation success vs HSGPA by sex

#HSGPA 1-2
HSGPA1 <- test %>%   filter(HS_GPA <2)
#Percent Female Graduated
HSGPA1_FGrad <- HSGPA1 %>%   filter(Sex=="F")
HSGPA1_FGrad <- (sum(HSGPA1_FGrad$Grad_Status=="Graduated")/nrow(HSGPA1_FGrad))*100
#Percent Male Graduated
HSGPA1_MGrad <- HSGPA1 %>%   filter(Sex=="M")
HSGPA1_MGrad <- (sum(HSGPA1_MGrad$Grad_Status=="Graduated")/nrow(HSGPA1_MGrad))*100

#HSGPA 2-3
HSGPA2 <- test %>%   filter(HS_GPA>2 & HS_GPA <3)
#Percent Female Graduated
HSGPA2_FGrad <- HSGPA2 %>%   filter(Sex=="F")
HSGPA2_FGrad <- (sum(HSGPA2_FGrad$Grad_Status=="Graduated")/nrow(HSGPA2_FGrad))*100
#Percent Male Graduated
HSGPA2_MGrad <- HSGPA2 %>%   filter(Sex=="M")
HSGPA2_MGrad <- (sum(HSGPA2_MGrad$Grad_Status=="Graduated")/nrow(HSGPA2_MGrad))*100

#HSGPA 3-4
HSGPA3 <- test %>%   filter(HS_GPA>3 & HS_GPA <4)
#Percent Female Graduated
HSGPA3_FGrad <- HSGPA3 %>%   filter(Sex=="F")
HSGPA3_FGrad <- (sum(HSGPA3_FGrad$Grad_Status=="Graduated")/nrow(HSGPA3_FGrad))*100
#Percent Male Graduated
HSGPA3_MGrad <- HSGPA3 %>%   filter(Sex=="M")
HSGPA3_MGrad <- (sum(HSGPA3_MGrad$Grad_Status=="Graduated")/nrow(HSGPA3_MGrad))*100

#HSGPA 4+
HSGPA4 <- test %>%   filter(HS_GPA>4)
#Percent Female Graduated
HSGPA4_FGrad <- HSGPA4 %>%   filter(Sex=="F")
HSGPA4_FGrad <- (sum(HSGPA4_FGrad$Grad_Status=="Graduated")/nrow(HSGPA4_FGrad))*100
#Percent Male Graduated
HSGPA4_MGrad <- HSGPA4 %>%   filter(Sex=="M")
HSGPA4_MGrad <- (sum(HSGPA4_MGrad$Grad_Status=="Graduated")/nrow(HSGPA4_MGrad))*100


GPA <- c("1-2","2-3","3-4","4+")
Female <- round(c(HSGPA1_FGrad, HSGPA2_FGrad, HSGPA3_FGrad, HSGPA4_FGrad),digits=1)
Male<- round(c(HSGPA1_MGrad,HSGPA2_MGrad,HSGPA3_MGrad,HSGPA4_MGrad),digits=1)
GPA_SexGradPer <- data.frame(GPA, Female, Male)
#replacing NaN with 0
GPA_SexGradPer[is.na(GPA_SexGradPer)] <- 0


dfp1 <- melt(GPA_SexGradPer)
names(dfp1)[3] <- "Percent"
ggplot(dfp1, aes(x = GPA, y= Percent, fill = variable), xlab="HS GPA") +
  geom_bar(stat="identity", width=.5, position = "dodge")+
  ggtitle("High School GPA vs Percent Graduated in 6 Years or Less for Male and Female Students")+
  xlab("High School GPA")+
  ylab("Percent Graduated")+
  labs(fill = "Sex of Student")+
  theme_bw()

#percentage graduation success vs SAT Total Score by sex
# 1 < SAT TOTal < 850
SAT1 <- test %>%   filter(SAT_Total <850)
#Percent Female Graduated
SAT1_FGrad <- SAT1 %>%   filter(Sex=="F")
SAT1_FGrad <- (sum(SAT1_FGrad$Grad_Status=="Graduated")/nrow(SAT1_FGrad))*100
#Percent Male Graduated
SAT1_MGrad <- SAT1 %>%   filter(Sex=="M")
SAT1_MGrad <- (sum(SAT1_MGrad$Grad_Status=="Graduated")/nrow(SAT1_MGrad))*100

#850 < SAT TOTal < 1100
SAT2 <- test %>%   filter(SAT_Total>850 & SAT_Total <1100)
#Percent Female Graduated
SAT2_FGrad <- SAT2 %>%   filter(Sex=="F")
SAT2_FGrad <- (sum(SAT2_FGrad$Grad_Status=="Graduated")/nrow(SAT2_FGrad))*100
#Percent Male Graduated
SAT2_MGrad <- SAT2 %>%   filter(Sex=="M")
SAT2_MGrad <- (sum(SAT2_MGrad$Grad_Status=="Graduated")/nrow(SAT2_MGrad))*100

#1100 < SAT TOTal < 1350
SAT3 <- test %>%   filter(SAT_Total>1100 & SAT_Total <1350)
#Percent Female Graduated
SAT3_FGrad <- SAT3 %>%   filter(Sex=="F")
SAT3_FGrad <- (sum(SAT3_FGrad$Grad_Status=="Graduated")/nrow(SAT3_FGrad))*100
#Percent Male Graduated
SAT3_MGrad <- SAT3 %>%   filter(Sex=="M")
SAT3_MGrad <- (sum(SAT3_MGrad$Grad_Status=="Graduated")/nrow(SAT3_MGrad))*100

#1350 < SAT TOTal < 1600
SAT4 <- test %>%   filter(SAT_Total>1350)
#Percent Female Graduated
SAT4_FGrad <- SAT4 %>%   filter(Sex=="F")
SAT4_FGrad <- (sum(SAT4_FGrad$Grad_Status=="Graduated")/nrow(SAT4_FGrad))*100
#Percent Male Graduated
SAT4_MGrad <- SAT4 %>%   filter(Sex=="M")
SAT4_MGrad <- (sum(SAT4_MGrad$Grad_Status=="Graduated")/nrow(SAT4_MGrad))*100


SAT <- c("1-850","850-1100","1100-1350","1350-1600")
Female. <- round(c(SAT1_FGrad, SAT2_FGrad, SAT3_FGrad, SAT4_FGrad),digits=1)
Male. <- round(c(SAT1_MGrad,SAT2_MGrad,SAT3_MGrad,SAT4_MGrad),digits=1)
SAT_SexGradPer <- data.frame(SAT, Female., Male.)
# make SAT_SexGradPer$SAT an ordered factor to avoid wonky ggplot shennanigans
SAT_SexGradPer$SAT <- factor(SAT_SexGradPer$SAT, levels = SAT_SexGradPer$SAT)


dfp2 <- melt(SAT_SexGradPer)
names(dfp2)[3] <- "Percent"
ggplot(dfp2, aes(x = SAT, y= Percent, fill = variable), xlab="SAT Total Score") +
  geom_bar(stat="identity", width=.5, position = "dodge",size=1.5)+
  ggtitle("SAT Total Score vs Percent Graduated in 6 Years or Less for Male and Female Students")+
  xlab("SAT Total Score")+
  ylab("Percent Graduated")+
  labs(fill = "Sex of Student")+
  theme_bw()

# Question 2 --------------------------------------------------------------

#students admitted to the Health and Human Services (HHS) cluster
HHSCluster <- test %>%   filter(Cluster_Admitted=="Health & Human Svcs")
#Turning Grad Status back to 1 or 0
HHSCluster$Grad_Status <- ifelse(HHSCluster$Grad_Status=="Graduated",1, 0)


# Generalized Linear Model because Dependent Variable is binary (graduation success)
# GLMs are an extension of linear regression models that allow the dependent variable to be non-normal
model <- glm(Grad_Status ~ Degree_Objective + HS_GPA + Institution_of_Origin +
               Sex + SAT_Total + Math_All_4yr_HS + Transfer_Units_Earned,
             data=HHSCluster, family=binomial)
summary(model)

pR2(model)

#"does this additional variable improve the model given that these other  
#variables are already in the model?"
an <- anova(model,test="Chisq")
an

#percentage graduation success by Taking Math all 4 years of HS
# Took Math all 4 years
Math4 <- HHSCluster %>%   filter(Math_All_4yr_HS ==1)
#Percent Graduated
Math4Grad <- (sum(Math4$Grad_Status=="Graduated")/nrow(Math4))*100

# Did not take Math all 4 years
NoMath4 <- HHSCluster %>%   filter(Math_All_4yr_HS ==0)
#Percent Graduated
NoMath4Grad <- (sum(NoMath4$Grad_Status=="Graduated")/nrow(NoMath4))*100

Math <- c("Took Math all 4 Years HS","Did Not Take Math all 4 Years HS")
Math4_Per <- round(c(Math4Grad,NoMath4Grad),digits=1)
Math_GradPer <- data.frame(Math, Math4_Per)

ggplot(data=Math_GradPer, aes(x=Math, y=Math4_Per))+
  geom_bar(stat="identity", fill="steelblue")+
  ylim(0,70)+
  ggtitle("Percent Graduated by Math Status")+
  ylab("Percent Graduated")+
  xlab(NULL)+
  theme_bw()
#transfer credits
ggplot(HHSCluster, aes(x=ID, y=Transfer_Units_Earned, color=Grad_Status))+
  geom_point(size=2, alpha=0.4)+
  ggtitle("Graduation Status by Transfer Credits Earned")+
  xlab("Student ID")+
  ylab("Transfer Credits Earned")+
  labs(colour = "Graduation Status")+
  theme_bw()