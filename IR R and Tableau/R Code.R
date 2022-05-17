library(readxl)
library(tidyverse)
library(tidyr)
library(dplyr)
library(writexl)
library(data.table)

test <- readxl::read_xlsx("~Path/Data - Fictionalized Survey Data.xlsx")

#reshape package and tidyverse----
try <- test[,c(1:3,10,12,16)]

#pivoting data from wide to long
pivot <- reshape2::melt(try)

colnames(pivot) <- c("A","B","QuestionID","Response")

# * Simple Agree/Disagree  ----
div <- pivot %>%
  group_by(A,B,QuestionID) %>% 
  count(Response) %>%
  mutate(Sum = sum(n)) %>%
  mutate(Proportion = (n / Sum)) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

#export data to visualization in Tableau
write_xlsx(div,"~Path/Vis1.xlsx")

#tidyverse package ----

# * Gantt Visualization ----

#inspired by: https://youtu.be/JodWmiIxl2c

test <- readxl::read_xlsx("~Path/Data - Fictionalized Survey Data.xlsx")

try <- test %>%
  pivot_longer(!c(A,B), names_to = "QuestionID", values_to = "Score") %>%
  drop_na(Score) %>%
  group_by(A,B,QuestionID) %>%
  mutate(A=case_when(A=="A1"~"Group 1",
                     A=="A2"~"Group 2")) %>%
  mutate(B=case_when(B=="B1"~"Question 1",
                     B=="B2"~"Question 2",
                     B=="B3"~"Question 3",
                     B=="B4"~"Question 4")) %>%
  count(Score) %>%
  mutate_at("Score", as.numeric) %>%
  mutate(NegScore = round(ifelse(Score == 3, 0.5*n, 
                                 ifelse(Score < 3, n, 0)))) %>%
  mutate(TotalNegScore = sum(NegScore)) %>%
  mutate(TotalScore = sum(n)) %>%
  mutate(GanttStart = -TotalNegScore/TotalScore) %>%
  mutate(PerTotSize = n/TotalScore)

#export data to visualization in Tableau
write_xlsx(try,"~Path/IR R and Tableau/Vis2.xlsx")

# Statistical Analysis by B levels ----
#Kruskal-Wallis test with epsilon square for effect size

test <- readxl::read_xlsx("~Path/Data - Fictionalized Survey Data.xlsx")

test <- test[,c(1:5)]

# * B1 Analysis ----

Spot <- subset(test, test$MsYear=="B1")
Spot <- Spot[,-2]
Group <- Spot[,1]
Group<- as.numeric(as.factor(Group$A))
Spot <- Spot[,-1]
Spot <- Spot %>% mutate_if(is.character, as.numeric)

# ** Kruskal-Wallis test----
SpotKWR <- as.data.frame(Rfast::kruskaltests(as.matrix(Spot[,c(1:10)]), ina=Group))
rownames(SpotKWR) <- colnames(Spot)
SpotKWR <- subset(SpotKWR, SpotKWR$pvalue < 0.05)

# ** Epsilon effect size----
Spot <- subset(test, test$MsYear=="B1")
#removing MsYear
Spot <- Spot[,-2]
#keeping A
Group <- Spot[,1]
#removing A to turn score columns numeric
Spot <- Spot[,-1]
#removing non-sig Q9
Spot <- Spot[,-2]
Spot <- Spot %>% mutate_if(is.character, as.numeric)
Spot <- cbind(Group,Spot)
n1 <- sum(table(Spot$Q1,Spot$A))
n2 <- sum(table(Spot$Q10,Spot$A))
n3 <- sum(table(Spot$Q11,Spot$A))
n4 <- sum(table(Spot$Q12,Spot$A))
n5 <- sum(table(Spot$Q14,Spot$A))
n6 <- sum(table(Spot$Q15,Spot$A))
n7 <- sum(table(Spot$Q16,Spot$A))
n8 <- sum(table(Spot$Q17,Spot$A))
n9 <- sum(table(Spot$Q18,Spot$A))
n <- as.data.frame(c(n1,n2,n3,n4,n5,n6,n7,n8,n9))
colnames(n) <- "n"

SpotKWR <- cbind(SpotKWR,n)
SpotKWR$Epsilon <- SpotKWR$stat*(SpotKWR$n+1)/(SpotKWR$n^2-1)


# ** Dunn's Post Hoc----
Spot <- subset(test, test$MsYear=="B1")
#removing MsYear
Spot <- Spot[,-2]
#keeping A
Group <- Spot[,1]
#removing A to turn score columns numeric
Spot <- Spot[,-1]
#removing non-sig Q9
Spot <- Spot[,-2]
Spot <- Spot %>% mutate_if(is.character, as.numeric)
Spot <- cbind(Group,Spot)
colname2 <- names(Spot)[2:10]
colname1 <- names(Spot)[1]

data <- lapply(colname2, function(x) {
  rstatix::dunn_test(Spot, reformulate(colname1, x),  
                     p.adjust.method = "BH")
})
data

B1Final <- SpotKWR
B1Final$MSYear <- rep("B1",nrow(B1Final))
B1Final$QuestionID <- rownames(B1Final)

SpotDunn <- data
#lapply(data, function(x) write.table( data.frame(x), '~Path/Result - B1MainQ Dunns.csv'  , append= T, sep=',' ))
data <- read.csv('~Path/Result - B1MainQ Dunns.csv',row.names = NULL)
data <- data[,-1]
Nth.delete <- function(dataframe, n)dataframe[-(seq(n,to=nrow(dataframe),by=n)),]
data <- Nth.delete(data, 11)
data <- subset(data, data$p.adj.signif != "ns")
names(data)[names(data) == '.y.'] <- 'QuestionID'
writexl::write_xlsx(data,'~/Path/Result - B1MainQ Dunns.xlsx')

# * B2 Analysis ----

Spot <- subset(test, test$MsYear=="B2")
Spot <- Spot[,-2]
Group <- Spot[,1]
Group<- as.numeric(as.factor(Group$A))
Spot <- Spot[,-1]
Spot <- Spot %>% mutate_if(is.character, as.numeric)

# ** Kruskal-Wallis test----
SpotKWR <- as.data.frame(Rfast::kruskaltests(as.matrix(Spot[,c(1:10)]), ina=Group))
rownames(SpotKWR) <- colnames(Spot)
SpotKWR <- subset(SpotKWR, SpotKWR$pvalue < 0.05)

# ** Epsilon effect size----
Spot <- subset(test, test$MsYear=="B2")
#removing MsYear
Spot <- Spot[,-2]
#keeping A
Group <- Spot[,1]
#removing A to turn score columns numeric
Spot <- Spot[,-1]
Spot <- Spot %>% mutate_if(is.character, as.numeric)
Spot <- cbind(Group,Spot)
n1 <- sum(table(Spot$Q1,Spot$A))
n2 <- sum(table(Spot$Q9,Spot$A))
n3 <- sum(table(Spot$Q10,Spot$A))
n4 <- sum(table(Spot$Q11,Spot$A))
n5 <- sum(table(Spot$Q12,Spot$A))
n6 <- sum(table(Spot$Q14,Spot$A))
n7 <- sum(table(Spot$Q15,Spot$A))
n8 <- sum(table(Spot$Q16,Spot$A))
n9 <- sum(table(Spot$Q17,Spot$A))
n10 <- sum(table(Spot$Q18,Spot$A))
n <- as.data.frame(c(n1,n2,n3,n4,n5,n6,n7,n8,n9,n10))
colnames(n) <- "n"

SpotKWR <- cbind(SpotKWR,n)
SpotKWR$Epsilon <- SpotKWR$stat*(SpotKWR$n+1)/(SpotKWR$n^2-1)


# ** Dunn's Post Hoc----
Spot <- subset(test, test$MsYear=="B2")
#removing MsYear
Spot <- Spot[,-2]
#keeping A
Group <- Spot[,1]
#removing A to turn score columns numeric
Spot <- Spot[,-1]
Spot <- Spot %>% mutate_if(is.character, as.numeric)
Spot <- cbind(Group,Spot)
colname2 <- names(Spot)[2:11]
colname1 <- names(Spot)[1]

data <- lapply(colname2, function(x) {
  rstatix::dunn_test(Spot, reformulate(colname1, x),  
                     p.adjust.method = "BH")
})
data

B2Final <- SpotKWR
B2Final$MSYear <- rep("B2",nrow(B2Final))
B2Final$QuestionID <- rownames(B2Final)


B2Dunn <- data
#lapply(data, function(x) write.table( data.frame(x), '~Path/Result - B2MainQ Dunns.csv'  , append= T, sep=',' ))
data <- read.csv('~Path/Result - B2MainQ Dunns.csv',row.names = NULL)
data <- data[,-1]
Nth.delete <- function(dataframe, n)dataframe[-(seq(n,to=nrow(dataframe),by=n)),]
data <- Nth.delete(data, 11)
names(data)[names(data) == '.y.'] <- 'QuestionID'
writexl::write_xlsx(data,'~Path/Result - B2MainQ Dunns.xlsx')

# * B3 Analysis ----

Spot <- subset(test, test$MsYear=="B3")
Spot <- Spot[,-2]
Group <- Spot[,1]
Group<- as.numeric(as.factor(Group$A))
Spot <- Spot[,-1]
Spot <- Spot %>% mutate_if(is.character, as.numeric)

# ** Kruskal-Wallis test----
SpotKWR <- as.data.frame(Rfast::kruskaltests(as.matrix(Spot[,c(1:10)]), ina=Group))
rownames(SpotKWR) <- colnames(Spot)
SpotKWR <- subset(SpotKWR, SpotKWR$pvalue < 0.05)

# ** Epsilon effect size----
Spot <- subset(test, test$MsYear=="B3")
#removing MsYear
Spot <- Spot[,-2]
#keeping A
Group <- Spot[,1]
#removing A to turn score columns numeric
Spot <- Spot[,-1]
Spot <- Spot %>% mutate_if(is.character, as.numeric)
Spot <- cbind(Group,Spot)
n1 <- sum(table(Spot$Q9,Spot$A))
n2 <- sum(table(Spot$Q10,Spot$A))
n3 <- sum(table(Spot$Q11,Spot$A))
n4 <- sum(table(Spot$Q14,Spot$A))
n5 <- sum(table(Spot$Q15,Spot$A))
n6 <- sum(table(Spot$Q16,Spot$A))
n7 <- sum(table(Spot$Q17,Spot$A))
n8 <- sum(table(Spot$Q18,Spot$A))
n <- as.data.frame(c(n1,n2,n3,n4,n5,n6,n7,n8))
colnames(n) <- "n"

SpotKWR <- cbind(SpotKWR,n)
SpotKWR$Epsilon <- SpotKWR$stat*(SpotKWR$n+1)/(SpotKWR$n^2-1)


# ** Dunn's Post Hoc----
Spot <- subset(test, test$MsYear=="B3")
#removing MsYear
Spot <- Spot[,-2]
#keeping A
Group <- Spot[,1]
#removing non-sig Q1 and Q12
Spot <- Spot[,-c(2,6)]
#removing A to turn score columns numeric
Spot <- Spot[,-1]
Spot <- Spot %>% mutate_if(is.character, as.numeric)
Spot <- cbind(Group,Spot)
colname2 <- names(Spot)[2:9]
colname1 <- names(Spot)[1]

data <- lapply(colname2, function(x) {
  rstatix::dunn_test(Spot, reformulate(colname1, x),  
                     p.adjust.method = "BH")
})
data

B3Final <- SpotKWR
B3Final$MSYear <- rep("B3",nrow(B3Final))
B3Final$QuestionID <- rownames(B3Final)

B3Dunn <- data
#lapply(data, function(x) write.table( data.frame(x), '~Path/Result - B3MainQ Dunns.csv'  , append= T, sep=',' ))
data <- read.csv('~Path/Result - B3MainQ Dunns.csv',row.names = NULL)
data <- data[,-1]
Nth.delete <- function(dataframe, n)dataframe[-(seq(n,to=nrow(dataframe),by=n)),]
data <- Nth.delete(data, 11)
names(data)[names(data) == '.y.'] <- 'QuestionID'
writexl::write_xlsx(data,'~Path/Result - B3MainQ Dunns.xlsx')

# * B4 Analysis ----

Spot <- subset(test, test$MsYear=="B4")
Spot <- Spot[,-2]
Group <- Spot[,1]
Group<- as.numeric(as.factor(Group$A))
Spot <- Spot[,-1]
Spot <- Spot %>% mutate_if(is.character, as.numeric)

# ** Kruskal-Wallis test----
SpotKWR <- as.data.frame(Rfast::kruskaltests(as.matrix(Spot[,c(1:10)]), ina=Group))
rownames(SpotKWR) <- colnames(Spot)
SpotKWR <- subset(SpotKWR, SpotKWR$pvalue < 0.05)

# ** Epsilon effect size----
Spot <- subset(test, test$MsYear=="B4")
#removing MsYear
Spot <- Spot[,-2]
#keeping A
Group <- Spot[,1]
#removing A to turn score columns numeric
Spot <- Spot[,-1]
#removing non-sig Q9
Spot <- Spot[,-2]
Spot <- Spot %>% mutate_if(is.character, as.numeric)
Spot <- cbind(Group,Spot)
n1 <- sum(table(Spot$Q1,Spot$A))
n2 <- sum(table(Spot$Q10,Spot$A))
n3 <- sum(table(Spot$Q11,Spot$A))
n5 <- sum(table(Spot$Q14,Spot$A))
n6 <- sum(table(Spot$Q15,Spot$A))
n7 <- sum(table(Spot$Q16,Spot$A))
n8 <- sum(table(Spot$Q17,Spot$A))
n9 <- sum(table(Spot$Q18,Spot$A))
n <- as.data.frame(c(n1,n2,n3,n5,n6,n7,n8,n9))
colnames(n) <- "n"

SpotKWR <- cbind(SpotKWR,n)
SpotKWR$Epsilon <- SpotKWR$stat*(SpotKWR$n+1)/(SpotKWR$n^2-1)


# ** Dunn's Post Hoc----
Spot <- subset(test, test$MsYear=="B4")
#removing MsYear
Spot <- Spot[,-2]
#keeping A
Group <- Spot[,1]
#removing A to turn score columns numeric
Spot <- Spot[,-1]
#removing non-sig Q9 and Q12
Spot <- Spot[,-c(2,5)]
Spot <- Spot %>% mutate_if(is.character, as.numeric)
Spot <- cbind(Group,Spot)
colname2 <- names(Spot)[2:9]
colname1 <- names(Spot)[1]

data <- lapply(colname2, function(x) {
  rstatix::dunn_test(Spot, reformulate(colname1, x),  
                     p.adjust.method = "BH")
})
data

B4Final <- SpotKWR
B4Final$MSYear <- rep("B4",nrow(B4Final))
B4Final$QuestionID <- rownames(B4Final)

B4Dunn <- data
#lapply(data, function(x) write.table( data.frame(x), '~Path/Result - B4MainQ Dunns.csv'  , append= T, sep=',' ))
data <- read.csv('~Path/Result - B4MainQ Dunns.csv',row.names = NULL)
data <- data[,-1]
Nth.delete <- function(dataframe, n)dataframe[-(seq(n,to=nrow(dataframe),by=n)),]
data <- Nth.delete(data, 11)
names(data)[names(data) == '.y.'] <- 'QuestionID'
#writexl::write_xlsx(data,'~Path/Result - B4MainQ Dunns.xlsx')

MainQFinal <- rbind(B1Final,B2Final,B3Final,B4Final)
MainQFinal <- MainQFinal %>%
  mutate(EpsilonInterpretation = case_when( Epsilon < 0.01  ~ "Negligible",
                                            Epsilon > 0.01 & Epsilon < 0.04 ~ 'Weak',
                                            Epsilon > 0.04 & Epsilon < 0.16 ~ 'Moderate',
                                            Epsilon > 0.16 & Epsilon < 0.36 ~ 'Relatively Strong',
                                            Epsilon > 0.36 & Epsilon < 0.64 ~ "Strong",
                                            Epsilon > 0.64 ~ "Very Strong"))
#writexl::write_xlsx(MainQFinal,'~Path/Result - MainQ.xlsx')



