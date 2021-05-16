# senior_project

##Project Goal:

#Show Connections with Division Champs and League(American Or National League) with Average Player and Runs Scored Through KNN and Decision Tree Classification
Algorithms

#Showed Linear Regression Models to try and show a linear relationship between Playoffs and Wins, Runs Scored, and average player salary in the FIRST MODEL
#SECOND model find a linear relationship betweene playoffs and av. player salary and runs scored

##R Markdown Code
---
title: "test"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
library(tidyverse)
library(randomForest)
library(party)
library(class)
library(FNN) ## knn.reg

```

### load the data
```{r}
# yankee_df_dum : removed NA observatiosn // add dummy_code
yankee_df = read.csv('yankee_df_dum.csv')
set.seed(1)  ## set the random seed before using the function generating random numbers
ran <- sample(1:nrow(yankee_df),.9*nrow(yankee_df))
nor <-function(x) { (x-min(x))/(max(x)-min(x))}
yankee_df_train<-yankee_df[ran,]
yankee_df_test<-yankee_df[-ran,]
```

## EDA
eda

## KNN
```{r}
yankee_df_nor = yankee_df
yankee_df_nor[,c(2,6,8)] = apply(yankee_df[,c(2,6,8)],2,nor) 
 yankee_df_reg<-yankee_df
Win_outcome <- yankee_df_reg %>% select(Win)
 yankee_df_reg<- yankee_df_reg %>% select(-Win)
 str(yankee_df_reg)
 knn.reg.fit = knn.reg(yankee_df_train[,-2], yankee_df_test[,-2],y=yankee_df_train[,2],k=4)
 #predicted Wins
 print(knn.reg.fit)
 #Runs Scored
 knn.reg.fit = knn.reg(yankee_df_train[,-36], yankee_df_test[,-36],y=yankee_df_train[,36],k=4)
print(knn.reg.fit)
```


## R Script Code get_Yankee_DF

library(tidyverse)
library(randomForest)
library(party)
library(dplyr)
library(psych)
# cleaning from the weird data
dat1=read_csv("yankees payroll.csv")
dat2=read_csv("yankees payroll2.csv")
dat3=read_csv("yankees payroll3.csv")

yankee_df = cbind.data.frame(dat1[,1:6],dat2[,4:6],dat3[,7])
yankee_df
# remove space in the names of d.f.
names(yankee_df)[c(6,8,10)] = c("avg_player_salary", "Run_Scored", "Div_Champs")


# check data
which(is.na(yankee_df),arr=T)

# fill the missing value
yankee_df[97,7]
yankee_df[125,9]
yankee_df = yankee_df[-c(97,125),]
sum(is.na(yankee_df))
# yankee_df[97,7] =    #fill here
# yankee_df[125,9] =   #fill here


# dummy!!! remove categorical.
str(yankee_df)
index.categorical = c(3,4,7,10)
temp = NULL
count = 0
for(i in index.categorical){
  count = count +1
  dums = dummy.code(yankee_df[,i])
  dim(dums)[2]-1
  temp[[count]] = dums[,1:(dim(dums)[2]-1)]  
}

yankee_df_dum = yankee_df[,1:2]
yankee_df_dum$League = temp[[1]]

yankee_df_dum = cbind.data.frame(yankee_df_dum, data.frame(temp[[2]]))
yankee_df_dum = cbind.data.frame(yankee_df_dum, yankee_df[,c(5,6)])
yankee_df_dum = cbind.data.frame(yankee_df_dum, data.frame(temp[[3]]))
names(yankee_df_dum)[34] = "playoffs"
yankee_df_dum = cbind.data.frame(yankee_df_dum, yankee_df[,c(8,9)])
yankee_df_dum = cbind.data.frame(yankee_df_dum, data.frame(temp[[4]]))
ncol(yankee_df_dum)
names(yankee_df_dum)[37] = "Div_Champs"
write.csv(yankee_df_dum,"yankee_df_dum.csv")
# names(yankee_df_clean)[c(6,8,10)] = c("avg_player_salary", "Run_Scored", "Div_Champs")
###################


#1 . show one tree -
#2. Random forest - variable importance plot
#3. misclasification rate




set.seed(1)  ## set the random seed before using the function generating random numbers
ran <- sample(1:nrow(yankee_df),.9*nrow(yankee_df))

yankee_df_train<-yankee_df_dum[ran,]
yankee_df_test<-yankee_df_dum[-ran,]



nor <-function(x) { (x-min(x))/(max(x)-min(x))}

# normalize the columns 2, 6, and 8 
# you can use apply function as belows
apply(yankee_df[,c(2,6,8)],2,nor) 

# Thus, if you want to replace them
yankee_df_nor = yankee_df
yankee_df_nor[,c(2,6,8)] = apply(yankee_df[,c(2,6,8)],2,nor) 


# Why do the traning set and the test set use different variables? It should be the same
yankee_df_reg<-yankee_df
Win_outcome <- yankee_df_reg %>% select(Win)
yankee_df_reg<- yankee_df_reg %>% select(-Win)
str(yankee_df_reg)
yankee_df_reg[, c("Loss", "avg_player_salary", "place", "Runs_Scored")] <- scale(yankee_df_reg[, c("Loss", "avg_player_salary", "place", "Runs_Scored")])
yankee_df_reg[c(1,4,5,7,8)] <- scale(yankee_df_reg[c(1,4,5,7,8)])

yankee_df_target<-as.factor(yankee_df[ran,2])
test_target<-as.factor(yankee_df[-ran,2])

library(class)
avg.salary=yankee_df$'average player salary'
divisional_champ<-yankee_df$'Div. Champs'
run_efficiency<-yankee_df$'Runs Scored'

which(is.na(yankee_df_train),arr=T)
#knn classification
set.seed(100000)
# 2 labels  -> dummy code, choose 1 column
# 5 labels  -> 4 columns



pr<-knn(x.train,x.test,cl=y.train,k=3) # this is a classification problem
library(FNN)
knn.fit = knn.reg(yankee_df_train[,-2], yankee_df_test[,-2],y=yankee_df_train[,2],k=4)

summary(knn.fit)

which(is.na(yankee_df_train))
na.fill(yankee_df, c("playoffs", NA))
na.fill(yankee_df, c("place", NA))
# target : class-variables. 
above_500= ifelse(yankee_df$Win>=83, "No","Yes")
# 
# Win: numerical values --- if it is your response, then the model should be regression not the classification
yankee_df$playoffs<-dummy.code(yankee_df$playoffs)


# Y~X   ;; using X, predict Y ;;   train
library(psych)
library(caret)
tb<- table(pr,test_target)
accuracy<-function(x){sum(diag(x)/(sum(rowSums(x))))*100}
accuracy(tb)
cl <- factor(c(rep("s",145), rep("c",145)))
yankee_df_target=cl
knn.cv(train, cl, k = 4, l = 0, prob = FALSE, use.all = TRUE)
Sigma<-matrix(c(1,0,0,1), nrow=2, ncol=2)
indxTrain <- sample(1:nrow(yankee_df), size=580)
yankee_df2<- yankee_df %>% select(yankee_df$Win,yankee_df$place,yankee_df$'Runs Scored',yankee_df$'average player salary')
train <- yankee_df[indxTrain,]
test <- yankee_df[-indTrain]
### change them! it's too large
namekey<-c(runs="Runs_Scored",average_player_salary="average player salary")
plyr::rename(yankee_df, replace=namekey, warn_missing=F)


#decision tree/random forest
library(class)
library(randomForest)

output.tree<-ctree(playoffs~Win+avg_player_salary+place, data=yankee_df)
rf.fit<-randomForest(playoffs~Win+avg_player_salary+place, data=yankee_df)

yankee_df
above_500= ifelse(yankee_df$Win>=83, "No","Yes")
winning_season=data.frame(yankee_df, above_500)
tree.yankee_df=tree(above_500~.-Win, data=yankee_df)
reg.tree<-rpart(above_500 ~ Win + Loss, data=yankee_df)
fit<-randomForest(above_500 ~ Win + Loss, data=yankee_df)
#Variable Importance Plot
(VI_F=importance(reg.tree))


