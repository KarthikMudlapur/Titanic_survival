library(foreign)
library(dplyr)
library(psych)
library(Hmisc)
library(ggplot2)
library(caret)
library(e1071)
library(randomForest)
library(missForest)
library(mice)
library(VIM)
library(lattice)
library(pan)
library(rpart)

getwd()

setwd("/Users/karthikmudlapur/desktop/R/Titanic")

Titanic.train <- read.csv(file = "train.csv", header = TRUE, stringsAsFactors = TRUE, na.strings=c(""," ", "NA"))
Titanic.test <- read.csv(file = "test.csv", header = TRUE, stringsAsFactors = TRUE, na.strings=c(""," ", "NA"))

Titanic.train$IsTrainSet <- TRUE 
Titanic.test$IsTrainSet <- FALSE

Titanic.test$Survived <- NA

Titanic_full <- rbind(Titanic.train, Titanic.test)

Titanic_full$Survived <- factor(Titanic_full$Survived)
Titanic_full$Pclass <- factor(Titanic_full$Pclass)

str(Titanic_full)
summary(Titanic_full)

colSums(is.na(Titanic_full))
#to check for missing values in any of the columns

sapply(Titanic_full[,sapply(Titanic_full, is.factor)], nlevels)
#to check the levels of different categorical variables


#Titanic_full <-Titanic_full[colSums(is.na(Titanic_full)) < (nrow(Titanic_full)*0.25)]
#summary(Titanic_full)

# >>>>>>>>> IMPUTING AGE USING LINEAR REGRESSION <<<<<<<<<<< 

Titanic_full [is.na(Titanic_full$Age), "Age"]
#to check the number of missing records

boxplot(Titanic_full$Age)
#to check for outliers as building a LM using least square method is susceptible to outliers, to check

boxplot.stats(Titanic_full$Age)
#to check the value of upper and lower wiskers, all well check from the summary

upper.wisker <- boxplot.stats(Titanic_full$Age)$stats[5]
#for the upper wisker, similarly [1] for the lower wisker

outlier.filter <- Titanic_full$Age < upper.wisker
# we have to remove these values, only upper as there r no outliers on the lower wiskers

Titanic_full[outlier.filter, ]
#to view the non outliers from the entire dataset

Age.model <- lm ( Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked, data = Titanic_full[outlier.filter, ] )
# to build a regression model using the outlier filtered data set

Age.row <- Titanic_full [is.na(Titanic_full$Age), c("Pclass", "Sex", "SibSp" , "Parch" , "Fare", "Embarked")]
#only see when the value of Age is NA and only for the ablove mentioned variables 

predict(Age.model, newdata = Age.row )
#predict the age for only those which has age as missing 

Age.predictions <- predict(Age.model, newdata = Age.row )
#storing the predictions

Titanic_full[is.na(Titanic_full$Age), "Age"] <- Age.predictions
#applying the predictions to the mainset

colSums(is.na(Titanic_full))
#to check if it the predictions has been assigned

#<<<<<<<<<<<<<<<<<<<IMPUTING FARE AND EMBARKED USING kNN>>>>>>>>>>>>>>>>>>>>

Titanic_full <- kNN(Titanic_full, variable = c("Fare", "Embarked"), k=10)
#for both continuous and categorical variables

colSums(is.na(Titanic_full))
#to check if it the predictions has been assigned





g.quant<- Titanic_full[c(2,3,5,6,7,8,10)]
remove(g.quant)
cor(g.quant)
#to check the correlation numbers

pairs(~ Name + Survived + Pclass + Age + SibSp + Parch + Fare, data = Titanic_full)
pairs.panels(Titanic_full[c(2,3,4,5,6,7,8,10)], gap=0)
#to check the correlation matrix

cross.tab<-table(Titanic_full$Survived, Titanic_full$Sex)
cross.tab
#to conduct the chi-square test

chisq.test(cross.tab)
#to check the correlation between 2 categorical variables

anova1<-aov(Survived ~ Pclass + Age + SibSp + Parch + Fare, data = Titanic_full)
#to check correlation between many continuous variables
#can conduct a t-test if only 2 variables
summary(anova1)


Titanic.train <- Titanic_full[Titanic_full$IsTrainSet==TRUE, ]
Titanic.test <- Titanic_full[Titanic_full$IsTrainSet==FALSE, ]

Titanic.test$Survived <- NULL 
Titanic.train$IsTrainSet <- NULL
Titanic.test$IsTrainSet <- NULL
Titanic.test$Cabin <- NULL
Titanic.train$Cabin <- NULL

#ind <- sample(2, nrow(Titanic_full), replace = T, prob = c(0.8,0.2))
#train.data <- Titanic_full [ind==1, ]
#test.data <- Titanic_full [ind==2, ]
#to split the dataset into training and testing dataset

remove(P.survive)


#<<<<<<<<<<<<<<<<<<<<< MODELLING USING LOGISTIC REGRESSION >>>>>>>>>>>>>>>>>>>>

Titanic.train.glm <- glm (formula = Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, family = "binomial"(link = logit), data = Titanic.train)
#to perform a logistic regression

summary(Titanic.train.glm)

Titanic.train.glm
#to view the results of the model

str(Titanic.train)

P = predict(Titanic.train.glm, newdata= Titanic.test, type = "response")
#to predict the accuracy of the model using the tet dataset
P.survive = round(P)
#rounding off the probability

solution<- data.frame(PassengerID=Titanic.test$PassengerId, Survived = P.survive)

write.csv(solution, file = 'logistic_Solution.csv', row.names = F)

confusionMatrix(P.survive, Titanic.test$Survived)
#to get goodness of it, all the different measures(accuracy, sensitivity etc)

#<<<<<<<<<<<<<<<<<<<<<<< MODEL USING RANDOM FOREST >>>>>>>>>>>>>>>>>>>>>>>

rf_model <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = Titanic.train)

pred.rf<- predict(rf_model,newdata=Titanic.test,type='class')
pred.rf

str(pred.rf)

PassengerId <- Titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- pred.rf
tail(output.df)

write.csv(output.df, file = 'random_forest.csv', row.names = F)

remove(output.df)
#<<<<<<<<<<<<<<<<<<<<< MODEL USING DECISION TRESS >>>>>>>>>>>>>>>>>>>>>>>>>

dt_model <- rpart(Survived ~ . , method='class', data= Titanic.train)
pred.dt<- predict(dt_model,newdata=Titanic.test,type='class')

output.df <- as.data.frame(PassengerId)
output.df$Survived <- pred.dt
tail(output.df)

write.csv(output.df, file = 'decision_trees.csv', row.names = F)
