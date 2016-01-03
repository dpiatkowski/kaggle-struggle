setwd("G:/repo/kaggle-struggle/titanic")

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

train <- read.csv("G:/repo/kaggle-struggle/titanic/data/train.csv")
test <- read.csv("G:/repo/kaggle-struggle/titanic/data/test.csv")

test$Survived <- NA
combi <- rbind(train, test)
combi$Name <- as.character(combi$Name)

combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
combi$Title[combi$Title %in% c('Mme', 'Mlle', 'Dona', 'Lady', 'Miss', 'Mrs', 'the Countess')] <- 'Ms'
combi$Title[combi$Title %in% c('Capt', 'Col', 'Don', 'Dr', 'Jonkheer', 'Major', 'Master', 'Rev', 'Sir')] <- 'Mr'
combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

train <- combi[1:891,]
test <- combi[892:1309,]

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, data=train, method="class")

Prediction <- predict(fit, test, type = "class")

write.csv(data.frame(PassengerId = test$PassengerId, Survived = Prediction), file = "gone.csv", row.names = FALSE)
