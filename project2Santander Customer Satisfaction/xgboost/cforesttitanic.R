library(randomForest)
library(party)
library(corrplot)
data$Pclass <- factor(data$Pclass)
data$Sex <- factor(data$SibSp)
data$Parch <- factor(data$Parch)
data$Embarked <- factor(data$Embarked)
data$Title <- factor(data$Title)
data$FsizeD <- factor(data$FsizeD)
data$isAlone <- factor(data$isAlone)
data$Fsize <- factor(data$Fsize)
train <- data[1:891,]
test <- data[892:1309,]
set.seed(102)
model <- cforest(factor(Survived) ~ Pclass + Sex + SibSp + Parch + Embarked + Title + FsizeD + isAlone + Fsize, data = train)

predict1 <- predict(model, test,  OOB=TRUE, type = "response")
predict2 <- predict(model, train,  OOB=TRUE, type = "response")
success <- 0
for(i in c(1:891)){
  if(predict2[i] == data$Survived[i]){
    success <- success + 1
  }
}
print(success / 1000)
