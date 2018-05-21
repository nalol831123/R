library(data.table)
library(Matrix)
library(xgboost)
library(caret)
library(dplyr)

df_train <- fread('train.csv', sep=",", na.strings = "NA")
df_test  <- fread('test.csv' , sep=",", na.strings = "NA")

df_test[is.na(df_test$Age),"Age"] <- mean(df_test$Age, na.rm = TRUE)
df_train[is.na(df_train$Age),"Age"] <- mean(df_train$Age, na.rm = TRUE)

data = rbind(df_train,df_test,fill=T)
data$Title <- gsub('(.*, )|(\\..*)', '', data$Name)

rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')


data$Title[data$Title == 'Mlle']        <- 'Miss' 
data$Title[data$Title == 'Ms']          <- 'Miss'
data$Title[data$Title == 'Mme']         <- 'Mrs' 
data$Title[data$Title %in% rare_title]  <- 'Rare Title'

data$Surname <- sapply(data$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])


data$Fsize <- data$SibSp + data$Parch + 1

data$FsizeD[data$Fsize == 1] <- 'singleton'
data$FsizeD[data$Fsize < 5 & data$Fsize > 1] <- 'small'
data$FsizeD[data$Fsize > 4] <- 'large'

data$isAlone <- 0
data[data$Fsize == 1,"isAlone"] <- 1


data$Deck <- factor(sapply(data$Cabin, function(x) unlist(strsplit(x, NULL)[[1]][1])))

data <- data[,-c("Ticket","Name","Surname", "Cabin", "Deck")]
ohe_feats = c('Pclass', "Sex",'SibSp' ,'Parch', 'Embarked', 'Title', 'FsizeD', 'isAlone','Fsize')


for (f in ohe_feats){
  levels = unique(data[[f]])
  data[[f]] = factor(data[[f]], level = levels)
}

train = data[data$PassengerId %in% df_train$PassengerId,]
y_train <- train[!is.na(Survived),Survived]
train = train[,Survived:=NULL]
train = train[,PassengerId:=NULL]
train_sparse <- data.matrix(train)

test = data[data$PassengerId  %in% df_test$PassengerId,]
test_ids <- test[,PassengerId]
test[,Survived:=NULL]
test[,PassengerId:=NULL]
test_sparse <- data.matrix(test)

dtrain <- xgb.DMatrix(data=train_sparse, label=y_train)
dtest <- xgb.DMatrix(data=test_sparse)

param <- list(objective   = "binary:logistic",
              eval_metric = "error",
              max_depth   = 7,
              eta         = 0.1,
              gammma      = 1,
              colsample_bytree = 0.5,
              min_child_weight = 1)

set.seed(1234)


system.time(xgb <- xgboost(params  = param,
                           data    = dtrain,
                           label   = y_train, 
                           nrounds = 500,
                           print_every_n = 100,
                           verbose = 1))

pred <- predict(xgb, dtest)
pred.resp <- ifelse(pred >= 0.5, 1, 0)

pred2 <- predict(xgb, dtrain)
pred2.resp <- ifelse(pred2 >= 0.5, 1, 0)
success <- 0
for(i in c(1:891)){
  if(pred2.resp[i] == data$Survived[i]){
    success <- success + 1
  }
}
print(success / 1000)