##載入套件

library(ggplot2)
library(ggthemes)
library(scales)
library(plyr)
library(dplyr)
library(mice)
library(randomForest)
library(party)
library(corrplot)

library(grid)
library(mvtnorm)
library(modeltools)
library(stats4)
library(strucchange)
library(zoo)
library(sandwich)

##讀入檔案

train <- read.csv("titanicTrain.csv", stringsAsFactors = F, na.strings = c("NA", ""))
Question <- read.csv("titanicQuestion.csv", stringsAsFactors = F, na.strings = c("NA", ""))

str(train)
str(Question)

data <- bind_rows(train, Question)
summary(data)

##Pclass對存活率影響
data$survived <- factor(data$survived)
ggplot(data = data[1:nrow(train),], mapping = aes(x = pclass, y = ..count.., fill=survived)) + 
  geom_bar(stat = "count", position='dodge') + 
  xlab("客艙等級") + 
  ylab("乘客數量") + 
  ggtitle("不同客艙等級對乘客存活率影響") + 
  scale_fill_manual(values=c("#FF0000", "#00FF00")) +
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

##Name對存活率影響
########提取姓名中的title
data$Title <- sapply(data$name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
data$Title <- sub(" ","",data$Title)
table(data$Title)

###将数量较少的Title归类为Others，并重新定义一些称呼
Others <- c('Capt','Col','Don','Dona','Jonkheer','Lady','Major','Sir','the Countess')
data$Title[data$Title=='Mlle'] <- 'Miss'
data$Title[data$Title=='Mme'] <- 'Mrs'
data$Title[data$Title=='Ms'] <- 'Miss'
data$Title[data$Title %in% Others] <- 'Others'
table(data$Title)

###用ggplot2绘制不同Title乘客的遇难和存活数
ggplot(data = data[1:891,], mapping = aes(x = Title, y = ..count.., fill=survived)) + 
  geom_bar(stat = "count", position='stack') + 
  xlab('Title') + 
  ylab('Count') + 
  ggtitle('Different Title impact survivor') + 
  scale_fill_discrete(name="survived", breaks=c(0, 1), labels=c("0", "1")) + 
  geom_text(stat = "count", aes(label = ..count..), position=position_stack(vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

###Sex對存活率影響
data$sex <- as.factor(data$sex)
ggplot(data = data[1:891,], mapping = aes(x = sex, y = ..count.., fill=survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  xlab('sex') + 
  ylab('count') + 
  ggtitle('Different Sex impact survivor') + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

#####先看sibsp对存活率的影响
ggplot(data = data[1:891,], mapping = aes(x = sibsp, y = ..count.., fill=survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  labs(title = "Different SibSp impact survivor", x = "sibsp", y = "Count", fill = "survived") + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) +
  scale_x_continuous(breaks = c(0:8)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")
#####再看Parch对存活率的影响
ggplot(data = data[1:891,], mapping = aes(x = parch, y = ..count.., fill=survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  labs(title = "Different Parch impact survivor", x = "Parch", y = "Count", fill = "Survived") + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  scale_x_continuous(breaks = c(0:6)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

#####新变量FamilySize对存活率的影响
data$FamilySize <- data$sibsp + data$parch + 1
data$FamilySize
ggplot(data = data[1:891,], mapping = aes(x = FamilySize, y = ..count.., fill=survived)) + 
  geom_bar(stat = 'count', position='dodge') + 
  xlab('FamilySize') + 
  ylab('Count') + 
  ggtitle('Different FamilySize impact survivor') + 
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  scale_x_continuous(breaks = c(0:11)) +
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

###ggplot绘制不同年龄的生存情况曲线图
ggplot(data[!is.na(data$survived),],aes(age,color=survived))+
  geom_line(aes(label=..count..), stat = 'bin', binwidth=5)  + 
  labs(title = "Different Age impact survivor", x = "Age", y = "Count", fill = "Survived")+
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

#####生成新变量Age_New并进行分析
data$age_New[data$age < 18] <- 'child'
data$age_New[data$age >= 18] <- 'adult'
data$age_New
table(data$age_New,data$survived)
#####用ggplot2绘制成年人和儿童的存活情况
ggplot(data[!is.na(data$survived),],aes(age_New,fill=survived))+
  geom_bar(stat = 'count',position = 'dodge')+
  ggtitle('Adult and Child  Impact Survivor')+
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

###ticket对存活率的影响
ticket.count <- aggregate(data$ticket, by = list(data$ticket), function(x) sum(!is.na(x)))
ticket.count
table(ticket.count$x)

###票价Fare对存活率的影响
ggplot(data = data[!is.na(data$survived) ,], aes(x = fare, color=survived)) + 
  geom_line(aes(label=..count..), stat = 'bin', binwidth=10)  + 
  labs(title = "Different Fare impact survivor", x = "Fare", y = "Count", fill = "Survived")+
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")


###登船港口Embarked对存活率的影响
ggplot(data[1:891, ], mapping = aes(x = embarked, y = ..count.., fill = survived)) +
  geom_bar(stat = 'count', position='dodge') + 
  xlab('Embarked') +
  ylab('Count') +
  ggtitle('Different Embarked impact survivor') +
  geom_text(stat = "count", aes(label = ..count..), position=position_dodge(width=1), , vjust=-0.5) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position="bottom")

###補充缺失值
sapply(data, function(x) sum(is.na(x)))

data <- data[-c(1001:1310) , ]
sapply(data, function(x) sum(is.na(x)))

###補充Embarked缺失值
embarked.na <- data$embarked
data_169 <- data[169,]
data_285 <- data[285,]
data_169
data_285
##62号乘客和830号乘客的票价Fare都是80， Pclass都是1，假设票价、客舱和等级相同的乘客是在同一个登船港口登船。绘制Embarked,fare,pclass的箱线图
#定义Embarked空白值为“C"
data$embarked[c(169, 285)] <- "C"

###Fare的缺失值
########查看缺失值位置和相关信息
fare.na <- is.na(data$fare)
which(fare.na %in% TRUE)
data_1226 <- data[1226, ]
data_1226
########用中位数填充缺失值
data$fare[1226] <- 8.05
sapply(data,function(x) sum(is.na(x)))

###age缺失值
########查看年龄缺失值相关信息
data[is.na(data$age), ]
########设置随机种子
set.seed(129)
########执行多重插补法并输出
ss <- c('name','ticket','cabin','FamilySize','Survived')
mice_age <- mice(data[,!names(data) %in% ss],method = 'rf')
mice_output <- complete(mice_age)
data$age <- mice_output$age
sapply(data,function(x) sum(is.na(x)))



data$pclass <- factor(data$pclass)
data$Title <- factor(data$Title)
data$sex <- factor(data$sex)
data$age_New <- factor(data$age_New)
data$FamilySize <- factor(data$FamilySize)
data$fare <- factor(data$fare)
data$embarked <- factor(data$embarked)
train <- data[1:1000,]
test <- data[1001:1309,]
set.seed(102)
model <- cforest(survived ~ pclass + Title + sex + age_New + FamilySize + fare + embarked, data = train ,controls = cforest_unbiased(ntree=2000,mtry=3))


predict4 <- predict(model, test,  OOB=TRUE, type = "response")

output <- data.frame(test ,survived = pr)
write.csv(output, file = "predict.csv", row.names = FALSE)