library(randomForest)
library(corrplot)
library(party)

##讀取檔案
data <- read.csv("all0618.csv")
test <- read.csv("all_test.csv")

##cforest
data$分類 <- factor(data$分類)
data$來賓 <- factor(data$來賓)

set.seed(102)
model <- cforest(factor(g_view) ~ 分類 + 來賓, data = data)

predict1 <- predict(model, test,  OOB=TRUE, type = "response")

success <- 0
for(i in c(1:209)){
  if(predict1[i] == data$g_view[i]){
    success <- success + 1
  }
}
print(success / 1000)
###失敗，正確率0.092

##xgboost
library(xgboost)
library(Matrix)
library(data.table)

##方便閱讀，留下我要的格數就好
topic <- data$分類
guest <- data$來賓
g_view <- data$g_view

##轉成matrix
superhot <- data.frame(topic, guest)
superhot <- data.frame(superhot, g_view)

sparse_matrix <- sparse.model.matrix(g_view~.-1, data = superhot)
output_vector = superhot$g_view == 1

##model
bst <- xgboost(data = sparse_matrix, label = output_vector, max.depth = 4,
               eta = 1, nthread = 2, nround = 7,objective = "binary:logistic")

importance <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bst)
head(importance)

pred <- predict(bst, sparse_matrix)

##跑出來大約有六種結果0.016..、0.027..、0.046..、0.059..、0.066..、0.086..
##分成1、2、3、4、5
for (i in c(1:209)){
  if(0.02>pred[i]&pred[i]>0.01){pred[i] = 5}
  if(0.04>pred[i]&pred[i]>0.02){pred[i] = 4}
  if(0.05>pred[i]&pred[i]>0.04){pred[i] = 3}
  if(0.06>pred[i]&pred[i]>0.05){pred[i] = 2}
  if(1>pred[i]&pred[i]>0.06){pred[i] = 1}
}
##對答案
success <- 0
for(i in c(1:209)){
  if(pred[i] == data$g_view[i]){
    success <- success + 1
  }
}
print(success / 1000)

##失敗、正確率0.04

##再試一次，把來賓拆開
data3 <- read.csv("all0620.csv")

##matrix
sparse_matrix3 <- sparse.model.matrix(g_view~.-1, data = data3)
output_vector = data3$g_view == 1
##model
bst3 <- xgboost(data = sparse_matrix3, label = output_vector, max.depth = 4,
               eta = 1, nthread = 2, nround = 10,objective = "binary:logistic")

importance <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bst)
head(importance)

##predict
pred <- predict(bst3, sparse_matrix3)

##結果有0.002893030、0.005682676、0.014142624、0.051017713、0.904718041四種
##分成1、2、3、4、5
for (i in c(1:209)){
  if(0.005>pred[i]&pred[i]>0.002){pred[i] = 5}
  if(0.01>pred[i]&pred[i]>0.005){pred[i] = 4}
  if(0.05>pred[i]&pred[i]>0.01){pred[i] = 3}
  if(0.9>pred[i]&pred[i]>0.05){pred[i] = 2}
  if(1>pred[i]&pred[i]>0.9){pred[i] = 1}
}
##對答案
success <- 0
for(i in c(1:209)){
  if(pred[i] == data$g_view[i]){
    success <- success + 1
  }
}
print(success / 1000)
##正確率0.105


##轉換表格
fuck <- data.frame()
fuck <- rbind(fuck, t(data3[1,]))
fuck <- rbind(t(data3[1,]), t(data3[2,]))
fuck <- rbind(fuck, t(data3[3,]))
for (i in c(4:209)){
  fuck <- rbind(fuck, t(data3[i,]))
}
colnames(fuck) <- c("guest")

write.csv(fuck,file="fuck",row.names = F)

fuck2 = read.csv("fuck2.csv")

##matrix
sparse_matrix4 <- sparse.model.matrix(g_view~.-1, data = fuck2)
output_vector = fuck2$g_view == 1
##model
bst4 <- xgboost(data = sparse_matrix4, label = output_vector, max.depth = 4,
                eta = 1, nthread = 2, nround = 5,objective = "binary:logistic")


##predict
fuckpred <- predict(bst4, sparse_matrix4)
max(fuckpred)
##結果有0.005357329、0.027304053、0.044980370、0.067359522、0.080388166、0.114250712、0.122233763、0.114250712、0.2499198
##分成1、2、3、4、5
for (i in c(1:1758)){
  if(0.027>fuckpred[i]&fuckpred[i]>0.0053){fuckpred[i] = 6}
  if(0.067>fuckpred[i]&fuckpred[i]>0.027){fuckpred[i] = 5}
  if(0.114>fuckpred[i]&fuckpred[i]>0.067){fuckpred[i] = 3}
  if(0.112>fuckpred[i]&fuckpred[i]>0.114){fuckpred[i] = 3}
  if(0.2>fuckpred[i]&fuckpred[i]>0.112){fuckpred[i] = 2}
  if(1>fuckpred[i]&fuckpred[i]>0.2){fuckpred[i] = 1}
}
##對答案
success <- 0
for(i in c(1:1758)){
  if(fuckpred[i] == fuck2$g_view[i]){
    success <- success + 1
  }
}
print(success / 1000)
##正確率0.373

##資料分析
view <- data$c_view
mean(view)
max(view)
min(view)
topic <- data$分類
table(data)
guest <- fuck2$guest
head(table(guest))

##畫圖
library(ggplot2)

qplot(日期, c_view, data = data, geom = "line")
qplot(x=日期,                               
      y=c_view,                              
      data=data,                      
      geom="point",                         # 圖形=scatter plot
      main = "觀看次數",  
      xlab="date",                          
      ylab="view",                    
      color = 分類                        # 以顏色標註月份，複合式的散布圖
)
ggsave("plot.png", width = 20, height = 10)

canvas <- ggplot(data=data)


夫妻 <- data[data$分類 == "夫妻",]
夫妻平均 <- mean(wife$c_view)
活動 <- data[data$分類 == "活動",]
活動平均 <- mean(活動$c_view)
遊戲 <- data[data$分類 == "遊戲",]
遊戲平均 <- mean(遊戲$c_view)
歌曲 <- data[data$分類 == "歌曲",]
歌曲平均 <- mean(歌曲$c_view)
模仿 <- data[data$分類 == "模仿",]
模仿平均 <- mean(模仿$c_view)
談話性 <- data[data$分類 == "談話性",]
談話性平均 <- mean(談話性$c_view)
職業 <- data[data$分類 == "職業",]
職業平均 <- mean(職業$c_view)

topic <- c("夫妻","活動","遊戲","歌曲","模仿","談話性","職業")
view <- c(夫妻平均,活動平均,遊戲平均,歌曲平均,模仿平均,談話性平均,職業平均)
group <- data.frame(topic,view)

qplot(分類, data = data, geom = "bar")
ggsave("分類.png", width = 8, height = 6)

qplot(topic, data = group, geom = "bar", fill = topic,weight = view) +
  ylab("平均觀看次數") 
ggsave("平均觀看次數.png", width = 8, height = 6)