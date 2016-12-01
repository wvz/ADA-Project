library(dplyr)
library(data.table)

tags<-fread('PythonTags.csv',header=T)
tags<-tags[which(tags$Tag!='python'),]
data<-fread('Python_Questions_clean.csv',header = T,sep="\t")
raw_data<-merge(data,tags,by.x='Id',by.y = 'Id',all.x=T)
grouped_tags<-group_by(tags,Id)%>%
              summarise(tag_count=n())
tag_popu<-as.data.frame(table(tags$Tag))
tag_freq<-merge(tags,tag_popu,by.x='Tag',by.y='Var1',all.x=T)
tag_freq$Freq<-tag_freq$Freq/dim(data)[1]
tag_feature<-group_by(tag_freq,Id)%>%
             summarise(max_tag_freq=max(Freq),sum_tag_freq=sum(Freq))
data_with_tag<-merge(data,grouped_tags,by.x = 'Id',by.y = 'Id',all.x = T)
data_with_tag$tag_count[which(is.na(data_with_tag$tag_count))]<-0
data_with_tag<-merge(data_with_tag,tag_feature,by.x = 'Id',by.y = 'Id',all.x = T)
data_with_tag$max_tag_freq[which(is.na(data_with_tag$max_tag_freq))]<-0
data_with_tag$sum_tag_freq[which(is.na(data_with_tag$sum_tag_freq))]<-0

text_features<-fread('../lib/additonal_python_text_features.csv',header = T)[,-c(1:2)]
exist_features<-read.csv('python_questions_features.csv',header = T)[,-c(1,3:6)]
text_features<-merge(text_features,exist_features,by.x = 'Id',by.y = 'Id',all.x=T)
final_features<-merge(data_with_tag[,-2],text_features,by = 'Id',all.x = T)
save(final_features,file='python_questions_final_features.RData')
write.csv(final_features, file = "R_questions_final_features.csv")
data<-read.csv("R_questions_final_features.csv",header = T)
load('R_questions_final_features.RData')
fit1<-lm(Score~tag_count+max_tag_freq+title_length+title_word_count,data=final_features)
summary(fit1)
plot(fit1$residuals~fit1$fitted.values)
plot(fit1$residuals~final_features$Score)
qqnorm(1/(final_features$Score+15))

#randomForest
library(randomForest)
library(miscTools)
library(ggplot2)

cols <- names(final_features)[-c(1:3,5:6)]
index<-sample(1:147075,50000)

train<-final_features[index,]
test<-final_features[-index,]
rf <- randomForest(Score ~ ., data=train[,cols], ntree=20)

r2 <- rSquared(test$Score, test$Score - predict(rf, test[,cols]))

mse <- mean((test$Score - predict(rf, test[,cols]))^2)

p <- ggplot(aes(x=actual, y=pred),
            data=data.frame(actual=test$Score, pred=predict(rf, test[,cols])))
p + geom_point() +
  geom_abline(color="red") +
  ggtitle(paste("RandomForest Regression in R r^2=", r2, sep=""))
importance(rf)

c2 <- chisq.test(final_features$sum_tag_freq, final_features$Score)
print(c2)

bst <- xgboost(data = train[,], label = train$label, max.depth = 4, nround = 3, objective = "binary:logistic")


library(kernlab)
fit <- ksvm(Score~tag_count+max_tag_freq+title_length+title_word_count, final_features[sample(1:147075,5000),])
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, final_features)
# summarize accuracy
rmse <- mean((final_features$Score - predictions)^2)
print(rmse)