setwd("C:/Users/Ocs/Documents/Coursera/ML_CP")
train<-read.csv(file = "pml-training.csv", header=T, na.strings=c("NA"), stringsAsFactors = F)
test<-read.csv(file = "pml-testing.csv", header=T, na.strings="NA", stringsAsFactors = F)

library(caret)
library(randomForest)

#There are some other non trivial NA like values
train[train==""]<-NA
test[test==""]<-NA
train[train=="#DIV/0!"]<-NA
test[test=="#DIV/0!"]<-NA

#Dropping variables with mainly NA values
#In fact all variables with NA-s
colnas<-NULL
for (i in 1:dim(train)[2]){
  colnas[i]<-sum(is.na(train[,i]))
}
train<-train[colnas<19000]
test<-test [colnas<19000]

#Also checking for rows with NAs, but there is none, because we dropped everything in the prev step.
#But there could be a setting in which there where coloumns (variables), with only some valuse missing
rownas<-NULL
for (j in 1:dim(train)[1]){
  rownas[j]<-sum(is.na(train[j,]))
}
summary(rownas)

#Also the first seven variables are IDs and similar metadata and are not needed for classification
train<-train[,8:length(train)]
test<-test [,8:length(test)]


#My modell will be the following random forest 
#modFit_true<-train(classe ~ ., method="rf",data=train)

#But before submitting I will test is (making the 20 obs in "test" some kind of "validation" instead)
#But because I dont want to overfit, I will carve out a large part (80%) of the given train set for the training
# in my test

subtrain_s <- createDataPartition(y=train$classe, p=0.75, list=FALSE)
subtrain <- train[subtrain_s,]
subtest <- train[-subtrain_s,]
set.seed(42)
modFit_s<-train(classe ~ ., method="rf",data=subtrain)
pred <- predict(modFit_s, subtest)
confusionMatrix(pred, subtest$classe)

# The actual predicting
set.seed(42)
modFit_true<-train(classe ~ ., method="rf",data=train)
pred_true <- predict(modFit_true, test)

# the given code for submission help
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
#subDIR
setwd("C:/Users/Ocs/Documents/Coursera/ML_CP/submission")
pml_write_files(pred_true)

