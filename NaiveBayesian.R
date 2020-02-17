library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(RWeka)
library(RColorBrewer)
library(rattle)
library(Rgraphviz)
library(e1071)
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("Rgraphviz")
football<-read.csv("C:/Users/shrut/Documents/MachineLearning/HW4/Football_Train.csv")
#ID3
football_information<-rpart(Label~.,data=football,parms=list(split="information"),control=rpart.control(minsplit = 1,minbucket = 1,cp=0.01))
summary(football_information)
#C-4.5
football_gainratio<-J48(Label~.,data=football,control=Weka_control(M=1,B=TRUE,U=TRUE))
football_test<-read.csv("C:/Users/shrut/Documents/MachineLearning/HW4/Football_test.csv")
predictmodel1<-predict(football_information,football_test,type="class")
predictmodel1
predictmodel2<-predict(football_gainratio,football_test)
predictmodel2
table1<-table(predictmodel1,football_test$Label)
table2<-table(predictmodel2,football_test$Label)
testmodel1<-confusionMatrix(table1,positive = "Win")
testmodel1$byClass
testmodelwithgainratio<-confusionMatrix(table2,positive = "Win")
testmodelwithgainratio$byClass
precision(table1)
#NaiveBayesian
NaiveBayesModel=naiveBayes(Label ~., data=football)
predictmodel3<-predict(NaiveBayesModel,football_test)
predictmodel3
table3<-table(predictmodel3,football_test$Label)
testmodelwithNaive<-confusionMatrix(table3,positive = "Win")
testmodelwithNaive$byClass