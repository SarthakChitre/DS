install.packages("kernlab")
library("kernlab")
library(caret)
library(plyr)
forest_fires=read.csv(file.choose())
View(forest_fires)
class(forest_fires)
str(forest_fires)
forest_fires$size_category=as.factor(forest_fires$size_category)
is.factor(forest_fires$size_category)

#Normalization
normalize=function(x){
  return((x-min(x))/(max(x)-min(x)))
}

#normalize
forest_fires$temp=normalize(forest_fires$temp)
forest_fires$RH   = normalize(forest_fires$RH)
forest_fires$wind = normalize(forest_fires$wind)
forest_fires$rain = normalize(forest_fires$rain)
#forest_fires$FFMC=normalize(forest_fires$FFMC)
#forest_fires$DMC=normalize(forest_fires$DMC)
#forest_fires$DC=normalize(forest_fires$DC)
#forest_fires$ISI=normalize(forest_fires$ISI)
#forest_fires$rain=normalize(forest_fires$rain)
#forest_fires$area=normalize(forest_fires$area)
#forest_fires=forest_fires[,c((3:13),31)]
attach(forest_fires)


View(forest_fires)


#Data Partition
set.seed(123)
inTraining=createDataPartition(forest_fires$size_category,p=0.75,list=F)
ff_train=forest_fires[inTraining,]
ff_test=forest_fires[-inTraining,]

install.packages("e1071")
library(e1071)
install.packages("klar")
library(klaR)

#model Building
model1<-ksvm(ff_train$size_category~temp+rain+wind+RH, 
             data= ff_train,kernel = "vanilladot")
model1
model1_prediction <- predict(model1,ff_test[,-12])
model1_prediction
mean(model1_prediction == ff_test$size_category) #####73.43

model2=ksvm(ff_train$size_category~temp+rain+wind+RH,
            data=ff_train,kernel="rbfdot")
model2
model2_prediction=predict(model2,ff_test[,-12])
mean(model2_prediction == ff_test$size_category) ##72.65

model3<-ksvm(size_category~temp+rain+wind+RH, 
             data= ff_train,kernel = "polydot")
model3
model3_prediction <- predict(model3, ff_test[,-12])
model3_prediction
mean(model3_prediction == ff_test$size_category) ##73.43

model4=ksvm(size_category~temp+rain+wind+RH,
            data=ff_train,kernel="besseldot")
model4
model4_prediction=predict(model4,ff_test)
model4_prediction
a=table(model3_prediction == ff_test$size_category)
a
mean(model3_prediction == ff_test$size_category) #73.2
