fraud_data=read.csv(file.choose())
library("caret")
library("C50")
View(fraud_data)
income=ifelse(fraud_data$Taxable.Income<=30000,"Risky","Good")
dftable=data.frame(fraud_data[-3],income)
View(dftable)
fraud_train=dftable[1:400,]
fraud_test=dftable[401:600,]


income_dt <- C5.0(as.factor(income) ~., data = fraud_train)
summary(income_dt)
pred <- predict.C5.0(income_dt,fraud_test[-6])
a <- table(fraud_test$income,pred)
a
sum(diag(a)/sum(a)) #84.5%
plot(income_dt)

# Bagging
library("ipred")
set.seed(300)
bag=bagging(as.factor(income)~., data= fraud_train, nbagg=50)
summary(bag)
bag_pred=predict(bag,fraud_test[-6])
b=table(fraud_test$income,bag_pred)
b
sum(diag(b)/sum(b)) #0.77

#Boosting