company_data=read.csv(file.choose())
install.packages("caret")
install.packages("C50")
library("caret")
library("C50")
View(company_data)
high_sale=ifelse(company_data$Sales<10,"NO","YES")
company=data.frame(company_data[-1],high_sale)
#company=data.frame(company_data[-1][-10][-9],high_sale)
View(company)
company_train <- company[1:300,]
company_test <- company[301:400,]

company_dt=C5.0(as.factor(high_sale)~.,data=company_train)
summary(company_dt)
pred <- predict.C5.0(company_dt,company_test[-11])
View(pred)
a <- table(company_test$high_sale,pred)
a
sum(diag(a)/sum(a)) #0.79
plot(company_dt)

#Bagging
install.packages("ipred")
library(ipred)
set.seed(300)
bag=bagging(as.factor(high_sale)~.,data=company_train, nbagg=50)
bag_pred=predict(bag,company_test[-11])
b=table(bag_pred, company_test$high_sale)
b
sum(diag(b)/sum(b)) #0.85
