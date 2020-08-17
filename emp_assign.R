emp=read.csv(file.choose())
View(emp)
summary(emp)
attach(emp)
plot(Salary_hike,Churn_out_rate)
cor(Salary_hike,Churn_out_rate)

#Simple Linear Regression
cmodel=lm(Churn_out_rate~Salary_hike,data=emp)
summary(cmodel) #0.8312

#Prediction
pred=predict(cmodel)
pred

cmodel$residuals
sum(cmodel$residuals)

mean(cmodel$residuals)
sqrt(sum(cmodel$residuals^2)/nrow(emp)) 

#ggplot
library(ggplot2)
ggplot(data = emp, aes(x = Salary_hike, y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp, aes(x=Salary_hike, y=pred))

#Logrithimic model
plot(log(Salary_hike),Churn_out_rate)
cor(log(Salary_hike),Churn_out_rate)
cmodel2=lm(Churn_out_rate~log(Salary_hike))
summary(cmodel2) #0.8486

#Prediction
pred2=predict(cmodel2)
pred2

#ggplot2
ggplot(data = emp, aes(x = log(Salary_hike), y = Churn_out_rate)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp, aes(x=log(Salary_hike), y=pred2))

#Exponential Method
plot(Salary_hike,log(Churn_out_rate))
cor(Salary_hike,log(Churn_out_rate))
cmodel3=lm(log(Churn_out_rate)~Salary_hike)
summary(cmodel3) #0.8735
#prediction
pred3=predict(cmodel3)
pred3
#ggplot2
ggplot(data = emp, aes(x = Salary_hike, y = log(Churn_out_rate))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp, aes(x=Salary_hike, y=pred3))

#Polynomial
# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))
plot(Salary_hike*Salary_hike,log(Churn_out_rate))
cor(Salary_hike*Salary_hike,log(Churn_out_rate))
cmodel4=lm(log(Churn_out_rate)~Salary_hike + I(Salary_hike*Salary_hike))
summary(cmodel4) #0.9836
#Prediction
pred4=predict(cmodel4)
pred4
#ggplot
ggplot(data = emp, aes(x = Salary_hike + I(Salary_hike*Salary_hike), y = log(Churn_out_rate))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp, aes(x=Salary_hike + I(Salary_hike*Salary_hike), y=pred4))
