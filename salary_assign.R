sal=read.csv(file.choose())
View(sal)
summary(sal)
attach(sal)
plot(sal$YearsExperience,sal$Salary)
cor(sal$YearsExperience,sal$Salary)

#Simple Linear Regression
smodel=lm(Salary~YearsExperience,data=sal)
summary(smodel) #0.957

#Predict
pred=predict(smodel)
pred

#ggplot
library(ggplot2)
ggplot(data = sal, aes(x = YearsExperience, y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = sal, aes(x=YearsExperience, y=pred))

#Logarithmic method  #Not needed
plot(log(YearsExperience),Salary)
cor(log(YearsExperience),Salary)
smodel2=lm(Salary~log(YearsExperience),data=sal)
summary(smodel2) #0.8539

pred2=predict(smodel2)
pred2

#Exponential method
plot(YearsExperience,log(Salary))
cor(YearsExperience,log(Salary))
smodel3=lm(log(Salary)~YearsExperience,data=sal)
summary(smodel3) #0.932

pred3=predict(smodel3)
pred3

#Polynominal
# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

tmodel4 <- lm(log(Salary) ~ YearsExperience + I(YearsExperience*YearsExperience))
summary(tmodel4)
pred4=predict(tmodel4)
pred4 #0.9486
