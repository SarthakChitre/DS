dt=read.csv(file.choose())
attach(dt)
View(dt)
summary(dt)
plot(dt$Sorting.Time,dt$Delivery.Time)
cor(Sorting.Time,Delivery.Time)

#Simple Linear Regression
tmodel=lm(Delivery.Time~Sorting.Time,data=dt)
summary(tmodel)
#Prediction
pred=predict(tmodel)
pred
sum(tmodel$residuals)

#ggplot
library(ggplot2)
ggplot(data = dt, aes(x = Sorting.Time, y = Delivery.Time)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dt, aes(x=Sorting.Time, y=pred))

#Logarithmic model
plot(log(Sorting.Time),Delivery.Time)
cor(log(Sorting.Time),Delivery.Time)
tmodel1=lm(Delivery.Time~log(Sorting.Time),data=dt)
summary(tmodel1)

pred1=predict(tmodel1)
pred1

#Exponential method
tmodel2=lm(log(Delivery.Time)~Sorting.Time,data=dt)
summary(tmodel2)

pred2=predict(tmodel2)
pred2

# Polynomial model with 2 degree (quadratic model)

plot(Sorting.Time, Delivery.Time)
plot(Sorting.Time*Sorting.Time, Delivery.Time)

cor(Sorting.Time*Sorting.Time, Delivery.Time)

plot(Sorting.Time*Sorting.Time, log(Delivery.Time))

cor(Sorting.Time, log(Delivery.Time))
cor(Sorting.Time*Sorting.Time, log(Delivery.Time))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

tmodel3 <- lm(log(Delivery.Time) ~ Sorting.Time + I(Sorting.Time*Sorting.Time))
summary(tmodel3)
pred3=predict(tmodel3)
pred3
#ggplot
ggplot(data = dt, aes(x = Sorting.Time + I(Sorting.Time^2), y = log(Delivery.Time))) + 
  geom_point(color='blue') +
  geom_line(color='red',data= dt, aes(x=Sorting.Time + I(Sorting.Time^2), y=pred3))
