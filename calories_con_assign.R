wt=read.csv(file.choose())
View(wt)
summary(wt)
plot(wt$Calories.Consumed,wt$Weight.gained..grams.)
colnames(wt)
cor(wt$Calories.Consumed,wt$Weight.gained..grams.)

#Simple Linear Regression
gmodel=lm(Weight.gained..grams.~Calories.Consumed,data=wt) #0.8968
summary(gmodel)
#Prediction
pred=predict(gmodel)
pred

#ggplot
library(ggplot2)
ggplot(data=wt,aes(x=Calories.Consumed,y=Weight.gained..grams.))+
  geom_point(color='blue')+
  geom_line(color='red',data = wt, aes(x=Calories.Consumed, y=pred))

#Logrithmic Model ... Not needed
plot(log(Calories.Consumed),Weight.gained..grams.)
cor(log(Calories.Consumed),Weight.gained..grams.)
gmodel2=lm(Weight.gained..grams.~log(Calories.Consumed),data=wt)
summary(gmodel2)

#Prediction
pred2=predict(gmodel2)
pred2

ggplot(data=wt,aes(x=log(Calories.Consumed),y=Weight.gained..grams.))+
  geom_point(color='blue')+
  geom_line(color='red',data = wt, aes(x=log(Calories.Consumed), y=pred2))

#Exponential Model
gmodel3=lm(log(Weight.gained..grams.)~Calories.Consumed,data=wt) #0.8776
summary(gmodel3)

pred3=predict(gmodel3)
pred3


ggplot(data=wt,aes(x=Calories.Consumed,y=log(Weight.gained..grams.)))+
  geom_point(color='blue')+
  geom_line(color='red',data = wt, aes(x=Calories.Consumed, y=pred3))

# Polynomial model with 2 degree (quadratic model) 
# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))
gmodel4=lm(log(wt$Weight.gained..grams.)~ Calories.Consumed + I(Calories.Consumed*Calories.Consumed),data=wt)
summary(gmodel4) #0.8776

pred4=predict(gmodel4)
pred4

ggplot(data=wt,aes(x=Calories.Consumed + I(Calories.Consumed*Calories.Consumed),y=log(Weight.gained..grams.)))+
  geom_point(color='blue')+
  geom_line(color='red',data = wt, aes(x=Calories.Consumed + I(Calories.Consumed*Calories.Consumed), y=pred4))




