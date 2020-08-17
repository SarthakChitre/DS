computer=read.csv(file.choose())
View(computer)
computer1=computer[-c(1,7,8,9)]
View(computer1)
pairs(computer1)
cor(computer1)
#Regression model
pcm=lm(price~.,data=computer1)
summary(pcm)

#checking for multicollinearity
library(car)
car::vif(pcm)

#Plots
plot(pcm)
residualPlots(pcm)
avPlots(pcm)
qqPlot(pcm)

#Iteration
comp1=computer1[-c(1441,1701),]
comp1['hd2']=comp1$hd*comp1$hd
comp1['trend2']=comp1$trend*comp1$trend

pcm2=lm(price~.,data=comp1)
summary(pcm2)
#0.7273
residualPlots(pcm2)
qqPlot(pcm2)

#Iteration2
comp2=computer1[-c(1441,1701,4074,4283,4072,4281),]
comp2['hd2']=comp2$hd*comp2$hd


pcm3=lm(price~.,data=comp2)
summary(pcm3)  
#R2square=0.7247
residualPlots(pcm3)
avPlots(pcm3)
qqPlot(pcm3)

#Iteration3...No use of doing it
comp3=computer1[-c(1441,1701,4074,4283,4072,4281,4410,4686,4404,4680),]
comp3['trend2']=log(comp3$trend)
pcm4=lm(price~.,data=comp3)
summary(pcm4)
residualPlots(pcm4)
avPlots(pcm3)
qqPlot(pcm3)