startups=read.csv(file.choose())
View(startups)
start_up=startups[-c(4)]
View(start_up)
pairs(start_up)
cor(start_up)
#Regression Model
upmodel=lm(Profit~.,data=start_up)
summary(upmodel)  
#R2square = 0.9507

##Checking multicollinearity
library(car)
car::vif(upmodel)
#Model validation techniques
#Diagnostic plots:
#Residual Plots,QQplots,Std.Residuals vs fitted
plot(upmodel)

residualPlots(upmodel)
#added variable plots
avPlots(upmodel)

qqPlot(upmodel)

#Iteration1
start_up1=start_up[-c(46,50),]
upmodel1=lm(Profit~.,data=start_up1)
summary(upmodel1)
#R2square = 0.9633
plot(upmodel)

residualPlots(upmodel)
#added variable plots
avPlots(upmodel)

qqPlot(upmodel1)

#Iteration2... Ok if not used
start_up2=start_up[-c(46,50,49,48),]
upmodel2=lm(Profit~.,data=start_up2)
summary(upmodel2)
qqPlot(upmodel2)

#Iteration 3.. ok if not used
start_up3=start_up[-c(46,50,15,49,48,16),]
upmodel3=lm(Profit~.,data=start_up3)
summary(upmodel3)
qqPlot(upmodel3)
