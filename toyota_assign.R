
toyota=read.csv(file.choose())
View(toyota)
pairs(toyota)
cor(toyota)
toyota1=toyota[c(-1)]
View(toyota1)
pairs(toyota1)
cor(toyota1)
model=lm(Price~.,data=toyota)
summary(model)
#
plot(model)
#Residual plots
library(car)
residualPlots(model)
#added variable plots
avPlots(model)

qqPlot(model)
#deletion diagnostics
influenceIndexPlot(model)

#Iteration 1
toyota2=toyota[-c(81,222),]
model2=lm(Price~.,data=toyota2)
summary(model2)
qqPlot(model2)
#Iteration2
toyota3=toyota[-c(600,602,961,959),]
model3=lm(Price~.,data=toyota3)
summary(model3)

#toyota2['Age2']=toyota$Age_08_04*toyota$Age_08_04
