library(kernlab)
library(caret)
library(plyr)
library(ggplot2)
install.packages("psych")
library(psych)
library(e1071)
train_sal=read.csv(file.choose())
test_sal=read.csv(file.choose())
str(train_sal)

class(train_sal)
is.factor(train_sal)
str(train_sal$Salary)
is.factor(test_sal$Salary)

train_sal$Salary=as.factor(train_sal$Salary)
test_sal$Salary=as.factor(test_sal$Salary)
str(test_sal)
str(train_sal)
View(train_sal)

#model
model1=ksvm(train_sal$Salary~.,data=train_sal,
            kernel="vanilladot")
model1
model1_prediction=predict(model1,test_sal[,-14])
model1_prediction
mean(model1_prediction == test_sal$Salary) #84.62

#model
model2=ksvm(train_sal$Salary~.,data=train_sal,
            kernel="rbfdot")
model2
model2_prediction=predict(model2,test_sal[,-14])
model2_prediction
mean(model2_prediction == test_sal$Salary) #85.41


model3=ksvm(train_sal$Salary~.,data=train_sal,
            kernel="polydot")
model3
model3_prediction=predict(model3,test_sal[,-14])
model3_prediction
mean(model3_prediction == test_sal$Salary) #84.6


ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$age, fill = train_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=train_sal,aes(x = train_sal$age, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = "Blue")

ggplot(data=train_sal,aes(x = train_sal$maritalstatus, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')
ggtitle("maritalstatus Density Plot")

