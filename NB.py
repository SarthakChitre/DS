import pandas as pd
import numpy as np
######### Iris Data Set ########################
iris = pd.read_csv("E:\\DataSets\\iris.csv")
from sklearn.naive_bayes import GaussianNB
from sklearn.naive_bayes import MultinomialNB
from sklearn.model_selection import train_test_split
from sklearn.metrics import confusion_matrix

ip_columns = ["Sepal.Length","Sepal.Width","Petal.Length","Petal.Width"]
op_column  = ["Species"]

# Splitting data into train and test
Xtrain,Xtest,ytrain,ytest = train_test_split(iris[ip_columns],iris[op_column],test_size=0.3, random_state=0)

ignb = GaussianNB() # normal distribution
#imnb = MultinomialNB() # textual data 

# Building and predicting at the same time 

pred_gnb = ignb.fit(Xtrain,ytrain).predict(Xtest)
#pred_mnb = imnb.fit(Xtrain,ytrain).predict(Xtest)


# Confusion matrix GaussianNB model
confusion_matrix(ytest,pred_gnb) # GaussianNB model
pd.crosstab(ytest.values.flatten(),pred_gnb) # confusion matrix using 
np.mean(pred_gnb==ytest.values.flatten()) # 100%


# Confusion matrix GaussianNB model
#confusion_matrix(ytest,pred_mnb) # GaussianNB model
#pd.crosstab(ytest.values.flatten(),pred_mnb) # confusion matrix using 
#np.mean(pred_mnb==ytest.values.flatten()) # 60%

#confusion_matrix(ytest,pred_mnb) # Multinomal model



############# Reading the Diabetes Data #################
Diabetes = pd.read_csv("E:\\Datasets\\Diabetes.csv")
colnames = list(Diabetes.columns)
predictors = colnames[:8]
target = colnames[8]
# Splitting data into training and testing 
DXtrain,DXtest,Dytrain,Dytest = train_test_split(Diabetes[predictors],Diabetes[target],test_size=0.3, random_state=0)
# Creating GaussianNB and MultinomialNB functions
Dgnb = GaussianNB()
Dmnb = MultinomialNB()
# Building and predicting at the same time 
Dpred_gnb = Dgnb.fit(DXtrain,Dytrain).predict(DXtest)
Dpred_mnb = Dmnb.fit(DXtrain,Dytrain).predict(DXtest)
# Confusion matrix 
# GaussianNB
confusion_matrix(Dytest,Dpred_gnb) 
print ("Accuracy",(138+38)/(138+38+19+36)) # 76.19 

# MultinomialNB
confusion_matrix(Dytest,Dpred_mnb)
print ("Accuracy",(114+36)/(114+43+38+36)) # 64.93
