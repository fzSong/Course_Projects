# -*- coding: utf-8 -*-
"""
Created on Tue Nov 27 19:33:13 2018

@author: ArkSong
"""

#Main file



# <codecell> Package Prepatation

import pandas as pd
import numpy as np
import scipy.optimize as op
from sklearn.metrics import roc_auc_score


# <codecell> 

data=pd.read_csv(r"C:\Users\ArkSong\Desktop\GWU\Machine Learning 1\Final Project\Code\test1.csv")

#data=pd.read_csv(r"C:\Users\ArkSong\Desktop\GWU\Machine Learning 1\Final Project\Code\Admission_Predict.csv")




# <codecell> 

data.columns
data.info()


# <codecell> 

data['term'][data.term == ' 60 months']=1
data['term'][data.term == ' 36 months']=0

data['grade'][data.grade == 'A']=1
data['grade'][data.grade == 'B']=2
data['grade'][data.grade == 'C']=3
data['grade'][data.grade == 'D']=4
data['grade'][data.grade == 'E']=5
data['grade'][data.grade == 'F']=6
data['grade'][data.grade == 'G']=6

data['MORTGAGE']=0
data['RENT']=0
data['MORTGAGE'][data.home_ownership == 'MORTGAGE']=1
data['RENT'][data.home_ownership == 'RENT']=1  #OWN is MORTGAGE=0 and RENT=0

data['Verified']=0
data['S_Verified']=0
data['Verified'][data.verification_status == 'Verified']=1
data['S_Verified'][data.verification_status == 'Source Verified']=1 #Not Verified is Verified=0 and S_Verified=0

data['status']=0
data['status'][data.loan_status == 'Charged Off']=1
data['status'][data.loan_status == 'Fully Paid']=0

data.drop(labels=['home_ownership','term','verification_status','loan_status'],axis=1,inplace=True)

data_v=data.values


# <codecell> 
    
X=np.array(data_v[...,0:(data_v.shape[1]-1)])
Y=np.array(data_v[...,data_v.shape[1]-1])

X=X.reshape(len(X),(data_v.shape[1]-1))
Y=Y.reshape(len(Y),1)

X = X.astype(float)
Y = Y.astype(float)

# <codecell> 

def sigmoid(z):
    
    g=1.0/(1.0+np.exp(-z))
    return g

def init_data(X):
    m,n=X.shape
    X=np.column_stack((np.ones((m,1)),X))
    return X


def costfunction(theta,X,Y,lam):
    m,n=X.shape
    theta=theta.reshape((n,1))
    J=sum(-Y*np.log(sigmoid(np.dot(X,theta)))-
          (1-Y)*np.log(1-sigmoid(np.dot(X,theta))))/m
    J=J+lam*sum(theta[1:n,0]**2)/(2*m)
    return J

def gradient(theta,X,Y,lam):
    m,n=X.shape
    theta=theta.reshape((n,1))
    grad=np.dot(X.T,(sigmoid(np.dot(X,theta))-Y))/m
    temp=np.append(0,lam*theta[1:,...]/m)
    temp=temp.reshape((n,1))
    grad=grad+temp
#    for i in range(1,n):
#        grad[i,0]=grad[i,0]+lam*theta[i,0]/m
    return grad.flatten()


def predict(theta,X):
    X=init_data(X)
    pred=sigmoid(np.dot(X,theta))        
    return pred

def cut_off(pred):
    m=len(pred)
    res=np.zeros(m)
    for i in range(0,m):
        if pred[i]>=0.5:
            res[i]=1
        else:
            res[i]=0
    res=res.reshape((m,1))
    return res
    
def train(X,Y,lam):
    m,n=X.shape
    init_theta=np.zeros(n+1)
    X=init_data(X)
    result=op.minimize(fun=costfunction,x0=init_theta,args=(X,Y,lam),
                       method='TNC',jac=gradient,options={'maxiter':1000})
    theta=result.x
    return theta

def learning_curve(X,Y,X_cv,Y_cv,lam):
    m,n=X.shape
    error_train=np.zeros((m,1))
    error_cv=np.zeros((m,1))
    
    for i in range(0,m):
        theta=train(X[0:i,...],Y[0:i,...],lam)
        Jt=costfunction(theta,init_data(X[0:i,...]),Y[0:i,...],lam)
        Jv=costfunction(theta,X_cv,Y_cv,lam)
        error_train[i,1]=Jt
        error_cv[i,1]=Jv
    
    return error_train,error_cv


# <codecell> 



theta=train(X,Y,0.1)

Y_pred=predict(theta,X)


res=cut_off(Y_pred)

print('Accuracy under 0.5 is',np.mean(res==Y)*100)



print('AUC is',roc_auc_score(Y,Y_pred))




# <codecell> 
    
data_test=pd.read_csv(r"C:\Users\ArkSong\Desktop\GWU\Machine Learning 1\Final Project\Code\ex2data1.csv",header=None)

data_test=np.array(data_test)


X_test=data_test[:,0:2]
Y_test=data_test[:,2]

Y_test=Y_test.reshape((len(X_test),1))

theta_test=train(X_test,Y_test,0)

Y_pred_test=predict(theta_test,X_test)

res_test=cut_off(Y_pred_test)

print('Accuracy under 0.5 is',np.mean(res_test==Y_test)*100)



print('AUC is',roc_auc_score(Y_test,Y_pred_test))

# <codecell> 

