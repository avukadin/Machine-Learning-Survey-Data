library(class)
library(e1071)
library(randomForest)
getmode <- function(v) {
  uniqv <- unique(v)
  if(length(uniqv)==length(v)){
    floor(mean(v))
  }
  else{
  uniqv[which.max(tabulate(match(v, uniqv)))]
  }
}


setwd("C:/Users/avukadin/Google Drive/School/841/Project")
#setwd("/Users/avukadin/Google Drive/School/841 (1)/Project")
combined=read.csv("combined.csv",header=TRUE)
test=read.csv("test.csv",header=TRUE)
avars1=read.csv("avars1.csv",header=TRUE)



test$starttime=NULL
test$startdate=NULL
test$endtime=NULL
test$enddate=NULL
test$simpc=NULL
test$sted=NULL
combined$starttime=NULL
combined$startdate=NULL
combined$endtime=NULL
combined$enddate=NULL
combined$simpc=NULL
combined$sted=NULL
for (i in 1:length(combined$core)){
  if(combined$core[i]==""){
    combined$core[i]="leisure"
    
  }
  
}

for (i in 1:length(test$core)){
  if(test$core[i]==""){
    test$core[i]="leisure"
    
  }
  
}

#Find Median Interesting Value For Test set
medianInt=vector(, length=length(test$id))
medianThink=vector(, length=length(test$id))
medianClear=vector(, length=length(test$id))
medianEnjoy=vector(, length=length(test$id))
medianDiff=vector(, length=length(test$id))
for (i in 1:length(test$id)){
  rows=which(test$id[i]==combined$id)
  
  
  medianInt[i]=(floor(getmode(combined$interesting[rows])))
  medianThink[i]=(floor(getmode(combined$thinking[rows])))
  medianClear[i]=(floor(getmode(combined$clear[rows])))
  medianEnjoy[i]=(floor(getmode(combined$enjoy[rows])))
  medianDiff[i]=(floor(getmode(combined$difficult[rows])))
  
  if(is.na(medianInt[i])){
    medianInt[i]=3
    
  }
  if(is.na(medianThink[i])){
    medianThink[i]=3
    
  }
  if(is.na(medianClear[i])){
    medianClear[i]=3
    
  }
 if(is.na(medianDiff[i])){
  medianDiff[i]=3
    
  }
  if(is.na(medianEnjoy[i])){
    medianEnjoy[i]=3
    
  }
}
test=cbind(test,medianInt,medianThink,medianClear,medianDiff,medianEnjoy)

#Find Median Interesting Value For Train set
medianInt=vector(, length=length(avars1$id))
medianThink=vector(, length=length(avars1$id))
medianClear=vector(, length=length(avars1$id))
medianEnjoy=vector(, length=length(avars1$id))
medianDiff=vector(, length=length(avars1$id))
for (i in 1:length(avars1$id)){
  rows=which(avars1$id[i]==combined$id)
  
  
  medianInt[i]=(round(getmode(combined$interesting[rows])))
  medianThink[i]=(round(getmode(combined$thinking[rows])))
  medianClear[i]=(round(getmode(combined$clear[rows])))
  medianEnjoy[i]=(round(getmode(combined$enjoy[rows])))
  medianDiff[i]=(round(getmode(combined$difficult[rows])))
  
  if(is.na(medianInt[i])){
    medianInt[i]=3
    
  }
  if(is.na(medianThink[i])){
    medianThink[i]=3
    
  }
if(is.na(medianClear[i])){
    medianClear[i]=3
    
  }
  if(is.na(medianDiff[i])){
    medianDiff[i]=3
    
  }
  if(is.na(medianEnjoy[i])){
   medianEnjoy[i]=3
    
  }
}

#avars1=model.matrix(avars1$id~.^2, avars1)
#correlationMatrix <- cor(avars1[,1:length(avars1[1,])])

avars1=cbind(avars1,medianInt,medianThink,medianClear,medianDiff,medianEnjoy)

#Combining Training Data
combined=merge(avars1,combined,by.avars1=id,by.combined=id)
core=combined$core
combined$core=NULL

for (i in 1:length(combined[1,])){
  combined[is.na(combined[,i]),i]=mean(na.omit(combined[,i]))
  
}
combined$core=core

#============================TESTING============================================

combined$id=as.factor(combined$id)
#Random Splitting into Test/Train
#================================

train_percentage=0.75

obs=floor(length(combined$interesting)/3)
train_size=floor(obs*train_percentage)
test_size=obs-train_size

set.seed(123)
random_sample=sample(1:obs,obs,replace=F) #Random samples, seed=123

train_set=combined[random_sample[1:train_size],]

test_set=combined[random_sample[(1+train_size):(obs)],]
#================================
test_set$medianInt=as.factor(test_set$medianInt)
train_set$medianInt=as.factor(train_set$medianInt)
test_set$medianEnjoy=as.factor(test_set$medianEnjoy)
train_set$medianEnjoy=as.factor(train_set$medianEnjoy)
test_set$medianDiff=as.factor(test_set$medianDiff)
train_set$medianDiff=as.factor(train_set$medianDiff)
test_set$medianClear=as.factor(test_set$medianClear)
train_set$medianClear=as.factor(train_set$medianClear)
test_set$medianThink=as.factor(test_set$medianThink)
train_set$medianThink=as.factor(train_set$medianThink)

#train_set$core1=NULL
#train_set$id=as.factor(train_set$id)
#test_set$id=as.factor(test_set$id)
train_set$sted=NULL
train_set$obs=NULL
train_set$train=NULL

#train_set$medianInt=NULL
#train_set$medianThink=NULL
#train_set$medianClear=NULL
#train_set$medianDiff=NULL
#train_set$medianEnjoy=NULL

train_set$thinking=NULL
train_set$clear=NULL
train_set$difficult=NULL
train_set$enjoy=NULL



test$starttime=NULL
test$startdate=NULL
test$endtime=NULL
test$enddate=NULL
test$simpc=NULL
test$sted=NULL
combined$starttime=NULL
combined$startdate=NULL
combined$endtime=NULL
combined$enddate=NULL
combined$simpc=NULL
combined$sted=NULL
train_set$starttime=NULL
train_set$startdate=NULL
train_set$endtime=NULL
train_set$enddate=NULL
train_set$simpc=NULL
train_set$sted=NULL


levels( test_set$medianInt)=c(1,2,3,4,5)
levels( train_set$medianInt)=c(1,2,3,4,5)
levels( train_set$medianEnjoy)=c(1,2,3,4,5)
levels( test_set$medianEnjoy)=c(1,2,3,4,5)
levels( test_set$medianThink)=c(1,2,3,4,5)
levels( train_set$medianThink)=c(1,2,3,4,5)
levels( train_set$medianClear)=c(1,2,3,4,5)
levels( test_set$medianClear)=c(1,2,3,4,5)
levels( test_set$medianDiff)=c(1,2,3,4,5)
levels( train_set$medianDiff)=c(1,2,3,4,5)

#Remove Correlated Terms
train_set$nettocat=NULL
train_set$nettoink=NULL
train_set$nettoink_f=NULL
train_set$brutoink=NULL
train_set$netinc=NULL
train_set$lftdcat=NULL
train_set$leeftijd=NULL
train_set$gebjaar=NULL
train_set$brutocat=NULL
train_set$aantalhh=NULL
train_set$woonvorm=NULL
train_set$brutohh_f=NULL
train_set$oplcat=NULL
train_set$oplzon=NULL

SVM=svm(as.factor(train_set$interesting)~.   
        ,data=train_set,probability=TRUE,cost=1,
        kernel="radial",scale=TRUE,type="C-classification")


test_y=test_set$interesting
test_set$interesting=NULL


SVM2=predict(SVM,test_set,probability=TRUE)

#Reorder columns
L1=as.integer(colnames( attr(SVM2,"probabilities")))[1]
L2=as.integer(colnames( attr(SVM2,"probabilities")))[2]
L3=as.integer(colnames( attr(SVM2,"probabilities")))[3]
L4=as.integer(colnames( attr(SVM2,"probabilities")))[4]
L5=as.integer(colnames( attr(SVM2,"probabilities")))[5]

#Log-Loss
LL=0
for (i in 1:test_size){
  LL=LL+log(attr(SVM2,"probabilities")[i,1])*(SVM2[i]==L1)+log(attr(SVM2,"probabilities")[i,2])*(SVM2[i]==L2)
  +log(attr(SVM2,"probabilities")[i,3])*(SVM2[i]==L3)+log(attr(SVM2,"probabilities")[i,4])*(SVM2[i]==L4)
  +log(attr(SVM2,"probabilities")[i,5])*(SVM2[i]==L5)
}

LL=-1*LL/test_size


#=======================================Submitting===============================================================
#Combining Test Data
kaggletemp=merge(test,avars1,by.avars1=id,by.test=id,all=TRUE,sort=FALSE)
kaggle=merge(kaggletemp,test,by.avars1=id,by.test=id,all=FALSE,sort=FALSE)
kaggle$id=as.factor(kaggle$id)
kaggle$interesting=NULL

core=kaggle$core
kaggle$core=NULL
for (i in 1:length(kaggle[1,])){
  kaggle[is.na(kaggle[,i]),i]=mean(na.omit(kaggle[,i]))
  
}


kaggle$core=core
kaggle$medianInt=as.factor(kaggle$medianInt)
kaggle$medianEnjoy=as.factor(kaggle$medianEnjoy)
kaggle$medianDiff=as.factor(kaggle$medianDiff)
kaggle$medianClear=as.factor(kaggle$medianClear)
kaggle$medianThink=as.factor(kaggle$medianThink)


kaggle_p=predict(SVM,kaggle,probability=TRUE)

L1=as.integer(colnames( attr(kaggle_p,"probabilities")))[1]
L2=as.integer(colnames( attr(kaggle_p,"probabilities")))[2]
L3=as.integer(colnames( attr(kaggle_p,"probabilities")))[3]
L4=as.integer(colnames( attr(kaggle_p,"probabilities")))[4]
L5=as.integer(colnames( attr(kaggle_p,"probabilities")))[5]

probs=matrix(0, length(kaggle_p), 5)
probs[,L1]=attr(kaggle_p,"probabilities")[,1]
probs[,L2]=attr(kaggle_p,"probabilities")[,2]
probs[,L3]=attr(kaggle_p,"probabilities")[,3]
probs[,L4]=attr(kaggle_p,"probabilities")[,4]
probs[,L5]=attr(kaggle_p,"probabilities")[,5]
output=cbind(kaggle$obs,probs)
write.csv(output,"out.csv")

