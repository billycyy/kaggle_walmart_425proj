#######################################################################
#Stat 425 Project Code
#Written by Yiyun Chen
#1,Please first set the work directory and make sure all the csv files are in the path
#2,Then make sure the following packages are installed
# lubridate randomForest foreach doParallel forecast
#3,Set the cores used by CPU in the randomforest model in around line 210
#by using registerDoParallel(cores=8)
#This code will automatically generate 5 jpeg files and 5 csv files(for submission)
#The whole run time is about 15 minutes
#pre process#############################################################
rm(list=ls())
setwd('C:/think/stat425proj') #Please change the path here

rawtrain=read.csv("train.csv")
rawstore=read.csv("stores.csv")
rawtest=read.csv("test.csv")

train=merge(x=rawtrain,y=rawstore,all.x=TRUE)
test=merge(x=rawtest,y=rawstore,all.x=TRUE)
names(train)=c("store","dept","date","sales","isholiday","type","size")
names(test)=c("store","dept","date","isholiday","type","size")

train$type=as.character(train$type)
train$type[train$type=="A"]=1
train$type[train$type=="B"]=2
train$type[train$type=="C"]=3
train$isholiday[train$isholiday=="TRUE"]=1
train$isholiday[train$isholiday=="FALSE"]=0

test$type=as.character(test$type)
test$type[test$type=="A"]=1
test$type[test$type=="B"]=2
test$type[test$type=="C"]=3
test$isholiday[test$isholiday=="TRUE"]=1
test$isholiday[test$isholiday=="FALSE"]=0

rm(rawtrain,rawtest,rawstore)

#summary#######################################################################

dstrain=table(train$dept,train$store)
dstest=table(test$dept, test$store)
dim(dstrain)
dim(dstest)
deptname=sort(unique(train$dept))
nstore=length(unique(test$store))
ndept=length(deptname)
missings=NULL
for (i in 1:ndept){
  for (j in 1:nstore){
    if ((dstest[i,j])>0 & dstrain[i,j]==0){
      missings=rbind(missings,cbind(deptname[i],j,which(test$dept==deptname[i] & test$store==j)))
    }
  }
}
colnames(missings)=c("dept","store","id in test")
missings
jpeg("firstlook.jpeg")
par(mfrow=c(3,3))
for (i in 1:5){
  plot.ts(train$sales[which(train$dept==1 & train$store==i)],ylab='weeksales',main=paste("Dept 1","Store",i))
}
for (i in 1:4){
  plot.ts(train$sales[which(train$dept==20 & train$store==i)],ylab='weeksales',main=paste("Dept 20","Store",i))
}
dev.off()
par(mfrow=c(1,1))
library(lubridate)
train$year=year(train$date)
train$month=month(train$date)
train$mday=day(train$date)
train$yday=yday(train$date)
train$wk=week(train$date)
train$interholiday=train$isholiday*train$yday
test$year=year(test$date)
test$wk=week(test$date)
test$month=month(test$date)
test$mday=day(test$date)
test$yday=yday(test$date)
test$interholiday=test$isholiday*test$yday

unique(train$wk[which(train$isholiday==1)])
unique(test$wk[which(test$isholiday==1)])
max(unique(train$wk[which(train$year<2012)]))
max(unique(test$wk[which(test$year==2012)]))
max(unique(test$wk[which(test$year==2013)]))

train$wk[which(train$year==2012)]=week(train$date[which(train$year==2012)])+1
test$wk=test$wk+1

week1=week("2010-02-05")

#simple model # 3 minutes run time # score 2750.71851 # rank 38 ############################
mypred=rep(NA,nrow(test))
for(i in 1:ndept){
  print(paste("dept",i)) 
  for(j in 1:nstore){
    if (dstest[i,j]==0 | dstrain[i,j]==0) next
    
    trainid=which(train$dept==deptname[i] & train$store==j)
    trainsale=train$sales[trainid] 
    testid=which(test$dept==deptname[i] & test$store==j)
    cumutrainwk=(train$year[trainid]-2010)*53+train$wk[trainid]-week1+1
    cumutestwk=(test$year[testid]-2010)*53+test$wk[testid]-week1+1      
    trainsale=rep(NA, max(cumutrainwk))
    trainsale[cumutrainwk]=train$sales[trainid];
    tmppred=trainsale[cumutestwk-53]
    tmppred[is.na(tmppred)]=0;
    mypred[testid]=tmppred;  
    
    chid=rep(0,5)
    chpred=rep(NA,5)
    for (l in 1:5){
      tempid=which(test$dept==deptname[i] & test$store==j & test$wk==(53-5+l));
      if(length(tempid)>0){
        chid[l]=tempid;
        chpred[l]=mypred[chid[l]]}
    }
    mean5=mean(chpred,na.rm=TRUE)
    mean3=mean(chpred[2:4],na.rm=TRUE)
    chpred[is.na(chpred)]=0;
    if(is.finite(mean3/mean5)&(mean3/mean5>1.1)){
      newpred=5/7*chpred;
      newpred[2:5]=newpred[2:5]+2/7*chpred[1:4]
      newpred[1]=chpred[1]    
      for (l in 1:5){
        if (chid[l] != 0) {mypred[chid[l]]=newpred[l]}
      }
    }
  }
}
mypred[is.na(mypred)]=0;

filepath = "cyysimple.csv";
ID=apply(test[,c("store", "dept", "date")], 1, function(x)  
  paste(x[1],  x[2],  as.character(x[3]), sep='_'))
ID = gsub("\\s","", ID)
myout = data.frame(ID=ID, Weekly_Sales = mypred)
write.csv(myout, file=filepath,  row.names=FALSE, quote = FALSE);

#linear model # 3 minutes run time # score 3108.14737 # rank 208 ####################################

mypred=rep(NA,nrow(test))
for(i in 1:ndept){
  print(paste("dept",i))
  outid= which(train$dept==deptname[i]) 
  if (length(unique(as.factor(train$store[outid])))==1){
    stepmod = step(lm((log(sales+5000))~1, data=train[outid,]), list(upper=lm(log(sales+5000)~isholiday
    +as.factor(wk), data=train[outid,])),trace=0, direction="forward", k=log(length(outid)));   
  } else { 
    stepmod = step(lm((log(sales+5000))~1, data=train[outid,]), list(upper=lm(log(sales+5000)~as.factor(store)
    +isholiday+as.factor(wk), data=train[outid,])),trace=0, direction="forward", k=log(length(outid)));                 
  }
    
  for(j in 1:nstore){
    if (dstest[i,j]==0 | dstrain[i,j]==0) next    
        
    inid=which(train$dept==deptname[i] & train$store==j)
    testid = which(test$dept==deptname[i] & test$store==j)
    newdata=test[testid,]
    if (dstrain[i,j]<30){       
      canpreid=which(newdata$wk %in% as.numeric(train$wk[outid]))
      zhenid=testid[canpreid]
      mypred[zhenid]=exp(predict(stepmod,newdata=test[zhenid,]))-5000
    } else {
      stepmodin=lm(log(sales+5000)~isholiday+as.factor(wk), data=train[inid,]);                 
      canpreid=which(newdata$wk %in% as.numeric(train$wk[inid]))
      zhenid=testid[canpreid]
      mypred[zhenid]=exp(predict(stepmodin,newdata=test[zhenid,]))-5000
    }
    
    chid=rep(0,5)
    chpred=rep(NA,5)
    for (l in 1:5){
      tempid=which(test$dept==deptname[i] & test$store==j & test$wk==(53-5+l));
      if(length(tempid)>0){
        chid[l]=tempid;
        chpred[l]=mypred[chid[l]]}
    }
    mean5=mean(chpred,na.rm=TRUE)
    mean3=mean(chpred[2:4],na.rm=TRUE)
    chpred[is.na(chpred)]=0;
    if(is.finite(mean3/mean5)&(mean3/mean5>1.1)){
      newpred=5/7*chpred;
      newpred[2:5]=newpred[2:5]+2/7*chpred[1:4]
      newpred[1]=chpred[1]    
      for (l in 1:5){
        if (chid[l] != 0) {mypred[chid[l]]=newpred[l]}
      }
    }

  }
}
mypred[is.na(mypred)]=0;

filepath = "cyylinear.csv";
ID=apply(test[,c("store", "dept", "date")], 1, function(x)  
  paste(x[1],  x[2],  as.character(x[3]), sep='_'))
ID = gsub("\\s","", ID)
myout = data.frame(ID=ID, Weekly_Sales = mypred)
write.csv(myout, file=filepath,  row.names=FALSE, quote = FALSE);


#random forest # 5 minutes run time # score 2747.65791 # rank 37 ########################
library(randomForest)
library(foreach)
library(doParallel) 
registerDoParallel(cores=8)

mypred=rep(NA,nrow(test))
out <- foreach(i=1:ndept,.combine='rbind',.export=c("randomForest")) %dopar% {
  outid= which(train$dept==deptname[i])
  temptrain=train[outid,];
  rfmod =randomForest(log(sales+5000) ~ size+wk+type+year+month+mday+yday+interholiday,ntree=300,mtry=3,replace=TRUE,data=temptrain)
  tempout=NULL  
  for(j in 1:nstore){
    if (dstest[i,j]==0 | dstrain[i,j]==0) next   
    
    testid = which(test$dept==deptname[i] & test$store==j)
    newdata=test[testid,]
    deepid=which(train$dept==deptname[i] & train$store==j);
    if (length(deepid)> 10) {
      newtemp=train[deepid,]
      newmod=randomForest(log(sales+5000) ~ wk+year+month+mday+yday+interholiday,ntree=300,mtry=3,replace=TRUE,data=newtemp)
      tmppred=exp(predict(newmod,newdata))-5000
      mypred[testid]=tmppred
    } else {
      tmppred=exp(predict(rfmod,newdata))-5000
      mypred[testid]=tmppred
    }
        
    chid=rep(0,5)
    chpred=rep(NA,5)
    for (l in 1:5){
      tempid=which(test$dept==deptname[i] & test$store==j & test$wk==(53-5+l));
      if(length(tempid)>0){
        chid[l]=tempid;
        chpred[l]=mypred[chid[l]]}
    }    
    mean5=mean(chpred,na.rm=TRUE)
    mean3=mean(chpred[2:4],na.rm=TRUE)
    chpred[is.na(chpred)]=0;    
    if(is.finite(mean3/mean5)&(mean3/mean5>1.1)){
      newpred=4.5/7*chpred;
      newpred[2:5]=newpred[2:5]+2.5/7*chpred[1:4]
      newpred[1]=chpred[1]      
      for (l in 1:5){
        if (chid[l] != 0) {mypred[chid[l]]=newpred[l]}
      }
    }
    tempout <-rbind(tempout,cbind(testid,mypred[testid]))
      
  }
  tempout
  
}
mypred[out[,1]]=out[,2];
mypred[is.na(mypred)]=0

filepath = "cyyrf.csv";
ID=apply(test[,c("store", "dept", "date")], 1, function(x)  
  paste(x[1],  x[2],  as.character(x[3]), sep='_'))
ID = gsub("\\s","", ID)
myout = data.frame(ID=ID, Weekly_Sales = mypred)
write.csv(myout, file=filepath,  row.names=FALSE, quote = FALSE);


#Seasonal Decomposition of Time Series by Loess # 3 minutes run time # score 2388.58667 # rank 3 #####
library(forecast)

mypred=rep(NA,nrow(test))
for(i in 1:ndept){
  print(paste("dept",i))  
  for(j in 1:nstore){
    if (dstest[i,j]==0 | dstrain[i,j]==0) next    
    
    trainid=which(train$dept==deptname[i] & train$store==j)
    testid=which(test$dept==deptname[i] & test$store==j)
    cumutrainwk=(train$year[trainid]-2010)*53+train$wk[trainid]-week1+1
    trainsale=rep(0,144)
    trainsale[cumutrainwk]=train$sales[trainid];
    cumutestwk=(test$year[testid]-2010)*53+test$wk[testid]-week1
    temptrain=log(trainsale+5000)  
    series <- ts(temptrain, frequency=53)    
    lenpred=max(cumutestwk)-144    
    stlfpred <- stlf(series,h=lenpred,s.window=3,method='ets',ic='bic',opt.crit='mae')
    tmppredall=as.numeric(stlfpred$mean)
    tmppred=tmppredall[(cumutestwk-144)]
    mypred[testid]=exp(tmppred)-5000
    
    chid=rep(0,5)
    chpred=rep(NA,5)
    for (l in 1:5){
      tempid=which(test$dept==deptname[i] & test$store==j & test$wk==(53-5+l));
      if(length(tempid)>0){
        chid[l]=tempid;
        chpred[l]=mypred[chid[l]]}
    }    
    mean5=mean(chpred,na.rm=TRUE)
    mean3=mean(chpred[2:4],na.rm=TRUE)
    chpred[is.na(chpred)]=0;    
    if(is.finite(mean3/mean5)&(mean3/mean5>1.1)){
      newpred=4.5/7*chpred;
      newpred[2:5]=newpred[2:5]+2.5/7*chpred[1:4]
      newpred[1]=chpred[1]      
      for (l in 1:5){
        if (chid[l] != 0) {mypred[chid[l]]=newpred[l]}
      }
    }
    
  }
}
mypred[is.na(mypred)]=0;

filepath = "cyystl.csv";
ID=apply(test[,c("store", "dept", "date")], 1, function(x)  
  paste(x[1],  x[2],  as.character(x[3]), sep='_'))
ID = gsub("\\s","", ID)
myout = data.frame(ID=ID, Weekly_Sales = mypred)
write.csv(myout, file=filepath,  row.names=FALSE, quote = FALSE);

#combining model(rf+stl) # score 2366.11713 # rank 2 #################################

rfpred=read.csv("cyyrf.csv")[,2]
stlpred=read.csv("cyystl.csv")[,2]
comb=0.2*rfpred+0.8*stlpred

filepath = "cyycomb.csv";
ID=apply(test[,c("store", "dept", "date")], 1, function(x)  
  paste(x[1],  x[2],  as.character(x[3]), sep='_'))
ID = gsub("\\s","", ID)
myout = data.frame(ID=ID, Weekly_Sales = comb)
write.csv(myout, file=filepath,  row.names=FALSE, quote = FALSE);

#check the results #######################################################################


simpred=read.csv("cyysimple.csv")[,2]
linpred=read.csv("cyylinear.csv")[,2]
jpeg("result1.jpeg")
par(mfrow=c(2,2))
trainid=which(train$dept==1 & train$store==1)
cumutrainwk=(train$year[trainid]-2010)*53+train$wk[trainid]-week1+1
testid=which(test$dept==1 & test$store==1)
cumutestwk=(test$year[testid]-2010)*53+test$wk[testid]-week1
plot(cumutrainwk,train$sales[trainid],ylab='weeksales',main="Dept 1 Store 1 simple",xlim=c(0,180),type='l')
points(cumutestwk,simpred[testid],type='l',col=2)
plot(cumutrainwk,train$sales[trainid],ylab='weeksales',main="Dept 1 Store 1 linear",xlim=c(0,180),type='l')
points(cumutestwk,linpred[testid],type='l',col=3)
plot(cumutrainwk,train$sales[trainid],ylab='weeksales',main="Dept 1 Store 1 randomforest",xlim=c(0,180),type='l')
points(cumutestwk,rfpred[testid],type='l',col=4)
plot(cumutrainwk,train$sales[trainid],ylab='weeksales',main="Dept 1 Store 1 stl",xlim=c(0,180),type='l')
points(cumutestwk,stlpred[testid],type='l',col=5)
dev.off()
jpeg("result2.jpeg")
par(mfrow=c(2,2))
trainid=which(train$dept==20 & train$store==1)
cumutrainwk=(train$year[trainid]-2010)*53+train$wk[trainid]-week1+1
testid=which(test$dept==20 & test$store==1)
cumutestwk=(test$year[testid]-2010)*53+test$wk[testid]-week1
plot(cumutrainwk,train$sales[trainid],ylab='weeksales',main="Dept 20 Store 1 simple",xlim=c(0,180),type='l')
points(cumutestwk,simpred[testid],type='l',col=2)
plot(cumutrainwk,train$sales[trainid],ylab='weeksales',main="Dept 20 Store 1 linear",xlim=c(0,180),type='l')
points(cumutestwk,linpred[testid],type='l',col=3)
plot(cumutrainwk,train$sales[trainid],ylab='weeksales',main="Dept 20 Store 1 randomforest",xlim=c(0,180),type='l')
points(cumutestwk,rfpred[testid],type='l',col=4)
plot(cumutrainwk,train$sales[trainid],ylab='weeksales',main="Dept 20 Store 1 stl",xlim=c(0,180),type='l')
points(cumutestwk,stlpred[testid],type='l',col=5)
dev.off()
jpeg("result3.jpeg")
par(mfrow=c(2,2))
trainid=which(train$dept==99 & train$store==40)
cumutrainwk=(train$year[trainid]-2010)*53+train$wk[trainid]-week1+1
testid=which(test$dept==99 & test$store==40)
cumutestwk=(test$year[testid]-2010)*53+test$wk[testid]-week1
plot(cumutrainwk,train$sales[trainid],ylab='weeksales',main="Dept 99 Store 40 simple",xlim=c(0,180),ylim=c(0,3000))
points(cumutestwk,simpred[testid],col=2)
plot(cumutrainwk,train$sales[trainid],ylab='weeksales',main="Dept 99 Store 40 linear",xlim=c(0,180),ylim=c(0,3000))
points(cumutestwk,linpred[testid],col=3)
plot(cumutrainwk,train$sales[trainid],ylab='weeksales',main="Dept 99 Store 40 randomforest",xlim=c(0,180),ylim=c(0,3000))
points(cumutestwk,rfpred[testid],col=4)
plot(cumutrainwk,train$sales[trainid],ylab='weeksales',main="Dept 99 Store 40 stl",xlim=c(0,180),ylim=c(0,3000))
points(cumutestwk,stlpred[testid],col=5)
dev.off()
jpeg("result4.jpeg")
par(mfrow=c(2,2))
trainid=which(train$dept==99 & train$store==37)
cumutrainwk=(train$year[trainid]-2010)*53+train$wk[trainid]-week1+1
testid=which(test$dept==99 & test$store==37)
cumutestwk=(test$year[testid]-2010)*53+test$wk[testid]-week1
plot(cumutrainwk,train$sales[trainid],ylab='weeksales',main="Dept 99 Store 37 simple",xlim=c(0,180),ylim=c(0,3000))
points(cumutestwk,simpred[testid],col=2)
plot(cumutrainwk,train$sales[trainid],ylab='weeksales',main="Dept 99 Store 37 linear",xlim=c(0,180),ylim=c(0,3000))
points(cumutestwk,linpred[testid],col=3)
plot(cumutrainwk,train$sales[trainid],ylab='weeksales',main="Dept 99 Store 37 randomforest",xlim=c(0,180),ylim=c(0,3000))
points(cumutestwk,rfpred[testid],col=4)
plot(cumutrainwk,train$sales[trainid],ylab='weeksales',main="Dept 99 Store 37 stl",xlim=c(0,180),ylim=c(0,3000))
points(cumutestwk,stlpred[testid],col=5)
dev.off()