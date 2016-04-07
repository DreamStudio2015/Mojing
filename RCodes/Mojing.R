###############################################################################
####################---------  Kesci-Mojing main  -----------##################
###############################################################################
# reference:
# [1] http://mojing.ppdai.com/
#
# Author: Zhu Davy
# E-mail: dreamstudio@vip.163.com
# Date  : Apri. 2nd 2016
###

### Configuration ----
source("RCodes/MJ_facilities.R")

### Load Data ----
load(file="RData/vData.origin.RData")
load(file="RData/vTData.origin.RData")

### Public Info ----
M <- 228
N <- 30000

### Data Process ----
source("RCodes/MJ_dataProcess.R")

#vData.master.ex.1 <- vData.master.ex[vData.master.ex[,"target"]==1,]
# name.tmp <- names(vData.master.ex.1)[sapply(vData.master.ex.1, class)=="factor"]
# for(i in name.tmp){
#   print(table(vData.master.ex.1[,i]))
# }

stat.target <- table(vData.master.ex[,"target"])
vData.master.ex.1<- vData.master.ex[vData.master.ex[,"target"]==1,]
vData.master.ex.0 <- vData.master.ex[vData.master.ex[,"target"]==0,]
ind.1 <- sample(2,stat.target["1"],replace=TRUE,prob=c(0.75,0.25))
ind.0 <- sample(2,stat.target["0"],replace=TRUE,prob=c(0.75,0.25))

vData.master.ex.training <- rbind(vData.master.ex.1[ind.1==1,],vData.master.ex.0[ind.0==1,])
vData.master.ex.test <- rbind(vData.master.ex.1[ind.1==2,],vData.master.ex.0[ind.0==2,])

### Classification of RandomForest ----
rm(vData.master,vData.logInfo,vData.UserInfoEx,
   vTData.master,vTData.logInfo,vTData.UserInfoEx)
#subset.rf <- which(vData.master.ex[,"target"]==1)
set.seed(71)
ptime <- Sys.time()
mojing.rf <- randomForest(target ~ ., data=vData.master.ex.training,mtry=8,na.action=na.omit,
                          importance=TRUE)
difftime(Sys.time(),ptime)
save(mojing.rf,file="Output/mojing.res2.RData")

#save(mojing.rf,file="Output/mojing.res1.RData")
# 1.68554 hours
#save(mojing.rf,file="Output/mojing.res.RData")

class(vData.master.ex[,"target"])
str(mojing.rf)


mojing.pred <- predict(mojing.rf,vData.master.ex.test[,-198])

auc.val <- auc(roc(vData.master.ex.test$target,mojing.pred))
auc.text <- paste("AUC:",round(auc.val[1]*100,1))
plot(roc(vData.master.ex.test$target,mojing.pred),col=10,main=auc.text)


length(vData.master.ex[1,])

length(mojing.pred)
length(vData.master.ex$target)

summary(mojing.pred)
length(mojing.pred[mojing.pred>0.5&!is.na(mojing.pred)])

### Model Evaluation ----
## Training Set Auccuracy and Recall
Acc.rf <- round((vConf.rf[1,1]+vConf.rf[2,2])/N,2)
Rec.rf <- round(vConf.rf[2,2]/(sum(vConf.rf[2,1:2])),2)
# AUC
auc.val <- auc(roc(vData.master.ex$target,as.numeric(mojing.rf$predicted)))
auc.text <- paste("AUC:",round(auc.val[1]*100,1))
plot(roc(vData.master.ex$target,as.numeric(mojing.rf$predicted)),col=10,main=auc.text)

## Test Set
rm(vData.master,vData.logInfo,vData.UserInfoEx,vData.master.ex,
   vTData.master,vTData.logInfo,vTData.UserInfoEx)
load(file="Output/mojing.res.RData")

mojing.pred <- predict(mojing.rf,vTData.master.ex)
table(mojing.pred)

### Result analysis ----
load(file="Output/mojing.res.RData")
importance.rf <- round(importance(mojing.rf), 2)
col.key.0 <- importance.rf[,"0"]
col.key.1 <- importance.rf[,"1"]
max(col.key)*
  
col.key.0 <- col.key.0[sort(col.key.0,decreasing=TRUE)<=0]
col.key.1 <- col.key.1[sort(col.key.1,decreasing=TRUE)<=0]
col.noise <- intersect(names(col.key.0),names(col.key.1))
save(col.noise,file="RData/col.noise.RData")

str(mojing.rf)
vConf.rf <- mojing.rf$confusion

