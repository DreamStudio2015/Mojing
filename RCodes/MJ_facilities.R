###############################################################################
######################---------  Kesci-Mojing  -----------#####################
###############################################################################
# reference:
# [1] http://mojing.ppdai.com/
#
# Author: Zhu Davy
# E-mail: dreamstudio@vip.163.com
# Date  : Apri. 2nd 2016
###

### Load libraries
library(randomForest)
library(woe)
library(pROC)
# library(RMySQL)
# library(plyr)
# library(ROCR)
# library(ineq)

### ftopk
# function to pick top 3 reasons
# works by sorting coefficient terms in equation
# and selecting top 3 in sort for each loan scored
ftopk<- function(x,top=3){
res=names(x)[order(x, decreasing = TRUE)][1:top]
paste(res,collapse=";",sep="")
}

### woe.part
# calculate the woe according to different parts
woe.part <- function (vData.ex=vData.ex,vTag=vTag,vPart.name=vPart.name) {
vTab <- table(vData.ex[,vTag])
vList.all <- replicate(8,0)
for(i in as.numeric(vPart.name)){
  #i <- vPart.name[2]
  vG.B.num <- c(0,0)
  vG.B.Ratio <- c(0,0)
  names(vG.B.num) <- c("0","1")
  names(vG.B.Ratio) <- c("0","1")
  vNum <- table(vData.ex[vData.ex[,"part"]==i,"label.ex"])
  vG.B.num[names(vNum)] <- vNum
  vG.B.Ratio[names(vG.B.num)] <- vG.B.num/vTab[names(vG.B.num)]
  vWOE <- round(log(vG.B.Ratio["1"]/vG.B.Ratio["0"]),3)
  vIV <- round((vG.B.Ratio["1"]-vG.B.Ratio["0"])*vWOE,3)
  vList <- cbind(t(vG.B.num),sum(vG.B.num),round(t(vG.B.Ratio),3),
                 round(sum(vG.B.num)/sum(vTab),3),vWOE,vIV)
  vList.all <- rbind(vList.all,vList)
}
colnames(vList.all) <- c("Bad","Good","Total","B%","G%","T%","WOE","IV")
woe.res <- vList.all[-1,]
# SCORE
raw.score <- (vList.all[-1,"WOE"] - min(vList.all[-1,"WOE"]))*10
woe.res <- cbind(woe.res,raw.score)
rownames(woe.res) <- vPart.name
return(res=list(woe.res=woe.res))
}

### Give a normal interval estimation for a sequence
normEstimate <- function (x,nSigma=2) {
v.mean <- mean(x)
v.sigma <- sd(x)
low.bounder <- v.mean-v.sigma*nSigma
upper.bounder <- v.mean+v.sigma*nSigma
res=round(c(low=low.bounder,expectation=v.mean,upper=upper.bounder))
}

### scoreMachine
scoreMachine <- function(){
#----
mData.score <- cbind(mData,score=0)
vN <- 7
vAttr <- c("ratio_early_pay","avg_days_overdue","ratio_overdue_bill_count","amount_overdue_bill",
           "ratio_overdue_amount","days_max_overdue","amount_max_overdue_bill")
vScore <- list(c("22.4","29.1","27.8"),
               c("22.4","7.2","0"),
               c("22.4","31.9","27.9","21.3"),
               c("22.4","19.4","19.2"),
               c("22.4","30","26.3"),
               c("22.4","15.2"),
               c("22.4","24.5","52.6"))
vIntercept <- 
#----
#----
mData.score <- cbind(mData,score=0)
vN <- 7
vAttr <- c("ratio_early_pay","avg_days_overdue","ratio_overdue_bill_count","amount_overdue_bill",
           "ratio_overdue_amount","days_max_overdue","amount_max_overdue_bill")
vScore <- list(c("24.7","29.5","24.2"),
               c("24.7","6","0"),
               c("24.7","32.8","29.6","21"),
               c("24.7","20.9","22.7"),
               c("24.7","32.4","28.8"),
               c("24.7","16.2"),
               c("24.7","27.1","54.6"))
#----
#----
mData.score <- cbind(mData,score=0)
vN <- 7
vAttr <- c("ratio_early_pay","avg_days_overdue","ratio_overdue_bill_count","amount_overdue_bill",
           "ratio_overdue_amount","days_max_overdue","amount_max_overdue_bill")
vScore <- list(c("27.1","33.9","21.6"),
               c("27.1","37","29.7"),
               c("27.1","9.6","6.6","0"),
               c("27.1","23.8","24"),
               c("27.1","35.1","31.1"),
               c("27.1","20.1"),
               c("27.1","28.6","49"))
#----

#----
mData.score <- cbind(mData,score=0)
vN <- 4
vAttr <- c("ratio_early_pay","ratio_overdue_amount","days_max_overdue","amount_max_overdue_bill")
vScore <- list(c("16.8","24.8","21.1"),
               c("16.8","18.9","12.4"),
               c("16.8","0"),
               c("16.8","20.5","42"))
#----
#----
mData.score <- cbind(mData,score=0)
vN <- 4
vAttr <- c("ratio_early_pay","ratio_overdue_amount","days_max_overdue","amount_max_overdue_bill")
vScore <- list(c("15.3","24","23.9"),
               c("15.3","19.2","11.3"),
               c("15.3","0"),
               c("15.3","18.1","39.9"))

#----
mData.score <- cbind(mData,score=0)
vN <- 5
vAttr <- c("ratio_early_pay","avg_days_overdue","ratio_overdue_bill_count","amount_overdue_bill",
           "amount_max_overdue_bill")
vScore <- list(c("26.6","33.9","28.7"),
               c("26.6","36.3","23.7"),
               c("26.6","10.8","7","0"),
               c("26.6","25.5","23.6"),
               c("26.6","28.2","49.3"))
#----

for(i in 1:vN){
  mData.score[,vAttr[i]] <- factor(mData.score[,vAttr[i]],labels=vScore[[i]])
  mData.score[,vAttr[i]] <- as.numeric(as.character(mData.score[,vAttr[i]]))
}
intervals <- 30
mData.score[,"score"] <- rowSums(mData.score[,vAttr])+intercept
mData.ks <- mData.score[,c("label.ex","score")]
label.cut <- cut(mData.ks[,"score"],intervals)
#label.cut <- cut(mData.ks[,"score"],breaks=12:20*10)
mData.ks <- cbind(mData.ks,label.cut=label.cut)
# ks
ks.res <- ks.test(mData.ks[mData.ks[,"label.ex"]==0,"score"], mData.ks[mData.ks[,"label.ex"]==1,"score"])
ks.text <- paste("K-S Index:",round(ks.res$statistic*100,1))
plot(ecdf(mData.ks[mData.ks[,"label.ex"]==0,"score"]),xlab="Score",main=ks.text)
plot(ecdf(mData.ks[mData.ks[,"label.ex"]==1,"score"]),add = TRUE, lty = "dashed",col=10)

# Gini
mData.bad.tab <- table(mData.ks[mData.ks[,"label.ex"]==0,"label.cut"])
mData.bad.cumsum <- cumsum(mData.bad.tab)
mData.bad.cumsum <- mData.bad.cumsum/max(mData.bad.cumsum)
mData.good.tab <- table(mData.ks[mData.ks[,"label.ex"]==1,"label.cut"])
mData.good.cumsum <- cumsum(mData.good.tab)
mData.good.cumsum <- mData.good.cumsum/max(mData.good.cumsum)
mData.gini <- rbind(c(0,0),cbind(mData.bad.cumsum,mData.good.cumsum))
gini.cal <- rbind(nGood=mData.good.tab,nBad=mData.bad.tab)
gini.cal <- rbind(gini.cal,Good.ratio=gini.cal["nGood",]/sum(mData.good.tab),
                  Bad.ratio=gini.cal["nBad",]/sum(mData.bad.tab))
gini.cal <- rbind(gini.cal,cum.Good.ratio=cumsum(gini.cal["Good.ratio",]),
                  cum.Bad.ratio=cumsum(gini.cal["Bad.ratio",]))
gini.cal <- rbind(gini.cal,within=gini.cal["Good.ratio",]*gini.cal["Bad.ratio",])
gini.cal <- rbind(gini.cal,between=2*gini.cal["Bad.ratio",]*c(0,gini.cal["cum.Good.ratio",1:intervals-1]))
gini.val <- 1 - (sum(gini.cal["within",]) + sum(gini.cal["between",]))
gini.text <- paste("Gini:",round(gini.val*100,1))
plot(mData.gini,type="l",lwd=2,col=10,xlab="bad",ylab="good",main=gini.text)
lines(rbind(c(0,0),cbind(seq(0,1,1/intervals),seq(0,1,1/intervals))))

# AUC
auc.val <- auc(roc(mData.ks$label.ex,mData.ks$score))
auc.text <- paste("AUC:",round(auc.val[1]*100,1))
plot(roc(mData.ks$label.ex,mData.ks$score),col=10,main=auc.text)
}

PDO <- function(vData=mData.ks){
  head(mData.ks)
  
  label.cutEx <- cut(mData.ks[,"score"],breaks=12:20*10)
  mData.ks <- cbind(mData.ks,label.cutEx)
  table(label.cutEx)
  plot(table(label.cutEx))
  mData.ks[mData.ks[,"label.cutEx"]=="(190,200]","label.ex"]
  
  vStat.data <- (mData.ks[,c("label.ex","label.cutEx")])
  colnames(vStat.data) <- c("label.ex","part")
  levels(vStat.data[,"part"]) <- 1:8
  head(vStat.data)
  vStat.res <- woe.part(vData.ex=vStat.data,vTag="label.ex",vPart.name=1:8)
  x.stat <- 12:19*10+5
  plot(x.stat,vStat.res$woe.res[,"G%"]*100,type="o",col=10,
       xlab="Score",ylab="Ratio:%",main="Cut-off")
  points(x.stat,vStat.res$woe.res[,"B%"]*100,type="o",col=3)
  abline(v=151,lty=2,col=6)
  text(x=151,y=0.01,"151",col=6)
  legend("topright",lty=c(1,1),pch=c(1,1),col=c(10,3),legend=c("good","bad"))
  
  vPDO.data <- cbind(Score=x.stat,woe=vStat.res$woe.res[,"WOE"])
  vPDO.data <- as.data.frame(vPDO.data[vPDO.data[,"woe"]!=Inf,])
  lm <- lm(woe~Score,vPDO.data)
  plot(x.stat,vStat.res$woe.res[,"WOE"],col=10,xlab="Score",
       ylab="Ln(odds)",main="Score to Odds Plot")
  abline(lm)
  abline(h=0,lty=2,col=6)
  abline(v=151,lty=2,col=6)
  text(x=148,y=0.3,"151",col=6)
  
  intercept <- lm$coefficients["(Intercept)"]
  k.score <- lm$coefficients["Score"]
  #PDO
  PDO <- round(log(2)/k.score,2)
  
  #cut-off score;odds is 1:1
  score.cutoff <- signif(-intercept/k.score,3)
  
  PDO <- 20
  score.cutoff <- 200
  k.ex <- log(2)/PDO
  intercept.ex <- -k.ex*score.cutoff
  coef.ex <- c(intercept.ex,k.ex)
  names(coef.ex) <- c("(Intercept)","Score")
  abline(coef.ex)
  
  x.ex <- (k.score*x.stat+intercept-intercept.ex)/k.ex
  plot(x.ex,vStat.res$woe.res[,"WOE"],col=4,xlab="Score",
       ylab="Ln(odds)",main="Score Projection")
  abline(coef.ex,col=4)
  points(x.stat,vStat.res$woe.res[,"WOE"],col=10)
  abline(lm,col=10)
  abline(h=0,lty=2,col=6)
  abline(v=151,lty=2,col=6)
  text(x=140,y=0.5,"151",col=10)
  abline(v=200,lty=2,col=6)
  text(x=190,y=0.5,"200",col=4)
  legend("bottomright",lty=c(1,1),pch=c(1,1),col=c(10,4),
         legend=c("Raw Score","Projection Score"))
  
  # Score Projection ----
  # The mutiplication ratio k and the intercept from raw score to projected score
  k.prj <- k.score/k.ex
  intercept.prj <- (intercept-intercept.ex)/k.ex
  rule.score <- round((round((coe-min(coe))*10,1)*k.prj)[-1],1)
  min.score <- round(round(-min(coe)*10,1)*k.prj,1)
  rule.score;min.score
  # Raw score mapping to final score
  x1 <- 150
  x1 <- 200
  k.prj*x1+intercept.prj
  
  min.score <- 66+0+62.8+56.6+66+44.8+66-245
  max.score <- 85.8+66+94+66+88.4+66+155-245
  
}

