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

### Load Training Data ----
vData.logInfo <- read.csv(file="Input/Training Set/PPD_LogInfo_3_1_Training_Set.csv")
vData.master <- read.csv(file="Input/Training Set/PPD_Training_Master_GBK_3_1_Training_Set.csv")
vData.UserInfoEx <- read.csv(file="Input/Training Set/PPD_Userupdate_Info_3_1_Training_Set.csv")
#save(vData.master,vData.logInfo,vData.UserInfoEx,file="RData/vData.origin.RData")
load(file="RData/vData.origin.RData")

### Stat for Na data ----
num.na <- c(id=0,n=0,ratio=0)
for(i in 2:M){
  n <- length(vData.master[is.na(vData.master[,i]),i])
  ratio <- signif(n/N,10)
  num.na <- rbind(num.na,c(id=i,n,ratio))
}
num.na <- num.na[-1,]
head(num.na)
num.na[num.na[,"ratio"]>0.06,]
names(vData.master[1,])[c(6,8,30,31,32)]

head(num.na)
num.na[num.na[,"n"]>0,]

### Clean the data
vData.master.ex <- as.data.frame(vData.master[,-c(6,8,30,31,32)])
colId.na <- num.na[num.na[,"n"]>0&num.na[,"ratio"]<=0.06,"id"]
rowId.com <- complete.cases(vData.master.ex[,colId.na])
vData.master.ex <- vData.master.ex[rowId.com,]

for(i in 2:M){
  n <- length(vData.master[is.na(vData.master[,i]),i])
  ratio <- signif(n/N,10)
  num.na <- rbind(num.na,c(id=i,n,ratio))
}

for(i in names(vData.master.ex[1,])){
  n <- length(vData.master.ex[is.na(vData.master.ex[,i]),i])
  if(n>0)print(c(n,i))
}
vData.master.ex[is.na(vData.master.ex[,"WeblogInfo_2"]),"WeblogInfo_2"]

length(vData.master.ex[,1])

length(vData.master[is.na(vData.master[,i]),i])

length(vData.master.ex[1,])
head(vData.master.ex)
str(vData.master.ex)


vLabel <- "target"

### UserInfo_1/UserInfo_2/UserInfo_3/UserInfo_4
# "UserInfo_1" ----
vAttr <- "UserInfo_1"
woe(vData.master,"UserInfo_1",TRUE,"target",3,Bad=1,Good=0)

woe.part(vData.master,vTag=vTag,vPart.name=vData.master[,""])

vPart.tmp <- cbind(v=vData.master[,vAttr],part=0)
head(vPart.tmp)
i=1
vPart.names <- as.numeric(names(table(vPart.tmp[,"v"])))
vPart.tmp[vPart.tmp[,"v"]==vPart.names[i],"v"]

table()

N <- length(vData.master[,1])
M <- length(vData.master[1,])
vPart.tmp[is.na(vPart.tmp[,"v"]),]





vPart.tmp[vPart.tmp[,"v"]>=66.04,"part"] <- 2
vPart.tmp[vPart.tmp[,"v"]<66.04,"part"] <- 1
vPart <- vPart.tmp[,"part"]
vPart <- as.factor(vPart)
vData.ex <- cbind(vData,part=vPart)
vPart.name <- levels(vPart)
res <- woe.part(vData.ex,vTag,vPart.name)
res
sum(res$woe.res[,"IV"])

woe(vData.master[,"target"])
table(vData.master[,"UserInfo_1"])

vData.master[,c("UserInfo_1","target")]

head(vData.logInfo)
head(vData.master)
head(vData.UserInfoEx)
names(vData.logInfo)
names(vData.master)
names(vData.UserInfoEx)

ncol(vData.master)
ncol(vData.logInfo)
ncol(vData.UserInfoEx)

sort(table(vData.master[,1])[1:30],decreasing=FALSE)[1:10]

i <- 3
table(vData.master[vData.master[,i]==-999,])

vData.master[,]


for(i in 211:230){
  print(i)
  print(names(table(vData.master[,i])[1:10]))
}

vData.master[,228]

write.csv(vData.master,file="Output/test.csv",fileEncoding="UTF-8")
enc2utf8(vData.master[,1])
str(vData.master)


table(vData.master[,"target"])
