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

### Configuration ----

### Load Training Data ----
vData.logInfo <- read.csv(file="Input/Training Set/PPD_LogInfo_3_1_Training_Set.csv")
vData.master <- read.csv(file="Input/Training Set/PPD_Training_Master_GBK_3_1_Training_Set.csv")
vData.UserInfoEx <- read.csv(file="Input/Training Set/PPD_Userupdate_Info_3_1_Training_Set.csv")

head(vData.logInfo)
head(vData.master)
head(vData.UserInfoEx)
names(vData.logInfo)
names(vData.master)
names(vData.UserInfoEx)

sort(table(vData.master[,1])[1:30],decreasing=FALSE)[1:10]

table(vData.master[,39])
for(i in 211:230){
  print(i)
  print(names(table(vData.master[,i])[1:10]))
}

vData.master[,228]

write.csv(vData.master,file="Output/test.csv",fileEncoding="UTF-8")
enc2utf8(vData.master[,1])
str(vData.master)


table(vData.master[,"target"])
