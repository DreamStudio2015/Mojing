###############################################################################
####################-----------  Kesci-Mojing   -------------##################
###############################################################################
# reference:
# [1] http://mojing.ppdai.com/
#
# Author: Zhu Davy
# E-mail: dreamstudio@vip.163.com
# Date  : Apri. 2nd 2016
###

### Load Training Data ----
vData.logInfo <- read.csv(file="Input/Training Set/PPD_LogInfo_3_1_Training_Set.csv")
vData.master <- read.csv(file="Input/Training Set/PPD_Training_Master_GBK_3_1_Training_Set.csv")
vData.UserInfoEx <- read.csv(file="Input/Training Set/PPD_Userupdate_Info_3_1_Training_Set.csv")
col.factor <- c(2:5,24:28,30:36,38:54,210,211,216,221,227)
for(i in col.factor){
  vData.master[,i]<- as.factor(vData.master[,i]) 
}
#save(vData.master,vData.logInfo,vData.UserInfoEx,file="RData/vData.origin.RData")
load(file="RData/vData.origin.RData")

### Load Test Data ----
vTData.logInfo <- read.csv(file="/Volumes/studio/Kesci-Mojing/Mojing/Input/Test Set/PPD_LogInfo_2_Test_Set.csv")
vTData.master <- read.csv(file="/Volumes/studio/Kesci-Mojing/Mojing/Input/Test Set/PPD_Master_GBK_2_Test_Set.csv")
vTData.UserInfoEx <- read.csv(file="Input/Test Set/PPD_Userupdate_Info_2_Test_Set.csv")
for(i in col.factor){ # col.factor is the same as Training data
  vTData.master[,i]<- as.factor(vTData.master[,i]) 
}
#save(vTData.master,vTData.UserInfoEx,vTData.logInfo,file="RData/vTData.origin.RData")
load(file="RData/vTData.origin.RData")
