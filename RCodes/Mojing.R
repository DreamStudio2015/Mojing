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
head(vData.UserInfoEx)
