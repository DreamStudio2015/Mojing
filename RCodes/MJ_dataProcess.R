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

### Public Info ----
M <- 228
N <- 30000

### Data Analysis ----
# Stat for Na data
num.na <- c(id=0,n=0,ratio=0)
for(i in 2:M){
  n <- length(vData.master[is.na(vData.master[,i]),i])
  ratio <- signif(n/N,10)
  num.na <- rbind(num.na,c(id=i,n,ratio))
}
num.na <- num.na[-1,]
# head(num.na)
# num.na[num.na[,"ratio"]>0.1,]

# Get the category columns with more than 53 categories
vType.col <- sapply(vData.master, class)
vBigCategory <- c(idx=0,n=0)
for(i in which(vType.col=="factor")){
  n <- length(levels(vData.master[,i]))
  if(n>53){vBigCategory <- rbind(vBigCategory,c(i,n))}
}
vBigCategory <- vBigCategory[-1,]
# Show the big category columns
#cbind(colNames=names(vData.master.ex[1,vBigCategory[,"idx"]]),vBigCategory)

### Clear the data ----
# Remove columns with more than 10% null values
# Remove columns with more than 53 categories
# Remove the noise columns
# num.na[num.na[,"ratio"]>0.1,][,"id"]   c(6,8,30,31,32)
col.null <- names(vData.master[1,])[num.na[num.na[,"ratio"]>0.1,][,"id"]]
col.bigCategory <- names(vData.master[1,vBigCategory[,"idx"]])
col.remove <- c("idx",union(col.null,col.bigCategory))
load(file="RData/col.noise.RData")
col.remove <- union(col.remove,col.noise)
vData.master.ex <- as.data.frame(vData.master[,setdiff(names(vData.master),col.remove)])
vTData.master.ex <- as.data.frame(vTData.master[,setdiff(names(vTData.master),col.remove)])

### Adjust the Test data ----
vTType.col <- sapply(vTData.master.ex, class)
idx.diff=integer()
vDiffCategory <- list()
for(i in which(vTType.col=="factor")){
  lev <- levels(vData.master.ex[,i])
  lev.t <- levels(vTData.master.ex[,i])
  res.tmp <- setdiff(lev.t,lev)
  if(length(res.tmp)!=0){
    idx.diff <- append(idx.diff,i)
    vDiffCategory <- append(vDiffCategory,list(set.diff=res.tmp))
  }
}
#idx.diff;vDiffCategory
# replace the unknown factor variables' levels with common one
ncount <- 0
for(i in idx.diff){
  ncount <- ncount + 1
  for(j in unlist(vDiffCategory[ncount])){
    levels(vTData.master.ex[,i])[levels(vTData.master.ex[,i]) %in% j] <- 
      names(which.max(table(vData.master.ex[,i])))
  }
}
# make sure the levels are the same as Training data
for(i in which(vTType.col=="factor")){
  levels(vTData.master.ex[,i]) <- levels(vData.master.ex[,i])
}