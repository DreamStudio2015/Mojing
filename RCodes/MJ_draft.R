

library(randomForest)

str(iris.ex)

runif(5)

## Classification:
##data(iris)
set.seed(71)
iris.ex <- cbind(iris,var.ex=as.character(trunc(runif(150,min=1,max=10))))
iris.rf <- randomForest(Species ~ ., data=iris.ex, importance=TRUE,
                        proximity=TRUE)
print(iris.rf)
## Look at variable importance:
round(importance(iris.rf), 2)

names(iris.ex)

## Do MDS on 1 - proximity:
iris.mds <- cmdscale(1 - iris.rf$proximity, eig=TRUE)
op <- par(pty="s")
pairs(cbind(iris[,1:4], iris.mds$points), cex=0.6, gap=0,
      col=c("red", "green", "blue")[as.numeric(iris$Species)],
      main="Iris Data: Predictors and MDS of Proximity Based on RandomForest")
par(op)
print(iris.mds$GOF)


identical(NULL, character(0));is.null(integer(0));
str(character(0));str(NULL)
length(NULL);length(character(0))
class(NULL); class(character(0))

is.na(character(1)[1])
character(1)[1]
str("")
class("")
(""==NULL)

set.seed(111)
ind <- sample(2, 5, replace = TRUE, prob=c(0.8, 0.2))
sample(2,5)
sample(2,5,repalce=TRUE)

factor(, labels = c("ss"))
vTst <- as.factor(letters[1:20])
levels(vTst)[levels(vTst) %in% c("c")] <- "d"



vMat <- as.data.frame(matrix(1:25,nrow=5))
vMat[vMat%%9==0] <- NA
vMat
vMat[!complete.cases(vMat),]

vRow.noise <- vData.master.ex[!complete.cases(vData.master.ex),]

# Complete the missing value with median value for all non-factor columns
colID.na <- which(!complete.cases(t(vData.master.ex)))
colID.factor <- which(sapply(vData.master.ex, class)=="factor")
colID.0factor <- which(sapply(vData.master.ex, class)!="factor")
colID.complete <- intersect(colID.na,colID.0factor)
colID.complete.factor <- intersect(colID.na,colID.factor)
colName.complete <- names(vData.master.ex[1,colID.complete])
val.median <- unlist(strsplit(summary(vData.master[,colName.complete])[3,],":"))
val.median <- as.numeric(val.median[seq(2,length(colID.complete)*2,by=2)])
names(val.median) <- colName.complete

mod(vData.master[,colID.complete.factor])

for(i in colName.complete){
  vData.master.ex[which(is.na(vData.master.ex[,i])),i] <- val.median[i]
}


vData.master.ex[100,!complete.cases(t(vData.master.ex))]


length(which(!complete.cases(t(vData.master.ex))))
names(vData.master.ex)
vData.master.ex[is.na(vData.master.ex[,1]),2]

i=100
vRow.noise[i,is.na(vRow.noise[i,])]
str(vRow.noise[1,is.na(vRow.noise[1,])])

stat.weblog <- summary(vData.master[,c("WeblogInfo_2", "WeblogInfo_4", "WeblogInfo_5", "WeblogInfo_6")])
stat.weblog[3,]

str(stat.weblog)
vTmp <- unlist(strsplit(stat.weblog[3,],":"))[seq(2,8,by=2)]
vTmp[seq(2,8,by=2)]
as.numeric(vTmp$` WeblogInfo_2`[2])
