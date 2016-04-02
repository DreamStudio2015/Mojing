##draw a circle
country<-c("老挝","菲律宾","尼泊尔","孟加拉","阿富汗","新加坡","泰国","蒙古","新西兰","印度尼西亚","印度","澳大利亚","台湾","日本","香港","柬埔寨","马来西亚","巴基斯坦","韩国","斯里兰卡","中国","越南")
percent<-c(90,81,80,77,75,74,73,72,68,68,68,67,65,63,61,60,59,58,53,51,49,48)
d<-data.frame(country,percent)
png("Output/test.png",width = 2048, height = 2048)

f<-function(name,value){
  xsize=200
  plot(0, 0,xlab="",ylab="",axes=FALSE,xlim=c(-xsize,xsize),ylim=c(-xsize,xsize))
  for(i in 1:length(name)){
    info = name[i]
    percent = value[i]
    k = (1:(360*percent/100)*10)/10
    r=xsize*(length(name)-i+1)/length(name)
    #print(r)
    x=r*sin(k/180*pi)
    y=r*cos(k/180*pi)
    text(-18,r,info,pos=2,cex=3)
    text(-9,r,paste(percent,"%"),cex=3)
    lines(x,y,col="red")
  }
}

f(country,percent)
dev.off()
f("",100)