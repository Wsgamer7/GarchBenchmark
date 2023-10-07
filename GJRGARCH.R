print("#########################################")
print("####Garch################################")
print("#########################################")
library(tidyverse)
library(xts, quietly = TRUE)
library(rugarch)
benchInGarch = function(xtsData, dataName) {
  print("----------------------------------------------------------------------------------")
  print(paste("DDData used:",dataName))
  spec1 = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)), 
                     mean.model=list(armaOrder=c(0,0), include.mean=TRUE),  
                     distribution.model="norm")
  mod1 = ugarchfit(spec = spec1, data = xtsData)
  print(paste("#GJRGarch(1,1) #norm #", dataName, seq=""))
  show(mod1)
  
  
  print("----------------------------------------------------------------------------------")
  print(paste("DDData used:",dataName))
  spec2 = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)), 
                     mean.model=list(armaOrder=c(0,0), include.mean=TRUE),  
                     distribution.model="snorm")
  mod2 = ugarchfit(spec = spec2, data = xtsData)
  print(paste("#GJRGarch(1,1) #snorm #", dataName, seq=""))
  show(mod2)
  
  print("----------------------------------------------------------------------------------")
  print(paste("DDData used:",dataName))
  spec3 = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)), 
                     mean.model=list(armaOrder=c(0,0), include.mean=TRUE),  
                     distribution.model="std")
  mod3 = ugarchfit(spec = spec3, data = xtsData)
  print(paste("#GJRGarch(1,1) #std #", dataName, seq=""))
  show(mod3)
  
  
  print("----------------------------------------------------------------------------------")
  print(paste("DDData used:",dataName))
  spec4 = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)), 
                     mean.model=list(armaOrder=c(0,0), include.mean=TRUE),  
                     distribution.model="sstd")
  mod4 = ugarchfit(spec = spec1, data = xtsData)
  print(paste("#GJRGarch(1,1) #sstd #", dataName, seq=""))
  show(mod4)
  print("-----------------------------------------")
  print("#########################################")
  print("####NEXT DATA #####NEXT DATA#############")
  print("#########################################")
}
huShen = read.csv("c_huShen.csv", header = TRUE, row.names = "date")
shangZheng = read.csv("c_shangZheng.csv", header = TRUE, row.names = "date")
xts.huShen = xts(huShen, order.by=as.Date(rownames(huShen),"%Y-%m-%d"))
xts.shangZheng = xts(shangZheng, order.by=as.Date(rownames(shangZheng),"%Y-%m-%d"))
benchInGarch(xts.huShen, "huShen")
benchInGarch(xts.shangZheng, "shangZheng")

