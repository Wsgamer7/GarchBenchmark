print("#########################################")
print("####Garch################################")
print("#########################################")
library(tidyverse)
library(xts, quietly = TRUE)
library(fGarch, quietly = TRUE)
benchInGarch = function(xtsData, dataName) {
  print(paste("DDData used:",dataName))
  mod1 <- garchFit(~ 1 + garch(1,1), xtsData, trace=FALSE)
  print(paste("#Garch(1,1) #norm #", dataName, seq=""))
  summary(mod1)

  print("----------------------------------------------------------------------------------")
  mod2 <- garchFit(~ 1 + garch(1,1), xtsData,cond.dist="snorm", trace=FALSE)
  print(paste("#Garch(1,1) #snorm #", dataName, seq=""))
  summary(mod2)

  print("----------------------------------------------------------------------------------")
  mod3 <- garchFit(~ 1 + garch(1,1), xtsData,cond.dist="std", trace=FALSE)
  print(paste("#Garch(1,1) #std #", dataName, seq=""))
  summary(mod3)

  print("----------------------------------------------------------------------------------")
  mod4 <- garchFit(~ 1 + garch(1,1), xtsData,cond.dist="sstd", trace=FALSE)
  print(paste("#Garch(1,1) #sstd #", dataName, seq=""))
  summary(mod4)
  print("-----------------------------------------")
  print("#########################################")
  print("####ENDENDENDEDN#########################")
  print("#########################################")
}
huShen = read.csv("c_huShen.csv", header = TRUE, row.names = "date")
shangZheng = read.csv("c_shangZheng.csv", header = TRUE, row.names = "date")
xts.huShen = xts(huShen, order.by=as.Date(rownames(huShen),"%Y-%m-%d"))
xts.shangZheng = xts(shangZheng, order.by=as.Date(rownames(shangZheng),"%Y-%m-%d"))
benchInGarch(xts.huShen, "huShen")
benchInGarch(xts.shangZheng, "shangZheng")


