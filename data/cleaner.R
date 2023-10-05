library(tidyverse)
log.return <- function(x){
  c(NA, diff(log(x)))
}
readDatePrice =  function(csvFilePath) {
  fileData = read_csv(
    csvFilePath
  )
  date = rev(fileData$date)[-c(1)]
  closePrice = rev(fileData$closePrice)
  logReturn = log.return(closePrice)[-c(1)]
  return (data.frame(date, logReturn))
}
filenames = c("huShen.csv",
              "huShenSmall.csv",
              "shangZheng.csv",
              "shangZhengSmall.csv")
for (filename in filenames) {
  data = readDatePrice(filename)
  write.csv(data, paste("c_", filename, sep=""), row.names=FALSE)
}