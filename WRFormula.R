AllWR <- foreach(i=1:17) %do% getNflLabModel("2015",i,mod = "Tournament",PositionID = "WR")
readWR <- foreach(i=1:17) %do% readNFLCSVs("2015",i,mod = "Tournament",PositionID = "WR")
AllWR16<- foreach(i=1) %do% getNflLabModel("2016",i,mod = "Tournament",PositionID = "WR")
readWR16 <- foreach(i=1) %do% readNFLCSVs("2016",i,mod = "Tournament",PositionID = "WR")
WR <- bind_rows(readWR16,readWR)
week <- 1
WRModel <- getNflLabModel("2016",week,PositionID = "WR")
WRModel <- na.zero(readNFLCSVs("2016",week,PositionID = "WR"))
WRnums <- na.zero(WR[,sapply(WR,is.numeric)])
WRnums <- WRnums %>% select(-X,-Properties.ActualPoints)
yWR <- WRnums$ActualPoints
xWR <- WRnums %>% select(-ActualPoints)
                                                      

WRFit <- lm(yWR~.,data = xWR)
summary(WRFit)
addToPred <- min(WRFit$residuals)
WRP <- predict(WRFit,WRModel)
WRP <- WRP-addToPred
WRFile <- cbind(WRModel$Properties.Player_Name,WRP)
write.csv(WRFile, file = paste0("WRP",week,".csv"))

