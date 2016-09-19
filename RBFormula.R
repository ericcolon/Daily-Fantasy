AllRB <- foreach(i=1:17) %do% getNflLabModel("2015",i,PositionID = "RB")
readRB <- foreach(i=1:17) %do% readNFLCSVs("2015",i,PositionID = "RB")
AllRB16<- foreach(i=1) %do% getNflLabModel("2016",i,mod = "Tournament",PositionID = "RB")
readRB16 <- foreach(i=1) %do% readNFLCSVs("2016",i,mod = "Tournament",PositionID = "RB")
RB <- bind_rows(readRB16,readRB)
week <- 1                                                        

RBModel <- getNflLabModel("2016",week,PositionID = "RB")
RBModel <- na.zero(readNFLCSVs("2016",week,PositionID = "RB"))
RBnums <- na.zero(RB[,sapply(RB,is.numeric)])
RBnums <- RBnums %>% select(-X,-Properties.ActualPoints)
yRB <- RBnums$ActualPoints
xRB <- RBnums %>% select(-ActualPoints)
                                                      

RBFit <- lm(yRB~.,data = xRB)
summary(RBFit)
addToPred <- min(RBFit$residuals)
RBP <- predict(RBFit,RBModel)
RBP <- RBP-addToPred
RBFile <- cbind(RBModel$Properties.Player_Name,RBP)
write.csv(RBFile, file = paste0("RBP",week,".csv"))

