AllDST <- foreach(i=1:17) %do% getNflLabModel("2015",i,PositionID = "DST")
readDST <- foreach(i=1:17) %do% readNFLCSVs("2015",i,PositionID = "DST")
AllDST16<- foreach(i=1) %do% getNflLabModel("2016",i,mod = "Tournament",PositionID = "DST")
readDST16 <- foreach(i=1) %do% readNFLCSVs("2016",i,mod = "Tournament",PositionID = "DST")
DST <- bind_rows(readDST16,readDST)

week <- 2

DSTModel <- getNflLabModel("2016",week,PositionID = "DST")
DSTModel <- na.zero(readNFLCSVs("2016",week,PositionID = "DST"))
DSTnums <- na.zero(DST[,sapply(DST,is.numeric)])
DSTnums <- DSTnums %>% select(-X,-Properties.ActualPoints)
yDST <- DSTnums$ActualPoints
xDST <- DSTnums %>% select(-ActualPoints)
                                                     

DSTFit <- lm(yDST~.,data = xDST)
summary(DSTFit)
addToPred <- min(DSTFit$residuals)
DSTP <- predict(DSTFit,DSTModel)
DSTP <- DSTP-addToPred
DSTFile <- data.frame(cbind(DSTModel$Properties.Player_Name,DSTP))
DSTFile <- DSTFile %>% dplyr::arrange(V1) %>% mutate(cruncherName = as.character(c('Cardinals','Falcons','Ravens','Bills','Panthers','Bears','Bengals','Browns','Cowboys','Broncos','Lions','Packers','Texans','Colts','Jaguars','Chiefs','Rams','Dolphins','Vikings','Patriots','Saints','Giants','Jets','Raiders','Eagles','Steelers','Chargers','49ers','Seahawks','Buccaneers','Titans','Redskins')))

write.csv(DSTFile, file = paste0("DSTP",week,".csv"))
