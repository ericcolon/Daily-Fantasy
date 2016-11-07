#' NBA Projections
#'
#' @param modelDate 
#'
#' @return NBA projections for desired Date
#' @export
#'
#' @examples projectNBA()
projectNBA <- function(modelDate = "11_7_2016"){
  getNBAPlayerModel(modelDate)
  readCSV <- readNBACSVs(modelDate)
  readCSV <- na.zero(readCSV)
  project1 <- readCSV$Properties.OppPlusMinus*0.2170312559
  project2 <- readCSV$Salary*0.0016437112
  project3 <- readCSV$Properties.Month_Salary_Change*-0.0008733492
  project4 <- readCSV$Properties.CeilingPct*0.1211694691
  project5 <- readCSV$Properties.FantasyPerMinute*2.5317331466
  project6 <- readCSV$Properties.Floor*0.4278688107
  Final <- project1+project2+project3+project4+project5+project6+1.6241031979
  projCostPerPoint <- readCSV$Salary/Final
  actualCostPerPoint <- readCSV$Salary/readCSV$ActualPoints
  finalDf <- data.frame(readCSV$Score,readCSV$Salary,readCSV$Properties.Player_Name,Final,projCostPerPoint,readCSV$ActualPoints,actualCostPerPoint)
  names(finalDf)<- list("Score","Salary","Name","Projection","ProjCostPerPt","ActualPoints","ActualCostPerPt")
  write.csv(finalDf, file = paste0("~/Desktop/NBA_Daily/",modelDate,"Projections.csv"))
  scoreQuantiles <- quantile(readCSV$Score)
  scoreThirdQuant <- scoreQuantiles[4]
  finalDf3rdQuartileScoreOnly <- finalDf %>% filter(Score>=scoreThirdQuant)
  write.csv(finalDf3rdQuartileScoreOnly, file = paste0("~/Desktop/NBA_Daily/",modelDate,"Projections3rdQuantileOnly.csv"))
  projValueQuants <- quantile(finalDf[,5])
  projValue1stQuant <- projValueQuants[2]
  projValueFilteredDf <- finalDf %>% filter(ProjCostPerPt<=projValue1stQuant)
  write.csv(projValueFilteredDf, file = paste0("~/Desktop/NBA_Daily/",modelDate,"ProjectedValuePlays.csv"))
  
  return(finalDf)
}