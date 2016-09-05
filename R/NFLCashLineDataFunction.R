dfsGoldNflRecap <- function(i){
tableget <- read_html(paste0("http://www.dfsgold.com/nfl/2016-week-",i,"-NFL-fantasy-recap-draftkings-6822"))
topLineup <- html_table(tableget)[[1]]
cashLines <- html_table(tableget)[[2]]
playerData <- html_table(tableget)[[3]]
write.csv(topLineup,file=paste0("~/Desktop/NFL_Daily/NFL2015Week",i,"topLineup.csv"))
write.csv(cashLines,file=paste0("~/Desktop/NFL_Daily/NFL2015Week",i,"cashLines.csv"))
write.csv(playerData,file=paste0("~/Desktop/NFL_Daily/NFL2015Week",i,"playerData.csv"))
}