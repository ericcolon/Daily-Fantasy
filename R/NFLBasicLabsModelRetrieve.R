#'NFL FantasyLabs Model Retrieve
#'
#' @param modelYear 
#' @param modelWeek 
#' @param model 
#' @param mod 
#' @param PositionID 
#'
#' @return FantasyLabs NFL model
#' @export
#'
#' @examples getNflLabModel(modelYear = "2015",modelWeek = NULL,model = NULL,mod = "", PositionID = NULL)
getNflLabModel <- function(modelYear = "2015",modelWeek = NULL,model = NULL,mod = "", PositionID = NULL){
  modelIdList <- c("519959","519957","199070","519650","519649","76475","519987")
  
  modelWeekDates2015 <- c("9_9_2015","9_16_2015","9_23_2015","9_30_2015","10_7_2015","10_14_2015","10_21_2015","10_28_2015","11_4_2015","11_11_2015","11_18_2015","11_25_2015","11_2_2015","11_9_2015","11_16_2015","11_23_2015","11_30_2015")
  if(modelYear == "2015"){
    if(is.null(modelWeek)){
      modelWeek <- readline("2015, Week 1-17?: ")
    }
    modelDate = modelWeekDates2015[modelWeek]
  }
  modelWeekDates2016 <- c("9_7_2016","9_14_2016","9_21_2016","9_28_2016","10_5_2016","10_12_2016","10_19_2016","10_26_2016","11_2_2016","11_9_2016","11_16_2016","11_23_2016","11_30_2016","12_7_2016","12_14_2016","12_21_2016","12_28_2016")  
  if(modelYear == "2016"){
    if(is.null(modelWeek)){
      modelWeek <- readline("2016, Week 1-17?: ")
    }
    modelDate = modelWeekDates2016[modelWeek]
  }
  modelId <- if(is.null(model)){
    modelId <- 1
  }
  if(mod=="Bales"){
    modelId = 2}
  if(mod=="Proj"){
    modelId = 3}
  if(mod=="Levitan"){
    modelId = 4}
  if(mod=="Cash"){
    modelId = 5}
  if(mod=="Optimized"){
    modelId = 6}
  if(mod=="Tournament"){
    modelId = 7}
  
  modelURL <- paste0("http://www.fantasylabs.com/api/playermodel/1/",modelDate,"/?modelid=",modelIdList[modelId])
  
  y <- paste0("nfl",modelYear,"Week",modelWeek,PositionID,".json")
  pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/52.0.2743.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/nfl/player-models/?date=09102016' -H 'Cookie: __distillery=a9f5185_f6f2f8ff-c6e2-4195-8692-9c8d16187164-b489a30ed-a7e7fa52b2a5-42c7; __cfduid=db9b1b49181a8ec11f989911199d472171470029244; LD_U=http%3A%2F%2Fwww.fantasylabs.com%2F; LD_R=http%3A%2F%2Fwww.fantasylabs.com%2FMLB%2Ftrends%2F473658%2F; __zlcmid=bufk4GxGcd42S2; LD_S=1473081919962; _gat=1; .AspNet.Cookies=VCOsQB8CaHd1sTx8xJPpnOiAzin9TRDC8L9xcITrceD9mrI5Eaq8Lwl2BKFh_N7jLybcglr-ez1esWdBPdDLjs_kcj5B55Ui8Jb7ABG_NzWvkr8pyk-MCZoTf9OWsWGlDzLVs5ZPXx5jxQ8ZVj-MTDhg3ucqVmzFnC1cJ1L1do4-1596p4SKMtFSONb-0hNWAgUs8tkGB_wbI5IC4ZHulHe8u_EU_JG36ByK0XyKBbYamr53Gta3r4e9F-og4B0AadK8rgJhOuwlMg3zgHTJ3_0XNpsix24bNcq-H8S48sVP1KMRPXLy3W84-7xKZ5H-fJM3A2J0bIF9cZOq8uDbQQztR8LHxoQqaIPO79CfNvwBAAJV1DZMsTiKjo9krHYjWFWCiUlgU3DQoOx1d3Js3XzGS0CHwr6u5w_MmXSI4OXXD-kH7cRM457GgNIEMEpGk6kEYCau1dmU-sPAOG_lj0H2fWdrvVgHyQ4IakYDGH0; flid=EUrsOK-wd0WjwXvdf65RNg; LD_T=a722ef7d-b6ba-4451-c097-4133db680240; _ga=GA1.2.940766083.1470029245' -o ",y," -H 'Connection: keep-alive' --compressed")  
  curlAddress <- paste0("curl ",modelURL," ",pmCurlHandles)
  system(curlAddress)
  
  nflModelAll <- jsonlite::fromJSON(y, flatten = TRUE)
  nflModelDK <- nflModelAll$PlayerModels[nflModelAll$PlayerModels$Properties.SourceId == 4,]
  if(is.null(PositionID)){
    nflModel <- dplyr::arrange(nflModelDK,desc(Properties.ActualPoints))}
  if(PositionID == "QB"){
    nflModel <- nflModelDK[nflModelDK$PositionId == "101",]
    nflModel <- dplyr::arrange(nflModel,desc(Properties.ActualPoints))}
  if(PositionID == "RB"){
    nflModel <- nflModelDK[nflModelDK$PositionId == "102",]
    nflModel <- dplyr::arrange(nflModel,desc(Properties.ActualPoints))}
  if(PositionID == "WR"){
    nflModel <- nflModelDK[nflModelDK$PositionId == "103",]
    nflModel <- dplyr::arrange(nflModel,desc(Properties.ActualPoints))}
  if(PositionID == "TE"){
    nflModel <- nflModelDK[nflModelDK$PositionId == "104",]
    nflModel <- dplyr::arrange(nflModel,desc(Properties.ActualPoints))}
  if(PositionID == "DST"){
    nflModel <- nflModelDK[nflModelDK$PositionId == "105",]
    nflModel <- dplyr::arrange(nflModel,desc(Properties.ActualPoints))}
  
  
  LabsModel <- sapply(nflModel,as.character)
  write.csv(LabsModel, file = paste0("~/Desktop/NFL_Daily/NFL_",modelYear,"_Week",modelWeek,"/NFL",modelYear,"Week",modelWeek,PositionID,mod,".csv"))
  return(LabsModel)
}
