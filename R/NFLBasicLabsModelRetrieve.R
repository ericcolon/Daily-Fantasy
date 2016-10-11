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
getNflLabModel <- function(modelYear = "2015",modelWeek = NULL,model = NULL,mod = "", PositionID = NULL,cookie ="__cfduid=d19da73f21ae9852e137438d8ffd398371475796569; LD_U=http%3A%2F%2Fwww.fantasylabs.com%2F; .AspNet.Cookies=4OqHDcdGDsIxvmTJlDZP4l5WDNavz_EqYieZYNQ0jwz5D4hdcpnW81hhl10guRKYUJ4FUaeWfGILkrlHYAc5iAfkM78TCuId4C3Msk1UXmSOrCRGtWWf8LD2PvJVzmQDYuhLPJxChRIyLrl1J_wmMfFVG1GmepgwOUhW6njdFz7iNy7KAp-QjTnS7IQs20QabkcogIBaqqNQDyuNHD-7oeljqkv1ZDJq6CuGlLxe_8VmH8IX9kEtk8WM2GWATZaahPx09bCoYiNuWcydMTVG2vEOxBjy_obw7EgcJaQ3L8BjTRtfezcSo3_zRaSbSd8hlzFdC3gXQjm5x3H48o68HzJ6Moj3FjwIiAE7cv2KOoZ8GqJBgdD-aSfYangaMtwkgSlO4wYb2eOiLkjZojaBf-nVARxa89E293Fan6i4dZXyCTetik94qijmISHokbYG01lOQUoEPWjm07GsoqWvEp6AVplqN5DVHnqiWC23e54; flid=EriUJhQuj0yimw3ajUgOew; __distillery=a88774e_344d8f47-a8f7-4094-81ba-d3336d4c2d4d-ba08c6bef-19ba082fafba-d101; LD_R=http%3A%2F%2Fwww.fantasylabs.com%2Fnfl%2Fplayer-models%2F; __zlcmid=cyfuDnxjy8XwIK; LD_S=1475989257516; _gat=1; _ga=GA1.2.407536382.1475796573; LD_T=6a763f78-d230-4827-f05d-37ca01630a5d'"){
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
  pmCurlHandles <- paste0("-H 'DNT: 1' -H 'Accept-Encoding: gzip, deflate, sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.116 Safari/537.36' -H 'Accept: application/json, text/plain, */*' -H 'Referer: http://www.fantasylabs.com/nfl/player-models/' -H 'Cookie: ",cookie,"  -o ",y," -H 'Connection: keep-alive' --compressed")
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
