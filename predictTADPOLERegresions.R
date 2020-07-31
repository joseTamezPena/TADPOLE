#' forecastCognitiveStatus
#' Forecast ADAS 13 and Ventricle to the specified date 
#' @param Models list of models to be used
#' @param TestDataFrame the frame to be tested
#'
#' @return
#' @export
#'
#' @examples
forecastRegressions <- function(Models,TestDataFrame,futuredate)
{

  deltaTime <- as.numeric(futuredate - TestDataFrame$EXAMDATE)
  TestDataFrame$TimeToLastVisit <- deltaTime
  TestDataFrame$SQRTimeToLastVisit <- deltaTime*deltaTime
  TestDataFrame$LOGTimeToLastVisit <- log(deltaTime)
  TestDataFrame$CUBTimeToLastVisit <- deltaTime*deltaTime*deltaTime*deltaTime
  TestDataFrame$SQRTTimeToLastVisit <- sqrt(deltaTime)
  TestDataFrame$DeltaAdas13 <- numeric(nrow(TestDataFrame))
  TestDataFrame$DeltaVentricle <- numeric(nrow(TestDataFrame))
  
  
  ADAS13_NC <- NULL
  ADAS13_MCI <- NULL
  ADAS13_AD <- NULL
  Ventricles_NC <- NULL
  Ventricles_MCI <- NULL
  Ventricles_AD <- NULL
  
  for (n in length(Models$MCI_Ventricle_ICV_MODEL))
  {
    ADAS13_NC <- cbind(ADAS13_NC,predict(Models$NC_ADAS_MODEL[[n]],TestDataFrame) + TestDataFrame$ADAS13)
    ADAS13_MCI <- cbind(ADAS13_MCI,predict(Models$MCI_ADAS_MODEL[[n]],TestDataFrame) + TestDataFrame$ADAS13)
    ADAS13_AD <- cbind(ADAS13_AD,predict(Models$AD_ADAS_MODEL[[n]],TestDataFrame) + TestDataFrame$ADAS13)
    
    Ventricles_NC <- cbind(Ventricles_NC,predict(Models$NC_Ventricle_ICV_MODEL[[n]],TestDataFrame) + TestDataFrame$Ventricles)
    Ventricles_MCI <- cbind(Ventricles_MCI,predict(Models$MCI_Ventricle_ICV_MODEL[[n]],TestDataFrame) + TestDataFrame$Ventricles)
    Ventricles_AD <- cbind(Ventricles_AD,predict(Models$AD_Ventricle_ICV_MODEL[[n]],TestDataFrame) + TestDataFrame$Ventricles)
  
  }

  predictions <- list(ADAS13_NC = ADAS13_NC,
                      ADAS13_MCI = ADAS13_MCI,
                      ADAS13AD = ADAS13_AD,
                      Ventricles_NC = Ventricles_NC,
                      Ventricles_MCI = Ventricles_MCI,
                      Ventricles_AD = Ventricles_AD
                      )
  return(predictions)
}
  