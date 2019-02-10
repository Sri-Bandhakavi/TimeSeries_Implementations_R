## 1. Use the "gtrendsR" library to extract all monthly historical data associated with the following 
##    search queries: "TV", "data science", and "hey google". 
## 2. Decompose the resulting series into their seasonal, trend, and remainder components. 
## 3. Using the HoltWinters and auto.arima functions, fits a single, double, and triple exponential smoothing to the 
##    data as well as an ARIMA model using only data for up to December 2016. 
## 4.  Calculate the accuracy of the predictions for all 2017 of each model via the mean absolute error. 

############################################################################################
 ##########################################################################################
  #Search Query = TV######################################################################
 ##########################################################################################
############################################################################################
  ## Generate TV data time as ts object and decompose 
library("gtrendsR")
library(lubridate)

TV<-gtrends(keyword="TV", time="all")

TVdata<-TV$interest_over_time # will use this to generate a time series and decompose
TVPartition<-TVdata # will use this identical dataset to TV data for partitioning to train and test sets

TVdata<-as.data.frame(TVdata[, 2])
colnames(TVdata)[1]<- "Hits"

TVdataTS<-ts(TVdata, start=c(2004,1), frequency=12)
ts.plot(TVdataTS)

decomposedRes <- decompose(TVdataTS)
plot(decomposedRes, col="blue") 


  ## Partition TV data to train(till 2016 end) and test(2017 only)
TVtrain<-as.data.frame(TVPartition[1:156, 2])

colnames(TVtrain)[1]<-"Hits"

TVtrain<-ts(TVPartition[1:156, 2])
TVtest<-ts(TVPartition[157:168, 2]) 

##################################################################
###Holt Winters Model with smoothing for alpha (random noise) only  
##################################################################
  #modeling
TV_intime_forecasts_HWa <- (HoltWinters(TVtrain, beta=FALSE, gamma = FALSE))

TV_intime_forecasts_HWa # notice the alpha coefficient value - recent data more important
plot(TV_intime_forecasts_HWa) # notice the overlap of observed versus fitted values
  
  #forecast into future
library(forecast)
TV_outoftime_forecasts_HWa <-forecast:::forecast.HoltWinters(TV_intime_forecasts_HWa, h=12) #12 months ahead forecast

TV_outoftime_forecasts_HWa # table of forecasts
plot(TV_outoftime_forecasts_HWa) # plot of forecasts

 # validate model performance
actuals<-as.data.frame(TVtest)
colnames(actuals)[1]<-"Actuals"

TVPred_HWa<-as.data.frame(TV_outoftime_forecasts_HWa)
TVPred_HWa<-as.data.frame(TVPred_HWa[, 1])
colnames(TVPred_HWa)[1]<-"HWaForecast"

ForecastValidationTable<-cbind(actuals, TVPred_HWa)
ForecastValidationTable$abserror_HWa<- abs(ForecastValidationTable$Actuals - ForecastValidationTable$HWaForecast)

###################################################################################
###Holt Winters Model with smoothing for alpha (random noise) and beta (trend) only
###################################################################################  
  #modeling
TV_intime_forecasts_HWab <- HoltWinters(TVtrain, gamma = FALSE) 

TV_intime_forecasts_HWab # notice the alpha coefficient value - recent data more important
plot(TV_intime_forecasts_HWab) # notice the overlap of observed versus fitted values

  #forecast into future
TV_outoftime_forecasts_HWab <-forecast:::forecast.HoltWinters(TV_intime_forecasts_HWab, h=12) #12 months ahead forecast

TV_outoftime_forecasts_HWab # table of forecasts
plot(TV_outoftime_forecasts_HWab) # plot of forecasts

  #validate model performance
TVPred_HWab<-as.data.frame(TV_outoftime_forecasts_HWab)
TVPred_HWab<-as.data.frame(TVPred_HWab[, 1])
colnames(TVPred_HWab)[1]<-"HWabForecast"

ForecastValidationTable<-cbind(ForecastValidationTable, TVPred_HWab)
ForecastValidationTable$abserror_HWab<-abs(ForecastValidationTable$Actuals - ForecastValidationTable$HWabForecast)



#########################################################################################################
###Holt Winters triple exponential smoothing - alpha (random noise), beta (trend) and gamma (seasonality)
#########################################################################################################
  #modeling
#TV_intime_forecasts_HWabg_<-HoltWinters(TVtrain) # model fails to execute so I did automated ses as an alternative (ets) - that will automatically choose best combination of smoothing for error, trend, and seasonality

TV_intime_forecasts_HWabg<-ets(TVtrain)

TV_intime_forecasts_HWabg

plot(TV_intime_forecasts_HWabg$fitted, ylab="Observed/Fitted", main="Auto ETS filtering")
lines(TVtrain, col="red")




  #forecast into future

TV_outoftime_forecasts_HWabg <-forecast:::forecast(TV_intime_forecasts_HWabg, h=12) #12 months ahead forecast

TV_outoftime_forecasts_HWabg # table of forecasts
plot(TV_outoftime_forecasts_HWabg) # plot of forecasts

  #validate
TVPred_HWabg<-as.data.frame(TV_outoftime_forecasts_HWabg)
TVPred_HWabg<-as.data.frame(TVPred_HWabg[, 1])
colnames(TVPred_HWabg)[1]<-"HWabgForecast"

ForecastValidationTable<-cbind(ForecastValidationTable, TVPred_HWabg)
ForecastValidationTable$abserror_HWabg<-abs(ForecastValidationTable$Actuals - ForecastValidationTable$HWabgForecast)

####################################################################################################
###Auto.arima model - automatically finds values for p, d, and q smoothing parameters
####################################################################################################
  #modeling
TV_intime_forecasts_autoarima<-auto.arima(TVtrain)

TV_intime_forecasts_autoarima

plot(fitted(TV_intime_forecasts_autoarima), ylab="Observed/Fitted", main="Autoarima filtering")
lines(TVtrain, col="red")


  #forecasting into future
TV_outoftime_forecasts_autoarima<-forecast:::forecast.Arima(TV_intime_forecasts_autoarima, h=12) # 12 months ahead forecast

plot(TV_outoftime_forecasts_autoarima)

  #validate
TVPred_aa<-as.data.frame(TV_outoftime_forecasts_autoarima)
TVPred_aa<-as.data.frame(TVPred_aa[, 1])
colnames(TVPred_aa)[1]<-"AutoArimaForecast"

ForecastValidationTable<-cbind(ForecastValidationTable, TVPred_aa)
ForecastValidationTable$abserror_AutoArima<-abs(ForecastValidationTable$Actuals - ForecastValidationTable$AutoArimaForecast)

#############################################################################################################################
######Arima model (manual selection of values for p, d, q coefficients)
#############################################################################################################################
 
 # 2 steps - 1. make series stationary and capture order of differencing  2. select p and q values - for # of coefficients for autoregression and moving average components of ARIMA, respectively

  # make series stationary
TVtraindiff1<- diff(TVtrain, differences = 1) # render time series data stationary; start with 1 lag as starting point and check effect visually

plot.ts(TVtraindiff1) # visual check - differenced time series with 1 lag looks stationary/like white noise. 
                          # next:statistically confirm series is stationary using adf.test (p-value < 0.05 for stationary) or kpss.test (p-value > 0.05 for stationary)
library(tseries)

adf.test(TVtraindiff1) # p-value = 0.01; series is stationary! Accept d=1 for ARIMA model

  #select p and q from PACF and ACF plots, respectively

library(astsa)
acf2(TVtraindiff1, max.lag = 20) # acf plot and PACF plot do demonstrate a "somewhat sinusoidal" pattern
                                # select p = 1 as ACF plot is sinusoidal
                                #select q =1 as PACF plot is sinusoidal

  #intime forecasts using selected values of p, d, q from above
TV_intime_forecasts_arima<-arima(TVtrain, order=c(1,1,1)) 

TV_intime_forecasts_arima

plot(fitted(TV_intime_forecasts_arima), ylab="Observed/Fitted", main="Arima filtering")
lines(TVtrain, col="red")

  ##forecast
library(forecast)
  
TV_outoftime_forecasts_arima <-forecast:::forecast.Arima(TV_intime_forecasts_arima, h=12)
TV_outoftime_forecasts_arima

  ##validate accuracy
  
TVPred_arima<-as.data.frame(TV_outoftime_forecasts_arima)
TVPred_arima<-as.data.frame(TVPred_arima[, 1])
colnames(TVPred_arima)[1]<-"ArimaForecast"

ForecastValidationTable<-cbind(ForecastValidationTable, TVPred_arima)
ForecastValidationTable$abserror_Arima<-abs(ForecastValidationTable$Actuals - ForecastValidationTable$ArimaForecast)


#####################################################################
################RESULTS SUMMARY OF TV SEARCHES AND ALL MODELS########
#####################################################################
#show model fit with all models

par(mfrow=c(2,3))

plot(TV_intime_forecasts_HWa)

plot(TV_intime_forecasts_HWab)

plot(TV_intime_forecasts_HWabg$fitted, ylab="Observed/Fitted", main="Auto ETS filtering")
lines(TVtrain, col="red")

plot(fitted(TV_intime_forecasts_autoarima), ylab="Observed/Fitted", main="Autoarima filtering")
lines(TVtrain, col="red")

plot(fitted(TV_intime_forecasts_arima), ylab="Observed/Fitted", main="Arima filtering")
lines(TVtrain, col="red")

dev.off()

#show forecasts of all models including absolute errors for each model by boxplot
par(mfrow=c(2, 3))

plot(TV_outoftime_forecasts_HWa)
plot(TV_outoftime_forecasts_HWab)
plot(TV_outoftime_forecasts_HWabg)
plot(TV_outoftime_forecasts_autoarima)
plot(TV_outoftime_forecasts_arima)

boxplot(ForecastValidationTable$abserror_HWa, ForecastValidationTable$abserror_HWab, 
        ForecastValidationTable$abserror_HWabg, ForecastValidationTable$abserror_AutoArima,
        ForecastValidationTable$abserror_Arima, main = "Absolute prediction error ranges of models for 2017",
        names=c("HWsingle\n6.23", "HWdouble\n7.94", "HWtriple/ets\n6.22", "AutoArima\n6.25", "Arima\n4.92"))

dev.off()

TVSearch_MAE<- c(mean(ForecastValidationTable$abserror_HWa), mean(ForecastValidationTable$abserror_HWab), 
               mean(ForecastValidationTable$abserror_HWabg), mean(ForecastValidationTable$abserror_AutoArima),
               mean(ForecastValidationTable$abserror_Arima))

TVSearch_MAE

Models<-c("HoltWinters_SingleSmoothing", "HoltWinters_DoubleSmoothing", "HoltWinters_TripleSmoothing",
                 "AutoArima", "Arima")

TVS_Model_Summary<-as.data.frame(rbind(Models, TVSearch_MAE))

colnames(TVS_Model_Summary) <- as.character(unlist(TVS_Model_Summary[1,]))
TVS_Model_Summary = TVS_Model_Summary[-1, ]
TVS_Model_Summary



############################################################################################
##########################################################################################
#Search Query = Data Science#############################################################
##########################################################################################
############################################################################################

## Generate Data Science data time, save as ts object, and decompose 
library("gtrendsR")
library(lubridate)

DSdata<-gtrends(keyword="Data Science", time="all")

DSdata<-DSdata$interest_over_time # will use this to generate a time series and decompose
DSPartition<-DSdata # will use this identical dataset for partitioning to train and test sets

DSdata<-as.data.frame(DSdata[, 2])
colnames(DSdata)[1]<- "Hits"

DSdataTS<-ts(DSdata, start=c(2004, 1), frequency = 12)
ts.plot(DSdataTS)

DS_decomposedRes <- decompose(DSdataTS)
plot(DS_decomposedRes, col="black") 

## Partition DS data to train(till 2016 end) and test(2017 only)
DStrain<-as.data.frame(DSPartition[1:156, 2])

colnames(DStrain)[1]<-"Hits"

DStrain<-ts(DSPartition[1:156, 2])
DStest<-ts(DSPartition[157:168, 2]) # 53 weeks in 2017
###

##################################################################
###Holt Winters Model with smoothing for alpha (random noise) only  
##################################################################
#modeling
DS_intime_forecasts_HWa <- (HoltWinters(DStrain, beta=FALSE, gamma = FALSE))

DS_intime_forecasts_HWa # notice the alpha coefficient value - recent data more important
plot(DS_intime_forecasts_HWa) # notice the overlap of observed versus fitted values

#forecast into future
library(forecast)
DS_outoftime_forecasts_HWa <-forecast:::forecast.HoltWinters(DS_intime_forecasts_HWa, h=12) #12 months ahead forecast

DS_outoftime_forecasts_HWa # table of forecasts
plot(DS_outoftime_forecasts_HWa) # plot of forecasts

# validate model performance
DSactuals<-as.data.frame(DStest)
colnames(DSactuals)[1]<-"Actuals"

DSPred_HWa<-as.data.frame(DS_outoftime_forecasts_HWa)
DSPred_HWa<-as.data.frame(DSPred_HWa[, 1])
colnames(DSPred_HWa)[1]<-"HWaForecast"

DSForecastValidationTable<-cbind(DSactuals, DSPred_HWa)
DSForecastValidationTable$abserror_HWa<- abs(DSForecastValidationTable$Actuals - DSForecastValidationTable$HWaForecast)

###################################################################################
###Holt Winters Model with smoothing for alpha (random noise) and beta (trend) only
###################################################################################  
#modeling
DS_intime_forecasts_HWab <- HoltWinters(DStrain, gamma = FALSE) 

DS_intime_forecasts_HWab # notice the alpha coefficient value - recent data more important
plot(DS_intime_forecasts_HWab) # notice the overlap of observed versus fitted values

#forecast into future
DS_outoftime_forecasts_HWab <-forecast:::forecast.HoltWinters(DS_intime_forecasts_HWab, h=12) #12 months ahead forecast

DS_outoftime_forecasts_HWab # table of forecasts
plot(DS_outoftime_forecasts_HWab) # plot of forecasts

#validate model performance
DSPred_HWab<-as.data.frame(DS_outoftime_forecasts_HWab)
DSPred_HWab<-as.data.frame(DSPred_HWab[, 1])
colnames(DSPred_HWab)[1]<-"HWabForecast"

DSForecastValidationTable<-cbind(DSForecastValidationTable, DSPred_HWab)
DSForecastValidationTable$abserror_HWab<-abs(DSForecastValidationTable$Actuals - DSForecastValidationTable$HWabForecast)

#########################################################################################################
###Holt Winters triple exponential smoothing - alpha (random noise), beta (trend) and gamma (seasonality)
#########################################################################################################
#modeling
#DS_intime_forecasts_HWabg<-HoltWinters(DStrain) # could not use this option as the model fails to execute - used automated ets smoothing instead
DS_intime_forecasts_HWabg<-ets(DStrain)

DS_intime_forecasts_HWabg

plot(DS_intime_forecasts_HWabg$fitted, ylab="Observed/Fitted", main="Auto ETS filtering")
lines(DStrain, col="red")


#forecast into future

DS_outoftime_forecasts_HWabg <-forecast:::forecast(DS_intime_forecasts_HWabg, h=12) #12 months ahead forecast

DS_outoftime_forecasts_HWabg # table of forecasts
plot(DS_outoftime_forecasts_HWabg) # plot of forecasts

#validate
DSPred_HWabg<-as.data.frame(DS_outoftime_forecasts_HWabg)
DSPred_HWabg<-as.data.frame(DSPred_HWabg[, 1])
colnames(DSPred_HWabg)[1]<-"HWabgForecast"

DSForecastValidationTable<-cbind(DSForecastValidationTable, DSPred_HWabg)
DSForecastValidationTable$abserror_HWabg<-abs(DSForecastValidationTable$Actuals - DSForecastValidationTable$HWabgForecast)


####################################################################################################
###Auto.arima model - automatically finds values for p, d, and q smoothing parameters
####################################################################################################
#modeling
DS_intime_forecasts_autoarima<-auto.arima(DStrain)

DS_intime_forecasts_autoarima

plot(fitted(DS_intime_forecasts_autoarima), ylab="Observed/Fitted", main="Autoarima filtering")
lines(DStrain, col="red")

#forecasting into future
DS_outoftime_forecasts_autoarima<-forecast:::forecast.Arima(DS_intime_forecasts_autoarima, h=12) # 12 months ahead forecast

plot(DS_outoftime_forecasts_autoarima)



#validate
DSPred_aa<-as.data.frame(DS_outoftime_forecasts_autoarima)
DSPred_aa<-as.data.frame(DSPred_aa[, 1])
colnames(DSPred_aa)[1]<-"AutoArimaForecast"

DSForecastValidationTable<-cbind(DSForecastValidationTable, DSPred_aa)
DSForecastValidationTable$abserror_AutoArima<-abs(DSForecastValidationTable$Actuals - DSForecastValidationTable$AutoArimaForecast)


#############################################################################################################################
######Arima model (manual selection of values for p, d, q coefficients)
#############################################################################################################################

# 2 steps - 1. make series stationary and capture order of differencing  2. select p and q values - for # of coefficients for autoregression and moving average components of ARIMA, respectively

# make series stationary
DStraindiff1<- diff(DStrain, differences = 1) # render time series data stationary; start with 1 lag as starting point and check effect visually

plot.ts(DStraindiff1) # visual check - differenced time series with 1 lag looks stationary/like white noise. 
# next:statistically confirm series is stationary using adf.test (p-value < 0.05 for stationary) or kpss.test (p-value > 0.05 for stationary)
library(tseries)

adf.test(DStraindiff1) # p-value = 0.01; series is stationary! Accept d=1 for ARIMA model

#select p and q from PACF and ACF plots, respectively

library(astsa)
acf2(DStraindiff1, max.lag = 20) # acf and pacf plot "somewhat sinusoidal" pattern; set p=4, q=1 since these reflect inflection points

#intime forecasts using selected values of p, d, q from above
DS_intime_forecasts_arima<-arima(DStrain, order=c(4,1,1)) 

plot(fitted(DS_intime_forecasts_arima), ylab="Observed/Fitted", main="Arima filtering")
lines(DStrain, col="red")


##forecast
library(forecast)

DS_outoftime_forecasts_arima <-forecast:::forecast.Arima(DS_intime_forecasts_arima, h=12)
DS_outoftime_forecasts_arima

##validate accuracy

DSPred_arima<-as.data.frame(DS_outoftime_forecasts_arima)
DSPred_arima<-as.data.frame(DSPred_arima[, 1])
colnames(DSPred_arima)[1]<-"ArimaForecast"

DSForecastValidationTable<-cbind(DSForecastValidationTable, DSPred_arima)
DSForecastValidationTable$abserror_Arima<-abs(DSForecastValidationTable$Actuals - DSForecastValidationTable$ArimaForecast)

###############################################################################
################RESULTS SUMMARY OF Data Science SEARCHES AND ALL MODELS########
###############################################################################

par(mfrow=c(2,3))

plot(DS_intime_forecasts_HWa)

plot(DS_intime_forecasts_HWab)

plot(DS_intime_forecasts_HWabg$fitted, ylab="Observed/Fitted", main="Auto ETS filtering")
lines(DStrain, col="red")

plot(fitted(DS_intime_forecasts_autoarima), ylab="Observed/Fitted", main="Autoarima filtering")
lines(DStrain, col="red")

plot(fitted(DS_intime_forecasts_arima), ylab="Observed/Fitted", main="Arima filtering")
lines(DStrain, col="red")

dev.off()





par(mfrow=c(2, 3))

plot(DS_outoftime_forecasts_HWa)
plot(DS_outoftime_forecasts_HWab)
plot(DS_outoftime_forecasts_HWabg)
plot(DS_outoftime_forecasts_autoarima)
plot(DS_outoftime_forecasts_arima)
boxplot(DSForecastValidationTable$abserror_HWa, DSForecastValidationTable$abserror_HWab, 
        DSForecastValidationTable$abserror_HWabg, DSForecastValidationTable$abserror_AutoArima,
        DSForecastValidationTable$abserror_Arima, main = "Absolute prediction error ranges of models for 2017",
        names=c("HWsingle\n22.43", "HWdouble\n16.67", "HWtriple/ets\n14.35", "AutoArima\n16.02", "Arima\n22.26"))


dev.off()


DSSearch_MAE<- c(mean(DSForecastValidationTable$abserror_HWa), mean(DSForecastValidationTable$abserror_HWab), 
                 mean(DSForecastValidationTable$abserror_HWabg), mean(DSForecastValidationTable$abserror_AutoArima),
                 mean(DSForecastValidationTable$abserror_Arima))


Models<-c("HoltWinters_SingleSmoothing", "HoltWinters_DoubleSmoothing", "HoltWinters_TripleSmoothing",
          "AutoArima", "Arima")

DSS_Model_Summary<-as.data.frame(rbind(Models, DSSearch_MAE))

colnames(DSS_Model_Summary) <- as.character(unlist(DSS_Model_Summary[1,]))
DSS_Model_Summary = DSS_Model_Summary[-1, ]

DSS_Model_Summary


############################################################################################
##########################################################################################
#Search Query = Hey Google###############################################################
##########################################################################################
############################################################################################
## Generate TV data time as ts object and decompose 
library("gtrendsR")
library(lubridate)

HG<-gtrends(keyword="hey google", time="all")

HGdata<-HG$interest_over_time # will use this to generate a time series and decompose
HGPartition<-HGdata # will use this identical dataset for partitioning to train and test sets

HGdata<-as.data.frame(HGdata[, 2])
colnames(HGdata)[1]<- "Hits"

HGdataTS<-ts(HGdata, start=c(2004,1), frequency=12)
ts.plot(HGdataTS)

HGdecomposedRes <- decompose(HGdataTS)
plot(HGdecomposedRes, col="purple") 

## Partition HG data to train(till 2016 end) and test(2017 only)
HGtrain<-as.data.frame(HGPartition[1:156, 2])

colnames(HGtrain)[1]<-"Hits"

HGtrain<-ts(HGPartition[1:156, 2])
HGtest<-ts(HGPartition[157:168, 2]) # 

##################################################################
###Holt Winters Model with smoothing for alpha (random noise) only  
##################################################################
#modeling
HG_intime_forecasts_HWa <- (HoltWinters(HGtrain, beta=FALSE, gamma = FALSE))

HG_intime_forecasts_HWa # notice the alpha coefficient value - recent data more important
plot(HG_intime_forecasts_HWa) # notice the overlap of observed versus fitted values

#forecast into future
library(forecast)
HG_outoftime_forecasts_HWa <-forecast:::forecast.HoltWinters(HG_intime_forecasts_HWa, h=12) #12 months ahead forecast

HG_outoftime_forecasts_HWa # table of forecasts
plot(HG_outoftime_forecasts_HWa) # plot of forecasts

# validate model performance
HGactuals<-as.data.frame(HGtest)
colnames(HGactuals)[1]<-"Actuals"

HGPred_HWa<-as.data.frame(HG_outoftime_forecasts_HWa)
HGPred_HWa<-as.data.frame(HGPred_HWa[, 1])
colnames(HGPred_HWa)[1]<-"HWaForecast"

HGForecastValidationTable<-cbind(HGactuals, HGPred_HWa)
HGForecastValidationTable$abserror_HWa<- abs(HGForecastValidationTable$Actuals - HGForecastValidationTable$HWaForecast)

###################################################################################
###Holt Winters Model with smoothing for alpha (random noise) and beta (trend) only
###################################################################################  
#modeling
HG_intime_forecasts_HWab <- HoltWinters(HGtrain, gamma = FALSE) 

HG_intime_forecasts_HWab # notice the alpha coefficient value - recent data more important
plot(HG_intime_forecasts_HWab) # notice the overlap of observed versus fitted values

#forecast into future
HG_outoftime_forecasts_HWab <-forecast:::forecast.HoltWinters(HG_intime_forecasts_HWab, h=12) #12 months ahead forecast

HG_outoftime_forecasts_HWab # table of forecasts
plot(HG_outoftime_forecasts_HWab) # plot of forecasts

#validate model performance
HGPred_HWab<-as.data.frame(HG_outoftime_forecasts_HWab)
HGPred_HWab<-as.data.frame(HGPred_HWab[, 1])
colnames(HGPred_HWab)[1]<-"HWabForecast"

HGForecastValidationTable<-cbind(HGForecastValidationTable, HGPred_HWab)
HGForecastValidationTable$abserror_HWab<-abs(HGForecastValidationTable$Actuals - HGForecastValidationTable$HWabForecast)

#########################################################################################################
###Holt Winters triple exponential smoothing - alpha (random noise), beta (trend) and gamma (seasonality)
#########################################################################################################
#modeling
#HG_intime_forecasts_HWabg<-HoltWinters(HGtrain) # had to abandon this option as the model fails to execute - and elected to use automated ets smoothing as shown below

HG_intime_forecasts_HWabg<-ets(HGtrain)

HG_intime_forecasts_HWabg

plot(HG_intime_forecasts_HWabg$fitted, ylab="Observed/Fitted", main="Auto ETS filtering")
lines(HGtrain, col="red")

#forecast into future

HG_outoftime_forecasts_HWabg <-forecast:::forecast(HG_intime_forecasts_HWabg, h=12) #12 months ahead forecast

HG_outoftime_forecasts_HWabg # table of forecasts
plot(HG_outoftime_forecasts_HWabg) # plot of forecasts

#validate
HGPred_HWabg<-as.data.frame(HG_outoftime_forecasts_HWabg)
HGPred_HWabg<-as.data.frame(HGPred_HWabg[, 1])
colnames(HGPred_HWabg)[1]<-"HWabgForecast"

HGForecastValidationTable<-cbind(HGForecastValidationTable, HGPred_HWabg)
HGForecastValidationTable$abserror_HWabg<-abs(HGForecastValidationTable$Actuals - HGForecastValidationTable$HWabgForecast)

####################################################################################################
###Auto.arima model - automatically finds values for p, d, and q smoothing parameters
####################################################################################################
#modeling
HG_intime_forecasts_autoarima<-auto.arima(HGtrain)

HG_intime_forecasts_autoarima

plot(fitted(HG_intime_forecasts_autoarima), ylab="Observed/Fitted")
lines(HGtrain, col="red")

#forecasting into future
HG_outoftime_forecasts_autoarima<-forecast:::forecast.Arima(HG_intime_forecasts_autoarima, h=12) # 12 months ahead forecast

plot(HG_outoftime_forecasts_autoarima)

#validate
HGPred_aa<-as.data.frame(HG_outoftime_forecasts_autoarima)
HGPred_aa<-as.data.frame(HGPred_aa[, 1])
colnames(HGPred_aa)[1]<-"AutoArimaForecast"

HGForecastValidationTable<-cbind(HGForecastValidationTable, HGPred_aa)
HGForecastValidationTable$abserror_AutoArima<-abs(HGForecastValidationTable$Actuals - HGForecastValidationTable$AutoArimaForecast)

#############################################################################################################################
######Arima model (manual selection of values for p, d, q coefficients)
#############################################################################################################################

# 2 steps - 1. make series stationary and capture order of differencing  2. select p and q values - for # of coefficients for autoregression and moving average components of ARIMA, respectively
plot.ts(HGtrain)
# make series stationary
HGtraindiff1<- diff(HGtrain, differences = 1) # render time series data stationary; start with 1 lag as starting point and check effect visually

plot.ts(HGtraindiff1) # visual check - differenced time series with 1 lag looks stationary/like white noise. 
# next:statistically confirm series is stationary using adf.test (p-value < 0.05 for stationary) or kpss.test (p-value > 0.05 for stationary)
library(tseries)

adf.test(HGtraindiff1) # p-value = 0.01; series is stationary! Accept d=1 for ARIMA model

#select p and q from PACF and ACF plots, respectively

library(astsa)
acf2(HGtraindiff1, max.lag = 20) # acf and pacf plot "somewhat sinusoidal" pattern; set p=1 and q=1 as point of inflection in both plots

#intime forecasts using selected values of p, d, q from above
HG_intime_forecasts_arima<-arima(HGtrain, order=c(1,1,1)) 

plot(fitted(HG_intime_forecasts_arima), ylab="Observed/Fitted")
lines(HGtrain, col="red")


##forecast
library(forecast)

HG_outoftime_forecasts_arima <-forecast:::forecast.Arima(HG_intime_forecasts_arima, h=12)

HG_outoftime_forecasts_arima
plot(HG_outoftime_forecasts_arima)

##validate accuracy

HGPred_arima<-as.data.frame(HG_outoftime_forecasts_arima)
HGPred_arima<-as.data.frame(HGPred_arima[, 1])
colnames(HGPred_arima)[1]<-"ArimaForecast"

HGForecastValidationTable<-cbind(HGForecastValidationTable, HGPred_arima)
HGForecastValidationTable$abserror_Arima<-abs(HGForecastValidationTable$Actuals - HGForecastValidationTable$ArimaForecast)


###############################################################################
################RESULTS SUMMARY OF Hey Google (HG) SEARCHES AND ALL MODELS#####
###############################################################################


par(mfrow=c(2,3))

plot(HG_intime_forecasts_HWa)

plot(HG_intime_forecasts_HWab)

plot(HG_intime_forecasts_HWabg$fitted, ylab="Observed/Fitted", main="Auto ETS filtering")
lines(HGtrain, col="red")

plot(fitted(HG_intime_forecasts_autoarima), ylab="Observed/Fitted", main="Autoarima filtering")
lines(HGtrain, col="red")

plot(fitted(HG_intime_forecasts_arima), ylab="Observed/Fitted", main="Arima filtering")
lines(HGtrain, col="red")

dev.off()


par(mfrow=c(2, 3))

plot(HG_outoftime_forecasts_HWa)
plot(HG_outoftime_forecasts_HWab)
plot(HG_outoftime_forecasts_HWabg)
plot(HG_outoftime_forecasts_autoarima)
plot(HG_outoftime_forecasts_arima)

boxplot(HGForecastValidationTable$abserror_HWa, HGForecastValidationTable$abserror_HWab, 
        HGForecastValidationTable$abserror_HWabg, HGForecastValidationTable$abserror_AutoArima,
        HGForecastValidationTable$abserror_Arima, main = "Absolute prediction error ranges of models for 2017",
        names=c("HWsingle\n7.83", "HWdouble\n7.25", "HWtriple/ets\n7.83", "AutoArima\n8.10", "Arima\n8.21"))

dev.off()




HGSearch_MAE<- c(mean(HGForecastValidationTable$abserror_HWa), mean(HGForecastValidationTable$abserror_HWab), 
                 mean(HGForecastValidationTable$abserror_HWabg), mean(HGForecastValidationTable$abserror_AutoArima),
                 mean(HGForecastValidationTable$abserror_Arima))


Models<-c("HoltWinters_SingleSmoothing", "HoltWinters_DoubleSmoothing", "HoltWinters_TripleSmoothing",
          "AutoArima", "Arima")

HG_Model_Summary<-as.data.frame(rbind(Models, HGSearch_MAE))

colnames(HG_Model_Summary) <- as.character(unlist(HG_Model_Summary[1,]))
HG_Model_Summary = HG_Model_Summary[-1, ]

HG_Model_Summary






###########***********************########################
##########################################################
############################QUESTION 2 ###################
##########################################################
###############********************#######################

# Using the same data of the previous script, plots the autocorrelation function 
# for up to 20 lags for each time series. Remove the trend component if necessary 
# to obtain useful information.  2. Using the information from the correlograms, 
# explain your choice of parameters for an ARIMA model.  3. With your decision, 
# fit an ARIMA model and calculate the mean absolute error of your model over the 
# whole data set (that is you don't need to hold out part of the data as in the previous 
# script). 4. Fit an ARIMA model with auto.arima on the same data. Compare the resulting 
# model complexity with your choice.  

#TV searches#
library(astsa)
acf2(TVtrain, max.lag=20) 
acf2(TVtraindiff1, max.lag = 20) # differencing order (d) set to 1 based on lines 154 to 164 of code above
                                # p and q set to 1 given somewhat sinusoidal pattern of ACF and PACF plots
TVFCAST_a<-arima(TVdataTS, order=c(1,1,1)) 
(forecast::accuracy(TVFCAST_a))

TVFCAST_aa<-auto.arima(TVdataTS)
(forecast::accuracy(TVFCAST_aa))

#Data Science searches#
acf2(DStrain, max.lag=20) 
acf2(DStraindiff1, max.lag = 20) # differencing order (d) set to 1 based on lines 392-402 of above code
                                # acf and pacf plot "somewhat sinusoidal" pattern; set p=4, q=1 since these reflect inflection points
DSFCAST_a<-arima(DSdataTS, order = c(4,1,1))
(forecast::accuracy(DSFCAST_a))

DSFCAST_aa<-auto.arima(DSdataTS)
(forecast::accuracy(DSFCAST_aa))

#Hey Google searches#
acf2(HGtrain, max.lag=20)
acf2(HGtraindiff1, max.lag=20) # differencing order(d) set to 1 based on lines 624-634 of code above
                          # acf and pacf plot "somewhat sinusoidal" pattern; set p=1 and q=1 as point of inflection in both plots
plot(HGtraindiff1)
HGFCAST_a<-arima(HGdataTS,order=c(1,1,1))
(forecast::accuracy(HGFCAST_a))

HGFCAST_aa<-auto.arima(HGdataTS)
(forecast::accuracy(HGFCAST_aa))
