# TimeSeries_Implementations_R
Implementation of multiple time series algorithms (exponential smooth-single, double, triple, arima-manual and auto) using preloaded datasets in gtrends library. 

For detailed descrption of use case examples and methods/results see (https://github.com/Sri-Bandhakavi/TimeSeries_Implementations_R/blob/master/TimeSeries_Examples_Methods_Results_Description.pdf). 

For brief summary of workflow steps/high level details see below.

## Workflow steps/high level details:
Used the “gtrendsR” library to extract all monthly historical data associated with the following search queries: “TV”, “data science”, and "hey google" and compared performance of various algorithms used. Workflow steps follow:

  •	Decomposed the resulting series into their seasonal, trend, and remainder components

  •	Used the HoltWinters (single, double, and triple exponential smoothing functions), ARIMA, and auto.ARIMA algorithms on data for up to         December 2016 time-period. 
  
  •	Plotted the autocorrelation function for up to 20 lags for each time series. Used information from above correlograms, to make choice of       parameters for an ARIMA model.  

  •	Calculated predictions for 2017 data and compared performance across all algorithms using various accuracy measures.

 


  

