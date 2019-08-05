# TimeSeries_Implementations_R
Implementation of multiple time series algorithms (exponential smooth-single, double, triple, arima-manual and auto) using preloaded datasets in gtrends library.

## Workflow steps/high level details:
•	Used the “gtrendsR” library to extract all monthly historical data associated with the following search queries: “TV”, “data science”, and "hey google"

•	Decomposed the resulting series into their seasonal, trend, and remainder components

•	Used the HoltWinters and auto.arima functions to fit a single, double, and triple exponential smoothing to the data as well as an ARIMA model using only data for up to December 2016. 

•	Calculated the accuracy of the predictions for all 2017 from above models.

•	Plotted the autocorrelation function for up to 20 lags for each time series. Removed the trend component if necessary, to obtain useful information.  

•	Used information from above correlograms, to make choice of parameters for an ARIMA model.  

•	Fitted an ARIMA model and calculated the mean absolute error of your model over the whole data set

•	Fitted an ARIMA model with auto.arima on the same data. 

