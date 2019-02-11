# TimeSeries_Implementations_R
Implementation of multiple time series algorithms (exponential smooth-single, double, triple, arima-manual and auto) using preloaded datasets in gtrends library.

## Workflow steps/high level details:
•	Used the “gtrendsR” library to extract all monthly historical data associated with the following search queries: “TV”, “data science”, and "hey google". <br>
•	Decomposed the resulting series into their seasonal, trend, and remainder components. <br>
•	Used the HoltWinters and auto.arima functions to fit a single, double, and triple exponential smoothing to the data as well as an ARIMA model using only data for up to December 2016.  <br>
•	Calculated the accuracy of the predictions for all 2017 from above models. <br>
•	Plotted the autocorrelation function for up to 20 lags for each time series. Removed the trend component if necessary, to obtain useful information.  <br>
•	Used information from above correlograms, to make choice of parameters for an ARIMA model.  <br>
•	Fitted an ARIMA model and calculated the mean absolute error for model. <br>
•	Fitted an ARIMA model with auto.arima on the same data and compared against manually generated ARIMA.

