#install.packages("quantmod")

require(quantmod)
endowment <- 100
getSymbols("SPY", from = "2000-01-01")

#How do we see the dimensions of the matrix?
dim(SPY)

SPY.close <- SPY[,4] #get only close data
SPY.vector <- as.vector(SPY.close)

#Daily returns 
#This is what we need to use if we want to get ln(close_t/close_t-1)
day_ret <- diff(log(SPY.vector), lag = 1)

#if you want to look at cumulative endowment amounts
#Add 1 to the returns and then create a cumulative factor vector
day_ret_factor <- day_ret + 1

#This is a vector of cumulative daily returns
cumulative_factor <- cumprod(day_ret_factor)

#see the results 
plot(endowment*cumulative_factor) 

#Plotting Time Series dataframes: use the Window function
plot(window(SPY.close, start=c("2016-01-01")))
