#Use OLS to fit model 

library(readr)

#Import data
national <- read_csv("C:/Users/Steven/SkyDrive/ECON 771/DataSets/national.csv")
View(national)

nationaldf <- as.data.frame(national)

#Lagpad creates a vector that is lagged by the amount in k
#x is the original vector or dataframe
lagpad <- function(x, k) {
  if (!is.vector(x)) 
    stop('x must be a vector')
  if (!is.numeric(x)) 
    stop('x must be numeric')
  if (!is.numeric(k))
    stop('k must be numeric')
  if (1 != length(k))
    stop('k must be a single number')
  c(rep(NA, k), x)[1 : length(x)] 
}

#To create a lag variable. Create a lagged column and then 
#concatenate to original Dataframe
#The variable I use is not a dataframe, just to keep things consistent
#
cpi_lag    <- data.frame(lagpad(nationaldf$cpi,1))
gdp_lag    <- data.frame(lagpad(nationaldf$gdp,1))
employ_lag <- data.frame(lagpad(nationaldf$employment,1))
oil_lag    <- data.frame(lagpad(nationaldf$oil_price,1))

colnames(cpi_lag)[1] <- "cpi_lag"
colnames(gdp_lag)[1] <- "gdp_lag"
colnames(employ_lag)[1] <- "employ_lag"
colnames(oil_lag)[1] <- "oil_lag"

nationaldf <- cbind(nationaldf, cpi_lag, gdp_lag, employ_lag, oil_lag)
summary(nationaldf)

inf    <- log(nationaldf$cpi) - log(nationaldf$cpi_lag)
gdp_gr <- log(nationaldf$gdp) - log(nationaldf$gdp_lag)
emp_gr <- log(nationaldf$employment) - log(nationaldf$employ_lag)
oil_gr <- log(nationaldf$oil_price) - log(nationaldf$oil_lag)

#Run regressions
model <- lm(gdp_gr ~ inf)
summary(model)

model2 <- lm(gdp_gr ~ inf + emp_gr + oil_gr)
summary(model2)