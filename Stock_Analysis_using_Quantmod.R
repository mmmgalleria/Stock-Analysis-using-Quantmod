# Import Library
library(quantmod)

# Get Stock Price
tsla <- getSymbols("TSLA", auto.assign = FALSE)
head(tsla)

# Store Closing Price (adjusted for stock splits)
tsla_cl <- tsla$TSLA.Close
head(tsla_cl)
tsla_cl2 <- Ad(tsla)
head(tsla_cl2)

# Remove Scientific Notation
options(scipen = 9999)

# Create Daily % change vector
daily_change <- tsla_cl/lag(tsla_cl,1) - 1
head(daily_change)

# Look at Histrogram of Percentage Change
hist(daily_change, 40, col = 'green')

# Assign Buy Signal Parameter
buy_signal <- .04

# Loop over all trading days (except the first)
# buy_signal == parameter
# tsla_cl == data
# daily_change == %change
# signal == 1 or 0 buy/sell

# Create Empty Output
signal <- c(NULL)

# Loop for Output Buy Signal
for (i in 2:length(tsla_cl)){
  if (daily_change[i] > buy_signal){
    signal[i] <- 1
  } else
    signal[i] <- 0
}

# Reclassify signal to an xts object (trying it to a date)
signal <- reclass(signal, tsla_cl)

# Plot Graph
chartSeries(tsla_cl,
            type = "l",
            subset = "2011-01::2012-01",
            theme = chartTheme("white"))addCCI() #add Commodity Channel Index

addTA(signal, type = "S", col = "red")