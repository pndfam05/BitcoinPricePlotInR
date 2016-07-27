#################################################
# Caution: This downloads a compressed file of
# about 60 MB
#################################################

# http://blog.revolutionanalytics.com/2015/11/accessing-bitcoin-data-with-r.html
# by Joseph Rickert

# I am not yet a Bitcoin advocate. Nevertheless, I am impressed with the
# amount of Bitcoin activity and the progress that advocates are making
# towards having Bitcoin recognized as a legitimate currency. Right now,
# I am mostly interested in the technology behind bitcoin and the possibility
# of working with some interesting data sets. A good bit of historical
# data is located on sites like bitstamp.net and bitcoincharts.com, and most
# of it is easily accessible from R with just a little data munging. In this
# post, I present some code that may be helpful to someone who wants to get
# started working with Bitcoin data in R.

# Transaction data is available in a JSON file from bitstamp.net here
# (https://www.bitstamp.net/api/transactions/). This
# can be easily read with the fromJSON() function from the RJSONIO package
# and put into a data frame with the help of do.call().

# install.packages("RJSONIO")
library(RJSONIO)
url <- "https://www.bitstamp.net/api/transactions/"
bs_data <- fromJSON(url) # returns a list
bs_df <- do.call(rbind,lapply(bs_data,data.frame,stringsAsFactors=FALSE))
# head(bs_df)

#         date     tid  price    type     amount
# 1 1446329342 9614372 310.78    0      0.15350400
# 2 1446329337 9614371 310.10    1      3.00000000
# 3 1446329278 9614370 310.10    1      5.40000000
# 4 1446329273 9614369 310.79    1      0.15682615
# 5 1446329273 9614368 310.70    0      0.02679437
# 6 1446329273 9614367 310.60    0      0.02680299

# ***************************************************************************
# If you are a newcomer you might find the one-liner using the plyr
# library more intuitive; but do have a look at the efficiency discussion
# on stackoverflow.

# library(plyr)
# bs_df2 <- ldply(bs_data,data.frame)
# ***************************************************************************


# In the data frame returned above "tid" is the transaction id. "date"
# is the time stamp of the transaction in unix time. (We will see how
# to work with that below.) "price" is the bitcoin price. "amount"
# the amount of bitcoin, and "type" = 0 for a buy and 1 for a sell.

# Bitcoincharts has some market data in JSON format here. However,
# applying exactly the same code will most likely result in a
# very unhelpful error message something like this:

# Error in data.frame(volume = 0, latest_trade = 1415135720, bid = 342.87015289,  :
#                         arguments imply differing number of rows: 1, 0

# What's going on here is that Nulls in the data will keep R from
# building a data frame because some rows will have more columns than
# others. On way around it is to use a function like nullToNA()to turn
# Nulls into NAs on the list returned by fromJSON().

# nullToNA <- function(x) {
#      x[sapply(x, is.null)] <- NA
#      return(x)
#      }
#
# url_m <- "http://api.bitcoincharts.com/v1/markets.json"
# mkt_data <- fromJSON(url_m)
# str(mkt_data)
# mkt_data2 <- lapply(mkt_data,nullToNA)
# mkt_df <- do.call(rbind,lapply(mkt_data2,data.frame,stringsAsFactors=FALSE))
# head(mkt_df)

#   volume latest_trade          bid       high currency currency_volume          ask        close          avg
#   1 1580.379   1446339230 4534200.0000 4800000.00      IDR    7313517821.8 4534300.0000 4534300.0000 4627699.9493
#   2 1656.868   1446338556  158168.0300     963.39      USD        637542.2     158.4900     597.8300     384.7876
#   3    0.000   1415135720     342.8702         NA      USD             0.0     347.8674     327.4206           NA
#   4    0.000   1444487795     383.6141         NA      SGD             0.0     409.2476     341.7299           NA
#   5    0.000   1446224056     291.2600         NA      EUR             0.0     291.3500     291.3400           NA
#   6    0.000   1414593608   25000.0000         NA      XRP             0.0   74998.0000   64001.0000           NA

#   symbol       low
#   1   btcoidIDR 4508600.0
#   2 localbtcUSD     248.9
#   3   rippleUSD        NA
#   4    anxhkSGD        NA
#   5    zyadoEUR        NA
#   6     justXRP        NA

# The easiest way to get Bitcoin transaction data is from the cache of
# zipped .csv files at http://api.bitcoincharts.com/v1/csv/. The following
# bit of code downloads the bitstamp transaction data in US dollars,
# un-zips it and reads all 8 million rows or so into a data frame.
# Then the unix timestamp is converted into a date.

# Code to read compressed .gz files
# http://api.bitcoincharts.com/v1/csv/
# Data Source

bitcoin_file <- "bitstampUSD.csv.gz"
URL <- "http://api.bitcoincharts.com/v1/csv"
source_file <- file.path(URL,bitcoin_file)
# Data destination on local disk
dataDir <-"C:/Users/deering/Documents/R/Bitcoin/Data"
dest_file <- file.path(dataDir,bitcoin_file)
# Download to disk
download.file(source_file,destfile = dest_file)
# Uncompress .gz file and read into a data frame
raw <- read.csv(gzfile(dest_file),header=FALSE)
# head(raw,2)

#   V1   V2      V3
#   1 1315922016 5.80  1.0000
#   2 1315922024 5.83  3.0000

names(raw) <- c("unixtime","price","amount")
raw$date <- as.Date(as.POSIXct(raw$unixtime, origin="1970-01-01"))
# tail(raw)
#       unixtime price amount       date
#   1 1315922016  5.80      1 2011-09-13
#   2 1315922024  5.83      3 2011-09-13

# Now for the payoff: we use dplyr functions and xts() to aggregate the
# transactions into a time series and digraph() visualize the results.

library(dplyr)
library(xts)
library(dygraphs)
data <- select(raw,-unixtime)
# rm(raw)

data <- mutate(data,value = price * amount)
by_date <- group_by(data,date)
daily <- summarise(by_date,count = n(),
                   m_price <-  mean(price, na.rm = TRUE),
                   m_amount <- mean(amount, na.rm = TRUE),
                   m_value <-  mean(value, na.rm = TRUE))

names(daily) <- c("date","count","m_value","m_price","m_amount")
# head(daily,2)
#   Source: local data frame [6 x 5]
#           date count  m_value   m_price m_amount
#         (date) (int)    (dbl)     (dbl)    (dbl)
#   1 2011-09-13    12 5.874167  4.864282 28.84145
#   2 2011-09-14    14 5.582143  4.367570 24.41820

# Make the m_value variable into a time series object
daily_ts <- xts(daily$m_value,order.by=daily$date)
# Plot with htmlwidget dygraph
dygraph(daily_ts,ylab="US Dollars",
main="Average Daily Purchase Price of a Bitcoin in USD") %>%
dySeries("V1",label="Avg Price (USD)") %>%
dyRangeSelector(dateWindow = c("2012-01-01","2016-07-25"))

#   This series tells the story of bitcoin so far: a long slow start
#   then a rocket ride to a high followed by a roller coaster ride down
#   to a what looks like it might be a plateau of respectability stability.
#
#   Finally note that there R some packages to help explore Bitcoin.
#   Rbitcoin provides a unified API interface to the bitstamp, kraken,
#   btce and bitmarket sites while rbitcoinchartsapi provides an interface
#   to the BitCoinCharts.com API. For a nice example of what the Rbitcoin
#   package can do have a look at Benedikt Koehler's post from earlier this year.

