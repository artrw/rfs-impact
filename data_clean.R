
###key info
avkey <- "HNEQIPE3SQDIM2Z5" #key for Alpha Vantage


if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

rins2013 <- read.csv(file="raw_data/2013 RINs OPIS.csv", header=TRUE)
rins2015 <- read.csv(file="raw_data/2015 RINs OPIS.csv", header=TRUE)

library(lubridate)

basic_cleaning <- function(rins){
  rins %>%
    mutate(Date=mdy(Date)) %>%
    filter(!is.na(Date))
}

rins2013 <- basic_cleaning(rins2013)
rins2015 <- basic_cleaning(rins2015)

library(jsonlite)
library(magrittr)

petroTickers <- c("VLO", "MPC", "XOM", "PSX", "CVX", "BP",
                  "HFC", "RDS-A", "CG", "TOT", "ANDV", "WNR")
bioTickers   <- c("ADM", "ANDE", "PEIX", "GPRE", "CZZ",
                  "FF", "GEVO", "MEIL", "NTOIY", "REGI", "SZYM")
stocks       <- c(petroTickers, bioTickers)

stockGrabber <- function(stock){
  APIcall <- paste0("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=",
  stock, "&outputsize=full&apikey=", avkey, "&datatype=csv")
  stockdata <- read_csv(APIcall, col_names=TRUE,
                        col_types="Dnnnnnnnn") %>%
    select(Date=timestamp, Price=adjusted_close)
  colnames(stockdata)[2] <- stock
  return(stockdata)
}

data2015 <- rins2015
data2013 <- rins2013
rm(rins2013, rins2015)

for (i in seq_along(stocks)){
 stockprice <- stockGrabber(stocks[i])
 data2015 <- left_join(data2015, stockprice, by="Date")
 data2013 <- left_join(data2013, stockprice, by="Date")
 print(paste(stocks[i], "loaded successfully"))
 Sys.sleep(12)
 rm(stockprice)
}
rm(i)




