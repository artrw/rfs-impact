#system's location for Ghostscript--leaving this blank is fine; it just embeds
#fonts for LaTeX figures; figures will just be slightly uglier without it
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.50/bin/gswin64c.exe")


#check if tidyverse and stargazer are installed; loads
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)
if (!require(stargazer)) install.packages("stargazer")
library(stargazer)

#import RIN data from raw
rins2013 <- read.csv(file="raw_data/2013 RINs OPIS.csv", header=TRUE)
rins2015 <- read.csv(file="raw_data/2015 RINs OPIS.csv", header=TRUE)

if(!require(lubridate)) install.packages("lubridate")
library(lubridate)

#reformat dates in RIN data; eliminate dates with missing data (i.e. weekends,
#national holidays, etc.)
BasicCleaning <- function(rins){
  rins %>%
    mutate(Date=mdy(Date)) %>%
    filter(!is.na(Date))
}

rins2013 <- BasicCleaning(rins2013)
rins2015 <- BasicCleaning(rins2015)

#import refinery stock data and reformat dates
petroStocks <- read.csv(file="raw_data/stocks_finaeon_readable.csv", 
                        header=TRUE)
colnames(petroStocks)[1] <- "Date"
petroStocks <- mutate(petroStocks, Date=mdy(Date))

#import covariate data
RUS3000 <- read.csv(file="raw_data/rus3000_finaeon_readable.csv",
                    header=TRUE)
colnames(RUS3000)[1] <- "Date"
RUS3000 <- mutate(RUS3000, Date=mdy(Date))
WTI <- read.csv(file="raw_data/wti_finaeon_readable.csv", header=TRUE)
colnames(WTI)[1] <- "Date"
WTI <- mutate(WTI, Date=mdy(Date))

#join together RIN, stock, and covariate data
data2013 <- left_join(rins2013, petroStocks, by="Date")
data2015 <- left_join(rins2015, petroStocks, by="Date")
data2013 <- left_join(data2013, RUS3000, by="Date")
data2015 <- left_join(data2015, RUS3000, by="Date")
data2013 <- left_join(data2013, WTI, by="Date")
data2015 <- left_join(data2015, WTI, by="Date")
rm(rins2013, rins2015, petroStocks, RUS3000, WTI)

#Backfilling two NAs in RUS3000 data
data2015$RUS3000[data2015$Date == as.Date("2014-12-31")] <- 
  data2015$RUS3000[data2015$Date == as.Date("2014-12-30")]
data2015$RUS3000[data2015$Date == as.Date("2015-01-30")] <- 
  data2015$RUS3000[data2015$Date == as.Date("2015-01-29")]

#modifying date extent of data
data2013 <- data2013[data2013$Date < "2014-05-15",]
data2015 <- data2015[data2015$Date < "2016-05-18",]
data2013$t <- as.numeric(rownames(data2013[order(data2013$Date),]))
data2015$t <- as.numeric(rownames(data2015[order(data2015$Date),]))

petroTickers <- c("VLO", "MPC", "XOM", "PSX", "CVX", "BP",
                  "HFC", "RDS.A", "CG", "TOT", "ANDV", "WNR")
bioTickers   <- c("ADM", "ANDE", "PEIX", "GPRE", "CZZ",
                  "FF", "GEVO", "MEIL", "NTOIY", "REGI", "SZYM")
stocks       <- c(petroTickers, bioTickers)

dates2015 <- as.Date.character(c("2015-05-29", "2015-11-30"))



#stationarity testing
if (!require(tseries)) install.packages("tseries")
library(tseries)
LeeStrazicichTest <- function(data, series, dates) {
  # Implements the exogenous LM Unit Root test with two structural breaks
  # from Lee & Strazicich 2003 REStat 85(4)
  # Args:
  #   data: Name of the data.frame holding the series and a Date variable
  #   series: Name of the series with the suspected structural break
  #   dates: Vector of exactly two dates where structural breaks are expected
  # Returns: t-statistic from the test
  #  
  if (class(dates) != "Date") {
    stop("Date argument must be of class Date")
  }
  if (length(dates) != 2){
    stop("Lee Strazicich LM test only works for two suspected breaks")
  }
  # End error handling
  data$DU1 <- 0
  data$DU2 <- 0
  data$DT1 <- 0
  data$DT2 <- 0
  dayafter1 <- dates[1] + 1
  # Fixing the fact that sometimes the "next day" has a closed market/no data
  while (!(dayafter1 %in% data$Date)){
    dayafter1 <- dayafter1 + 1
  }
  dayafter2 <- dates[2] + 1
  while (!(dayafter2 %in% data$Date)){
    dayafter2 <- dayafter2 + 1
  }
  data[data$Date >= (dayafter1), ]$DU1 <- 1
  data[data$Date >= (dayafter2), ]$DU2 <- 1
  data[data$Date >= (dayafter1), ]$DT1 <- (data[data$Date >= (dayafter1), ]$t-
                                             data[data$Date == dates[1],]$t)
  data[data$Date >= (dayafter2), ]$DT2 <- (data[data$Date >= (dayafter2), ]$t-
                                             data[data$Date == dates[2],]$t)
  data$ylag   <- c(NA, series[1:(length(series)-1)])
  data$ydiff  <- series-data$ylag
  data$DU1lag <- c(NA, data$DU1[1:(length(data$DU1)-1)])
  data$DU2lag <- c(NA, data$DU2[1:(length(data$DU2)-1)])
  data$DT1lag <- c(NA, data$DT1[1:(length(data$DT1)-1)])
  data$DT2lag <- c(NA, data$DT2[1:(length(data$DT2)-1)])
  data$DU1diff <- data$DU1-data$DU1lag
  data$DU2diff <- data$DU2-data$DU2lag
  data$DT1diff <- data$DT1-data$DT1lag
  data$DT2diff <- data$DT2-data$DT2lag
  deltareg <- lm(ydiff ~ 0 + DU1diff + DU2diff + DT1diff + DT2diff, data=data)
  psi <- series[1]-(deltareg$coefficients[1]*data$DU1[1] +
                      deltareg$coefficients[2]*data$DU2[1] +
                      deltareg$coefficients[3]*data$DT1[1] +
                      deltareg$coefficients[4]*data$DT2[1])
  data$Slag <- data$ylag-psi-(deltareg$coefficients[1]*data$DU1lag +
                                deltareg$coefficients[2]*data$DU2lag +
                                deltareg$coefficients[3]*data$DT1lag +
                                deltareg$coefficients[4]*data$DT2lag)
  testreg <- lm(ydiff ~ 0 + DU1diff + DU2diff + DT1diff + DT2diff +Slag,
                data=data)
  tstat <- summary(testreg)[["coefficients"]][,"t value"][5]
  return(tstat)
}
StationarityTest <- function(data, series, dates, summarystats=TRUE){
  if (class(dates) != "Date") {
    stop("Date vector must be of class date")
  }
  if (class(series) != "character") {
    stop("Series vector must be of class character (variables names only)")
  }
  rows <- length(series)
  if (length(dates) == 2){
    results <- data.frame(ADF=rep(NA, rows),
                          KPSS=rep(NA, rows),
                          LS=rep(NA, rows), row.names=series)
  } else {
    print("Not conducting Lee-Strazicich test because length(dates)!=2")
    results <- data.frame(ADF=rep(NA, rows),
                          KPSS=rep(NA, rows))
  }
  for (i in 1:length(series)){
    ts <- ts(data[, colnames(data) == series[i]])
    adf.result <- adf.test(ts)
    results[i, 1] <- as.numeric(adf.result[1])
    results[i, 1] <- paste0("dollarsign", round(as.numeric(results[i, 1]), 3))
    if (adf.result[4] < 0.01){
      results[i, 1] <- paste0(results[i, 1], "threestardollarsign")
    } else if (adf.result[4] < 0.05){
      results[i, 1] <- paste0(results[i, 1], "twostardollarsign")
    } else if (adf.result[4] < 0.1){
      results[i, 1] <- paste0(results[i, 1], "onestardollarsign")
    } else {
      results[i, 1] <- paste0(results[i, 1], "dollarsign")
    }
    kpss.result <- kpss.test(ts)
    results[i, 2] <- as.numeric(kpss.result$statistic)
    results[i, 2] <- paste0("dollarsign", round(as.numeric(results[i, 2]), 3))
    if (kpss.result[3] <= 0.01){ #KPSS does not report p values < 0.01
      results[i, 2] <- paste0(results[i, 2], "threestardollarsign")
    } else if (kpss.result[3] < 0.05){
      results[i, 2] <- paste0(results[i, 2], "twostardollarsign")
    } else if (kpss.results[3] < 0.1){
      results[i, 2] <- paste0(results[i, 2], "onestardollarsign")
    } else {
      results[i, 2] <- paste0(results[i, 2], "dollarsign")
    }
    if (length(dates) == 2){
      if (length(ts) < 100){
        print("WARNING: LS test critical values assume T=100")
      }
      ls.result <- LeeStrazicichTest(data, ts, dates)
      results[i, 3] <- paste0("dollarsign", round(as.numeric(ls.result), 3))
      if (ls.result < -4.76){
        results[i, 3] <- paste0(results[i, 3], "threestardollarsign")
      } else if (ls.result < -4.19){
        results[i, 3] <- paste0(results[i, 3], "twostardollarsign")
      } else if (ls.result < -3.88){
        results[i, 3] <- paste0(results[i, 3], "onestardollarsign")
      } else {
        results[i, 3] <- paste0(results[i, 3], "dollarsign")
      }
    }
  }
  if (summarystats == TRUE){
    sumstats <- data.frame(Mean=rep(NA, rows),
                           StDev=rep(NA, rows),
                           N=rep(NA, rows))
    for (i in 1:length(series)){
      variable <- data[, colnames(data) == series[i]]
      sumstats[i, 1] <- paste0("dollarsign", round(mean(variable), 3), 
                               "dollarsign") 
      sumstats[i, 2] <- paste0("dollarsign", round(sd(variable), 3), 
                               "dollarsign") 
      sumstats[i, 3] <- paste0("dollarsign", round(length(variable), 3), 
                               "dollarsign") 
    }
    results <- cbind(sumstats, results)
  }
  return(results)
}
stargazer(StationarityTest(data2015, c("D6", "D5", "D4", petroTickers), 
          dates2015), summary=FALSE, type="latex", 
          out="tables/stationarity_raw.tex")



#Event study modelling
EventStudy <- function(data, series, dates, lags=0, poly.order=4, start=NULL,
                       end=NULL, covariates=NULL, monthday=TRUE, CI=FALSE){
  if (!("Date" %in% colnames(data))){
    stop("Data input must have a date column titled 'Date'")
  }
  if (!("t" %in% colnames(data))){
    stop("Data input must have a time column titled 't'")
  }
  if (poly.order < 1){
    stop("Polynomial order must be at least one")
  }
  if ((min(dates) <= min(data$Date)) | (max(dates) >= max(data$Date))){
    stop("Data extent does not match extent of dates provided")
  }
  #End error handling
  lags <- as.integer(lags)
  if (!is.null(start)){
    data <- data[data$Date >= as.Date.character(start), ]
  }
  if (!is.null(start)){
    data <- data[data$Date <= as.Date.character(end), ]
  }
  #Creaing event dummies
  event.dummies <- data.frame(matrix(0, nrow=length(data$Date), 
                                     ncol=(length(dates)*(lags+1))))
  for (d in 1:length(dates)){
    row <- data$t[data$Date == dates[d]]
    for (l in 0:lags){
      col <- ((d-1)*(lags+1))+l+1
      colnames(event.dummies)[col] <- paste0("event_", d, "_lag_", l)
      event.dummies[(row+l), col] <- 1
    }
  }
  event.dummies$Date <- data$Date
  data <- merge(data, event.dummies, by="Date")
  #Log-differencing series and covariates
  dl.series <- data[, colnames(data) %in% c(series, covariates)]
  if (is.vector(dl.series) == TRUE){
    dl.series <- c(NA, diff(log(dl.series)))
    dl.series <- data.frame(placeholder=dl.series, Date=data$Date)
    colnames(dl.series) <- c(paste0("dl.", series), "Date")
  } else {
    for (vec in 1:(dim(dl.series)[2])){
      dl.series[, vec] <- c(NA, diff(log(dl.series[, vec])))
    }
    colnames(dl.series) <- paste0("dl.", colnames(dl.series))
    dl.series$Date <- data$Date
  }
  data <- merge(data, dl.series, by="Date")
  #Adding polynomials of time
  poly.order <- as.integer(poly.order)
  time.polynomials <- data.frame(Date=data$Date)
  if (poly.order > 1){
    for (num in 2:poly.order){
      time.polynomials[, num] <- (data$t)^num
      colnames(time.polynomials)[num] <- paste0("t", num)
    }
  data <- merge(data, time.polynomials, by="Date")
  }
  #Month/day fixed effects
  if (monthday == TRUE){
    data$month <- month(data$Date)
    data$day <- wday(data$Date)
  }
  #Data reshape and modelling
  longdata <- reshape(data,
                      varying=paste0("dl.", series),
                      v.names="price",
                      timevar="series",
                      times=paste0("dl.", series),
                      direction="long")
  longdata <- longdata[!is.na(longdata$price), ]
  #Brief explanation of modelling procedure for quick collection of numerous
  #models: use code to determine all possible variables that will be included
  #in a given model, create a new, temporary dataset that includes only those
  #variables, then pass them to an lm() function using the . functionality,
  #which allows for the automatic inclusion of every variable in a dataset;
  #then compute standard errors as described in the paper
  if (monthday == TRUE){
    vars <- c("price", colnames(time.polynomials), paste0("dl.", covariates),
              colnames(event.dummies), "day", "month")
    vars <- vars[vars != "Date"]
    regdata <- longdata[, colnames(longdata) %in% vars]
    reg <- lm(price ~ factor(day) + factor(month) + ., data=regdata)
  } else {
    vars <- c("price", colnames(time.polynomials), paste0("dl.", covariates),
              colnames(event.dummies))
    vars <- vars[vars != "Date"]
    regdata <- longdata[, colnames(longdata) %in% vars]
    reg <- lm(price ~ ., data=regdata)
  }
  regdata$event <- rowSums(regdata[, colnames(regdata) %in% 
                                     colnames(event.dummies)])
  rownames(regdata) <- 1:(length(regdata$event))
  eventIDs <- as.integer(rownames(regdata[regdata$event == 1, ]))
  resids <- unname(reg$residuals)
  resids <- resids[-eventIDs]
  resids <- resids[order(resids)]
  numresids <- length(resids)
  SQ.lower.01 <- (resids[floor(0.005*numresids)])
  SQ.upper.01 <- (resids[ceiling(0.995*numresids)])
  SQ.lower.05 <- (resids[floor(0.025*numresids)])
  SQ.upper.05 <- (resids[ceiling(0.975*numresids)])
  SQ.lower.1  <- (resids[floor(0.05*numresids)])
  SQ.upper.1  <- (resids[ceiling(0.95*numresids)])
  event.coefs <- colnames(event.dummies)[colnames(event.dummies) != "Date"]
  results <- as.numeric(reg$coefficients[event.coefs])
  for (i in 1:(length(results))){
    pointest <- as.numeric(results[i])
    if (pointest <= SQ.lower.01 | pointest >= SQ.upper.01){
      results[i] <- paste0("dollarsign", round(pointest, 3), 
                               "threestardollarsign")
    } else if (pointest <= SQ.lower.05 | pointest >= SQ.upper.05){
      results[i] <- paste0("dollarsign", round(pointest, 3), 
                               "twostardollarsign")
    } else if (pointest <= SQ.lower.1 | pointest >= SQ.upper.1){
      results[i] <- paste0("dollarsign", round(pointest, 3), 
                               "onestardollarsign")
    } else {
      results[i] <- paste0("dollarsign", round(pointest, 3), 
                               "dollarsign")
    }
  }
  if (CI == TRUE) {
    results <- c(results, paste0("dollarsign(", round(SQ.lower.01, 3), ", ",
                                 round(SQ.upper.01, 3), ")dollarsign"),
                          paste0("dollarsign(", round(SQ.lower.05, 3), ", ",
                                 round(SQ.upper.05, 3), ")dollarsign"),
                          paste0("dollarsign(", round(SQ.lower.1, 3), ", ",
                                 round(SQ.upper.1, 3), ")dollarsign"), 
                          paste0("dollarsign", length(series), "dollarsign"),
                          paste0("dollarsign", dim(regdata)[1], "dollarsign"))
                          #last two are number of firms and observations
  } else {
    results <- c(results, paste0("dollarsign", length(series), "dollarsign"),
                          paste0("dollarsign", dim(regdata)[1], "dollarsign"))
  }
  return(results)
}
stargazer(cbind(c("Lag 0", "Lag 1", "Lag 2", "Lag 3", "Lag 4", "Lag 5", "Lag 6",
                  "Lag 0", "Lag 1", "Lag 2", "Lag 3", "Lag 4", "Lag 5", "Lag 6",
                  "SQ 1% Bounds", "SQ 5% Bounds", "SQ 10% Bounds", "Firms", "N"),
                EventStudy(data2015, c("CVX", "XOM", "TOT", "BP", "RDS.A"),
                    dates2015, lags=6, poly.order=4, 
                    covariates="RUS3000", monthday=TRUE, CI=TRUE),
                EventStudy(data2015, c("VLO", "MPC", "PSX"),
                    dates2015, lags=6, poly.order=4, 
                    covariates="RUS3000", monthday=TRUE, CI=TRUE),
                EventStudy(data2015, c("CG", "WNR", "HFC", "ANDV"),
                    dates2015, lags=6, poly.order=4, 
                    covariates="RUS3000", monthday=TRUE, CI=TRUE),
                EventStudy(data2015, petroTickers, 
                    dates2015, lags=6, poly.order=4,  
                    covariates="RUS3000", monthday=TRUE, CI=TRUE)),
             summary=FALSE, type="latex", out="tables/event_studies_raw.tex")



#cointegration testing
if (!require(urca)) install.packages("urca")
library(urca)
if (!require(vars)) install.packages("vars")
library(vars)
CointegrationTest <- function(data, firms){
  #Runs Johansen Trace & Eigenvalue tests for cointegration for every pairwise
  #combination of RINs (D6, D5, D4) and firms and returns a LaTeX table
  {
    if (class(data) != "data.frame"){
      stop("Data must be of class data.frame")
    }
    if (class(firms) != "character"){
      stop("Vector of firms must be of class character")
    }
    if (FALSE %in% (firms %in% colnames(data))){
      stop("No data exists for a subset of firms")
    }
  }
  #End error handling
  results <- data.frame(D6e=character(length=length(firms)), 
                        D5e=".", D4e=".", D6t=".", D5t=".", D4t=".", 
                        row.names=firms, stringsAsFactors=FALSE)
  rins <- c("D6", "D5", "D4", "D6", "D5", "D4")
  for (i in 1:(length(firms))){
    firm <- data[firms[i]]
    for (t in 1:3){
      rin <- data[rins[t]]
      pair <- cbind(firm, rin)
      ord.opt <- VARselect(diff(as.matrix(pair)), type="const")
      ord.choice <- as.integer(max(ord.opt$selection[1], 2))
      #(order must be at least 2 for use of ca.jo function)
      eigen.test <- ca.jo(pair, type="eigen", ecdet="const",
                          K=ord.choice, spec="transitory")
      eigen.stat <- round(eigen.test@teststat[length(eigen.test@teststat)], 3)
      if (eigen.stat > eigen.test@cval[length(eigen.test@teststat), 3]){
        results[i, t] <- paste0("dollarsign", eigen.stat, "threestardollarsign")
      } else if (eigen.stat > eigen.test@cval[length(eigen.test@teststat), 2]){
        results[i, t] <- paste0("dollarsign", eigen.stat, "twostardollarsign")
      } else if (eigen.stat > eigen.test@cval[length(eigen.test@teststat), 1]){
        results[i, t] <- paste0("dollarsign", eigen.stat, "onestardollarsign")
      } else {
        results[i, t] <- paste0("dollarsign", eigen.stat, "dollarsign")
      }
      trace.test <- ca.jo(pair, type="trace", ecdet="const",
                          K=ord.choice, spec="transitory")
      trace.stat <- round(trace.test@teststat[length(trace.test@teststat)], 3)
      if (trace.stat > trace.test@cval[length(trace.test@teststat), 3]){
        results[i, t+3] <- paste0("dollarsign", trace.stat, 
                                  "threestardollarsign")
      } else if (trace.stat > trace.test@cval[length(trace.test@teststat), 2]){
        results[i, t+3] <- paste0("dollarsign", trace.stat, "twostardollarsign")
      } else if (trace.stat > trace.test@cval[length(trace.test@teststat), 1]){
        results[i, t+3] <- paste0("dollarsign", trace.stat, "onestardollarsign")
      } else {
        results[i, t+3] <- paste0("dollarsign", trace.stat, "dollarsign") 
      }
    }
  }
  return(results)
}
stargazer(CointegrationTest(data2015, petroTickers),
          summary=FALSE, type="latex", out="tables/cointegration_raw.tex")


#Time series modelling   
TimeSeries <- function(data, series, firm, order=NULL, covariates=NULL){
  d.series <- data[, colnames(data) == as.character(series)]
  d.firm   <- data[, colnames(data) == as.character(firm)]
  if (length(covariates) > 0){
    d.covariates <- data[, colnames(data) %in% covariates]
    x <- cbind(d.series, d.firm, d.covariates)
    colnames(x) <- c(series, firm, covariates)
  } else {
    x <- cbind(d.series, d.firm)
    colnames(x) <- c(series, firm)
  }
  xdiff <- diff(x)
  if (is.null(order)){
    ord <- VARselect(xdiff, type="const")
    ord.choice <<- unname(ord$selection[1])
    #using AIC (BIC not available)
  } else {
    ord.choice <<- order
  } #ord.choice must be assigned to global evn b/c of scoping error in {vars}
  model <- VAR(xdiff, p=ord.choice, type="const")
  return(model)
}
TimeSeriesTable <- function(data, rin, firms, lags=10, order=NULL, 
                            covariates=NULL){
  results <- data.frame(matrix(NA, ncol=length(firms), nrow=lags+5))
  colnames(results) <- firms
  rownames(results) <- c("Constant", paste("Lag", 1:lags), "N", "Ljung-Box",
                         "Jarque-Bera", "Shapiro-Wilk")
  for (i in 1:length(firms)){
    model <- TimeSeries(data, rin, firms[i], order=order, covariates=covariates)
    varnum <- 2 + length(covariates)
    evaltext <- paste0("summary(model)$varresult$", firms[i], "$coefficients")
    var.results <- eval(parse(text=evaltext))
    results[1, i] <- paste0("dollarsign", round(var.results["const", 1], 3))
    if (var.results["const", 4] <= 0.01){
      results[1, i] <- paste0(results[1, i], "threestardollarsign")
    } else if (var.results["const", 4] <= 0.05){
      results[1, i] <- paste0(results[1, i], "twostardollarsign")
    } else if (var.results["const", 4] <= 0.1){
      results[1, i] <- paste0(results[1, i], "onestardollarsign")
    } else {
      results[1, i] <- paste0(results[1, i], "dollarsign")
    }
    for (n in 1:((nrow(var.results)-1)/varnum)){
      results[n+1, i] <- paste0("dollarsign", 
                                round(var.results[(1+varnum*(n-1)), 1], 3))
      if (var.results[(1+varnum*(n-1)), 4] <= 0.01){
        results[n+1, i] <- paste0(results[n+1, i], "threestardollarsign")
      } else if (var.results[(1+varnum*(n-1)), 4] <= 0.05){
        results[n+1, i] <- paste0(results[n+1, i], "twostardollarsign")
      } else if (var.results[(1+varnum*(n-1)), 4] <= 0.1){
        results[n+1, i] <- paste0(results[n+1, i], "onestardollarsign")
      } else {
        results[n+1, i] <- paste0(results[n+1, i], "dollarsign")
      }
    }
    results[2+lags, i] <- paste0("dollarsign", model$obs, "dollarsign")
    evaltext2 <- paste0("model$varresult$", firms[i], "$residuals")
    resids <- eval(parse(text=evaltext2))
    lbtest <- Box.test(resids, type="Ljung-Box")
    jbtest <- jarque.bera.test(resids)
    swtest <- shapiro.test(resids)
    results[3+lags, i] <- paste0("dollarsign", round(lbtest$statistic, 3))
    if (lbtest$p.value <= 0.01){
      results[3+lags, i] <- paste0(results[3+lags, i], "threestardollarsign")
    } else if (lbtest$p.value <= 0.05){
      results[3+lags, i] <- paste0(results[3+lags, i], "twostardollarsign")
    } else if (lbtest$p.value <= 0.1){
      results[3+lags, i] <- paste0(results[3+lags, i], "onestardollarsign")
    } else {
      results[3+lags, i] <- paste0(results[3+lags, i], "dollarsign")
    }
    results[4+lags, i] <- paste0("dollarsign", round(jbtest$statistic, 3))
    if (jbtest$p.value <= 0.01){
      results[4+lags, i] <- paste0(results[4+lags, i], "threestardollarsign")
    } else if (jbtest$p.value <= 0.05){
      results[4+lags, i] <- paste0(results[4+lags, i], "twostardollarsign")
    } else if (jbtest$p.value <= 0.1){
      results[4+lags, i] <- paste0(results[4+lags, i], "onestardollarsign")
    } else {
      results[4+lags, i] <- paste0(results[4+lags, i], "dollarsign")
    }
    results[5+lags, i] <- paste0("dollarsign", round(swtest$statistic, 3))
    if (swtest$p.value <= 0.01){
      results[5+lags, i] <- paste0(results[5+lags, i], "threestardollarsign")
    } else if (swtest$p.value <= 0.05){
      results[5+lags, i] <- paste0(results[5+lags, i], "twostardollarsign")
    } else if (swtest$p.value <= 0.1){
      results[5+lags, i] <- paste0(results[5+lags, i], "onestardollarsign")
    } else {
      results[5+lags, i] <- paste0(results[5+lags, i], "dollarsign")
    }
  }
  return(results)
}

stargazer(TimeSeriesTable(data2015, "D6", petroTickers, lags=4),
          summary=FALSE, type="latex", out="tables/D6VAR_raw.tex")
stargazer(TimeSeriesTable(data2015, "D5", petroTickers, lags=6),
          summary=FALSE, type="latex", out="tables/D5VAR_raw.tex")
stargazer(TimeSeriesTable(data2015, "D4", petroTickers, lags=4),
          summary=FALSE, type="latex", out="tables/D4VAR_raw.tex")

stargazer(TimeSeriesTable(data2015, "D6", petroTickers, lags=6, order=6),
          summary=FALSE, type="latex", out="tables/D6VAR6lags_raw.tex")
stargazer(TimeSeriesTable(data2015, "D5", petroTickers, lags=6, order=6),
          summary=FALSE, type="latex", out="tables/D5VAR6lags_raw.tex")
stargazer(TimeSeriesTable(data2015, "D4", petroTickers, lags=6, order=6),
          summary=FALSE, type="latex", out="tables/D4VAR6lags_raw.tex")


if(!require(extrafont)) install.packages("extrafont")
library(extrafont)
if(!require(fontcm)) install.packages("fontcm")
library(fontcm)
font_install('fontcm')
loadfonts(device="win")

#plot of 2015 RIN data
pdf("figures/2015RINs.pdf", family="CM Roman", w=6, h=3.5)
ggplot(data2015, aes(x=Date)) +
  geom_line(aes(y=D6, col="D6")) +
  geom_line(aes(y=D5, col="D5")) +
  geom_line(aes(y=D4, col="D4")) +
  geom_vline(xintercept=c(as.Date("2015-05-29"), as.Date("2015-11-30")),
             linetype="dotted") +
  geom_text(aes(x=as.Date("2015-05-29"), label="May 29", y=1.1)) +
  geom_text(aes(x=as.Date("2015-11-30"), label="November 30", y=1.1), 
             angle=0) +
  labs(y="Price") + 
  scale_y_continuous(breaks=c(.5, 1, 1.5), label=c("$0.50", "$1.00", "$1.50")) +
  scale_x_date(breaks=c(as.Date("2015-01-01"), as.Date("2016-01-01")),
                    label=c("2015", "2016")) +
  scale_color_manual(name = "RIN Legend", values = c("D6"="Black", "D5"="Blue",
                                                     "D4"="Red"),
                     guide = guide_legend(reverse=TRUE)) +
  theme(text=element_text(size=11, family='CM Roman', face="plain"), 
        axis.text=element_text(size=11),
        legend.justification=c(1, 0),
        legend.position = c(0.95, 0.05),
        panel.background = element_blank(),
        axis.line=element_line(color="Black"),
        legend.key=element_blank())
dev.off()
embed_fonts("figures/2015RINs.pdf")


#plot of 2013 RIN data
pdf("figures/2013RINs.pdf", family="CM Roman", w=6, h=3.5)
ggplot(data2013, aes(x=Date)) +
  geom_line(aes(y=D6, col="D6")) +
  geom_line(aes(y=D5, col="D5")) +
  geom_line(aes(y=D4, col="D4")) +
  geom_vline(xintercept=c(as.Date("2013-08-06"), as.Date("2013-10-11"),
                          as.Date("2013-11-15")), linetype="dotted") +
  geom_text(aes(x=as.Date("2013-08-06"), label="August 6", y=1.6), angle=20) +
  geom_text(aes(x=as.Date("2013-10-11"), label="October 11", y=1.56), angle=20) +
  geom_text(aes(x=as.Date("2013-11-15"), label="November 15", y=1.51), 
             angle=20) +
  labs(y="Price") + 
  scale_y_continuous(breaks=c(.5, 1, 1.5), label=c("$0.50", "$1.00", "$1.50")) +
  scale_x_date(breaks=c(as.Date("2013-01-01"), as.Date("2014-01-01")),
               label=c("2013", "2014")) +
  scale_color_manual(name = "RIN Legend", values = c("D6"="Black", "D5"="Blue",
                                                     "D4"="Red"),
                     guide = guide_legend(reverse=TRUE)) +
  theme(text=element_text(size=11, family='CM Roman', face="plain"), 
        axis.text=element_text(size=11),
        legend.justification=c(1, 0),
        legend.position = c(0.95, 0.05),
        panel.background = element_blank(),
        axis.line=element_line(color="Black"),
        legend.key=element_blank())
dev.off()
embed_fonts("figures/2013RINs.pdf")

IrfMaker <- function(data, rin, firm, n.ahead=20){
  irf <- irf(TimeSeries(data, rin, firm), n.ahead=n.ahead)
  data.frame(point=eval(parse(text=paste0("irf$irf$", rin, "[, 2]"))),
             lower=eval(parse(text=paste0("irf$Lower$", rin, "[, 2]"))),
             upper=eval(parse(text=paste0("irf$Upper$", rin, "[, 2]")))) %>%
    ggplot(aes(x=0:n.ahead)) +
      geom_line(aes(y=point), color="Black") +
      geom_line(aes(y=lower), color="Red") +
      geom_line(aes(y=upper), color="Red") +
      geom_ribbon(aes(x=0:n.ahead, ymax=upper, ymin=lower, 
                      fill="Red", alpha=.1)) +
      geom_hline(yintercept=0, color="Black") +
      labs(y=firm, x="Lags") +
      theme(text=element_text(size=8, family='CM Roman', face="plain"), 
            axis.text=element_text(size=8),
            axis.title=element_text(size=8),
            panel.background = element_blank(),
            axis.line=element_line(color="Black"),
            legend.position="none")
  
}

for (firm in petroTickers){
  path <- paste0("figures/IRFD5", firm, ".pdf")
  pdf(path, family="CM Roman", w=3, h=2)
  plot(IrfMaker(data2015, "D5", firm))
  dev.off()
  embed_fonts(path)
  rm(path)
}

#Robustness checks mentioned but not reported
EventStudy(data2015, c("CVX", "XOM", "TOT", "BP", "RDS.A"),
           dates2015, lags=6, poly.order=4, 
           covariates="WTI", monthday=TRUE, CI=TRUE)
EventStudy(data2015, c("VLO", "MPC", "PSX"),
           dates2015, lags=6, poly.order=4, 
           covariates="WTI", monthday=TRUE, CI=TRUE)
EventStudy(data2015, c("CG", "WNR", "HFC", "ANDV"),
           dates2015, lags=6, poly.order=4, 
           covariates="WTI", monthday=TRUE, CI=TRUE)
EventStudy(data2015, petroTickers, 
           dates2015, lags=6, poly.order=4,  
           covariates="WTI", monthday=TRUE, CI=TRUE)
TimeSeriesTable(data2015, "D5", petroTickers, order=6, covariates="RUS3000")
TimeSeriesTable(data2015, "D5", petroTickers, order=6, covariates="WTI")