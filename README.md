# AJAX_MVP

if (!require("pacman")) install.packages("pacman")
pacman::p_load(timeSeries,fPortfolio,quantmod,caTools,Quandl,PerformanceAnalytics,fAssets)
library(timeSeries); library(fPortfolio); library(quantmod); library(caTools);library(PerformanceAnalytics)# may also 

"OPTIMIZE YOUR PORTFOLIO"
library("devtools")
library(quantmod)
library(Quandl)

#load in the russel 2000 stocks
setwd("C:/Users/jroac_000/Desktop/START-UP")
russell <- read.csv("russell_2000_.csv")
rus2000<- as.vector(russell$TICKER) #put into a vector so we can request

rus2000 <- rus2000[-906] #the reason I have do this individually is because there are tickers in the string that the API can't pull, thus stoppping the process
ClosingPricesRead <- NULL
for (Ticker in rus2000)
  ClosingPricesRead <- cbind(ClosingPricesRead,
                             getSymbols(Ticker, src = "google",from = "2017-06-01")) 
ClosingPrices <- ClosingPricesRead[apply(ClosingPricesRead,1,function(x) all(!is.na(x))),]
returns <- as.timeSeries(tail(ClosingPrices,-1) / as.numeric(head(ClosingPrices,-1)) - 1)

# DIVERSIFICATION: time to pick the lowest correlation values stocks.
install.packages("corrplot")
#ALL_CORR<- all_correlations(returns,sorted="strength",type="spearman")
library(corrplot)
#TEMP <- data.frame( t(as.matrix(RESULTS2[,2:12])) )
#names(TEMP) <- sectors
correlations <- abs(cor(returns))
row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
correlations<- correlations[row_indic ,row_indic ]
corrplot(correlations[,-13], method="square", title = "Industrial Sector")
top_rank_industrial_corr <- as.data.frame(sort(apply(correlations,1,mean)))

#BACK TEST 
getSymbols("SPY", src = "google",from = "2017-01-01")
