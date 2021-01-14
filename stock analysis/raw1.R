#Three stock symbols

symbols <- c("AMZN", "AAPL", "NFLX")

#Dictionaries needed to work with stock prices

library(TTR); library(quantmod); library(PerformanceAnalytics); library(dplyr);
library(ggplot2); library(gridExtra); library(ggthemes);

#getting data for Amazon, Apple and Netflix

getSymbols(symbols)

amazon <- AMZN
apple <- AAPL
netflix <- NFLX

new_cols <- c("open", "high", "low", "close", "volume", "adjusted")

dataclean <- function(df){
  #Calculating the following returns: Open to Close, Open to Open, Open to High, Open to Low,
  # High to Close, Low to Close, Low to High

  df$op_cl <- OpCl(df)
  df$op_op <- OpOp(df)
  df$op_hi <- OpHi(df)
  df$op_lo <- OpLo(df)
  df$hi_cl <- HiCl(df)
  df$lo_cl <- LoCl(df)
  df$lo_hi <- LoHi(df)

  #Calculating Open to close return percentages over 1day, 7day, 30day periods

  df$p_op_cl_1 <- Delt(Op(df), Cl(df), k = 1)
  df$p_op_cl_2 <- Delt(Op(df), Cl(df), k = 2)
  df$p_op_cl_3 <- Delt(Op(df), Cl(df), k = 3)

  #Addng columns with lagged close values

  df$lag_cl_1 <- Lag(Cl(df))
  df$lag_cl_2 <- Lag(Cl(df), 2)
  df$lag_cl_3 <- Lag(Cl(df), 3)

  #Move up OpCl by one period

  df$next_op_cl <- Next(OpCl(df))

  return(df)
}

amazon <- dataclean(amazon)
apple <- dataclean(apple)
netflix <- dataclean(netflix)

#Calculating weekly, monthly and quartlerly returns

amazon_returns <- allReturns(amazon)
apple_returns <- allReturns(apple)
netflix_returns <- allReturns(netflix)

#Let us look at SD and kurtosis for daily, weekly, monthly and quarterly returns

adsd <- sd(amazon_returns$daily, na.rm = TRUE)
awsd <- sd(amazon_returns$weekly, na.rm = TRUE) 
amsd <- sd(amazon_returns$monthly, na.rm = TRUE)
aqsd <- sd(amazon_returns$quarterly, na.rm = TRUE)

adk <- kurtosis(amazon_returns$daily, na.rm = TRUE)
awk <- kurtosis(amazon_returns$weekly, na.rm = TRUE)
amk <- kurtosis(amazon_returns$monthly, na.rm = TRUE)
aqk <- kurtosis(amazon_returns$quarterly, na.rm = TRUE)

apdsd <- sd(apple_returns$daily, na.rm = TRUE)
apwsd <- sd(apple_returns$weekly, na.rm = TRUE) 
apmsd <- sd(apple_returns$monthly, na.rm = TRUE)
apqsd <- sd(apple_returns$quarterly, na.rm = TRUE)

apdk <- kurtosis(apple_returns$daily, na.rm = TRUE)
apwk <- kurtosis(apple_returns$weekly, na.rm = TRUE)
apmk <- kurtosis(apple_returns$monthly, na.rm = TRUE)
apqk <- kurtosis(apple_returns$quarterly, na.rm = TRUE)

ndsd <- sd(netflix_returns$daily, na.rm = TRUE)
nwsd <- sd(netflix_returns$weekly, na.rm = TRUE) 
nmsd <- sd(netflix_returns$monthly, na.rm = TRUE)
nqsd <- sd(netflix_returns$quarterly, na.rm = TRUE)

ndk <- kurtosis(netflix_returns$daily, na.rm = TRUE)
nwk <- kurtosis(netflix_returns$weekly, na.rm = TRUE)
nmk <- kurtosis(netflix_returns$monthly, na.rm = TRUE)
nqk <- kurtosis(netflix_returns$quarterly, na.rm = TRUE)

##AMAZON
#plots to visually observe comparison with normal curves
r1 <- ggplot(amazon_returns, aes(x = amazon_returns$daily)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "skyblue", bins = 50) + 
  stat_function(fun = dnorm, color = "red", args = list(mean = mean(amazon_returns$daily, na.rm = TRUE),
                                         sd = sd(amazon_returns$daily, na.rm = TRUE))) +
  theme_minimal() + ggtitle("Daily Returns") + xlab("")

r2 <- ggplot(amazon_returns, aes(x = amazon_returns$weekly)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "skyblue", bins = 50) + 
  stat_function(fun = dnorm, color = "red", args = list(mean = mean(amazon_returns$weekly, na.rm = TRUE),
                                                        sd = sd(amazon_returns$weekly, na.rm = TRUE))) +
  theme_minimal() + ggtitle("Weekly Returns") + xlab("")

r3 <- ggplot(amazon_returns, aes(x = amazon_returns$monthly)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "skyblue", bins = 50) + 
  stat_function(fun = dnorm, color = "red", args = list(mean = mean(amazon_returns$monthly, na.rm = TRUE),
                                                        sd = sd(amazon_returns$monthly, na.rm = TRUE))) +
  theme_minimal() + ggtitle("Monthly Returns") + xlab("")

r4 <- ggplot(amazon_returns, aes(x = amazon_returns$quarterly)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "skyblue", bins = 50) + 
  stat_function(fun = dnorm, color = "red", args = list(mean = mean(amazon_returns$quarterly, na.rm = TRUE),
                                                        sd = sd(amazon_returns$quarterly, na.rm = TRUE))) +
  theme_minimal() + ggtitle("Quarterly Returns") + xlab("")

grid.arrange(r1, r2, r3, r4, nrow = 2, ncol = 2,
             heights = c(rep(1000,2)), widths = c(rep(1000,2)), name = "Amazon")

##APPLE
#plots to visually observe comparison with normal curves
r5 <- ggplot(apple_returns, aes(x = apple_returns$daily)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "lightgreen", bins = 50) + 
  stat_function(fun = dnorm, color = "red", args = list(mean = mean(apple_returns$daily, na.rm = TRUE),
                                                        sd = sd(apple_returns$daily, na.rm = TRUE))) +
  theme_minimal() + ggtitle("Daily Returns") + xlab("")

r6 <- ggplot(apple_returns, aes(x = apple_returns$weekly)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "lightgreen", bins = 50) + 
  stat_function(fun = dnorm, color = "red", args = list(mean = mean(apple_returns$weekly, na.rm = TRUE),
                                                        sd = sd(apple_returns$weekly, na.rm = TRUE))) +
  theme_minimal() + ggtitle("Weekly Returns") + xlab("")

r7 <- ggplot(apple_returns, aes(x = apple_returns$monthly)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "lightgreen", bins = 50) + 
  stat_function(fun = dnorm, color = "red", args = list(mean = mean(apple_returns$monthly, na.rm = TRUE),
                                                        sd = sd(apple_returns$monthly, na.rm = TRUE))) +
  theme_minimal() + ggtitle("Monthly Returns") + xlab("")

r8 <- ggplot(apple_returns, aes(x = apple_returns$quarterly)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "lightgreen", bins = 50) + 
  stat_function(fun = dnorm, color = "red", args = list(mean = mean(apple_returns$quarterly, na.rm = TRUE),
                                                        sd = sd(apple_returns$quarterly, na.rm = TRUE))) +
  theme_minimal() + ggtitle("Quarterly Returns") + xlab("")

grid.arrange(r5, r6, r7, r8, nrow = 2, ncol = 2,
             heights = c(rep(1000,2)), widths = c(rep(1000,2)), name = "apple")

##NETFLIX
#plots to visually observe comparison with normal curves
r11 <- ggplot(netflix_returns, aes(x = netflix_returns$daily)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "grey", bins = 50) + 
  stat_function(fun = dnorm, color = "red", args = list(mean = mean(netflix_returns$daily, na.rm = TRUE),
                                                        sd = sd(netflix_returns$daily, na.rm = TRUE))) +
  theme_minimal() + ggtitle("Daily Returns") + xlab("")

r12 <- ggplot(netflix_returns, aes(x = netflix_returns$weekly)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "grey", bins = 50) + 
  stat_function(fun = dnorm, color = "red", args = list(mean = mean(netflix_returns$weekly, na.rm = TRUE),
                                                        sd = sd(netflix_returns$weekly, na.rm = TRUE))) +
  theme_minimal() + ggtitle("Weekly Returns") + xlab("")

r13 <- ggplot(netflix_returns, aes(x = netflix_returns$monthly)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "grey", bins = 50) + 
  stat_function(fun = dnorm, color = "red", args = list(mean = mean(netflix_returns$monthly, na.rm = TRUE),
                                                        sd = sd(netflix_returns$monthly, na.rm = TRUE))) +
  theme_minimal() + ggtitle("Monthly Returns") + xlab("")

r14 <- ggplot(netflix_returns, aes(x = netflix_returns$quarterly)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "grey", bins = 50) + 
  stat_function(fun = dnorm, color = "red", args = list(mean = mean(netflix_returns$quarterly, na.rm = TRUE),
                                                        sd = sd(netflix_returns$quarterly, na.rm = TRUE))) +
  theme_minimal() + ggtitle("Quarterly Returns") + xlab("")

grid.arrange(r11, r12, r13, r14, nrow = 2, ncol = 2,
             heights = c(rep(1000,2)), widths = c(rep(1000,2)), name = "netflix")

stock_stat <- data.frame(period = c("Daily", "Weekly", "Monthly", "Quarterly"),
                         amazon_sd = c(adsd, awsd, amsd, aqsd), amazon_kurtosis = c(adk,awk,amk,aqk),
                         apple_sd = c(apdsd, apwsd, apmsd, apqsd), apple_kurtosis = c(apdk,apwk,apmk,apqk),
                         netflix_sd = c(ndsd, nwsd, nmsd, nqsd), netflix_kurtosis = c(ndk,nwk,nmk,nqk))
print(stock_stat)

#Calculating Indicators for the different stocks

amazon_macd <- MACD(amazon_returns$daily, maType = 'SMA')
apple_macd <- MACD(apple_returns$daily, maType = 'SMA')
netflix_macd <- MACD(netflix_returns$daily, maType = 'SMA')

amazon_rsi <- RSI(amazon_returns$daily)
apple_rsi <- RSI(apple_returns$daily)
netflix_rsi <- RSI(netflix_returns$daily)

amazon_adx <- ADX(amazon)
apple_adx <- ADX(apple)
netflix_adx <- ADX(netflix)

amazon_roc <- ROC(amazon$AMZN.Close, n = 5)
apple_roc <- ROC(apple$AAPL.Close, n = 5)
netflix_roc <- ROC(netflix$NFLX.Close, n = 5)

#backtesting functions

backtest <- function(df, rtn.daily, from_date,to_date,strategy,strategy_name){
  rtn <- rtn.daily[index(rtn.daily)<=to_date & index(rtn.daily)>=from_date] 
  trade_return <- rtn * stats::lag(strategy, na.pad = FALSE)
  cumm_return <- Return.cumulative(trade_return)
  annual_return <- Return.annualized(trade_return) 
  summary(as.ts(trade_return))
  SharpeRatio <- SharpeRatio(as.ts(trade_return), Rf = 0, p = 0.95, FUN = "StdDev")
  SharpeRatioAnnualized <- SharpeRatio.annualized(trade_return, Rf = 0)
  out <- as.data.frame(c(cumm_return,annual_return,SharpeRatio,SharpeRatioAnnualized))
  out <- round(out,2)
  colnames(out) <- strategy_name
  row.names(out) <- c('Cumulative Return','Annualized Return',
                      'Sharpe Ratio','Annualized Sharpe Ratio')
  
  return( out )
}

BH_backtest <- function(df, rtn.daily, from_date,to_date,strategy_name){
  trade_return <- rtn.daily[index(rtn.daily)<=to_date & index(rtn.daily)>=from_date]
  cumm_return <- Return.cumulative(trade_return)
  annual_return <- Return.annualized(trade_return) 
  summary(as.ts(trade_return))
  SharpeRatio <- SharpeRatio(as.ts(trade_return), Rf = 0, p = 0.95, FUN = "StdDev")
  SharpeRatioAnnualized <- SharpeRatio.annualized(trade_return, Rf = 0)
  out <- as.data.frame(c(cumm_return,annual_return,SharpeRatio,SharpeRatioAnnualized))
  out <- round(out,2)
  colnames(out) <- strategy_name
  row.names(out) <- c('Cumulative Return','Annualized Return',
                      'Sharpe Ratio','Annualized Sharpe Ratio')
  
  return( out )
}


#Strategies for Amazon
chartSeries(amazon, subset = "2019", theme = "white",
            TA = "addVo();addBBands();addMACD();addRSI();
            addTA(OpCl(amazon), col = 'blue', type = 'h')")

# Strategy 1: if macd>signal, enter and stay in the market. 
# If macd<signal, exit the market.
s1_amazon <- ifelse ((amazon_macd$signal < amazon_macd$macd) , 1, 0)
s1_amazon[is.na(s1_amazon)] <-0

# Strategy 2: if overbought, enter and stay in the market.
s2_amazon <- ifelse ((amazon_macd$signal < amazon_macd$macd) & (amazon_rsi$rsi > 70), 1, 0)
s2_amazon[is.na(s2_amazon)] <-0

# Strategy 3: Attempting to time the market by tracking change in RSI, enter and stay in the market
s3_amazon <- ifelse ((amazon_returns$daily > 0 & (amazon_rsi$rsi - stats::lag(amazon_rsi$rsi) > 0)), 1, 0)
s3_amazon[is.na(s3_amazon)] <-0

#Strategy 4: if any of the directional indicators are going up, enter and stay in the market.
s4_amazon <- ifelse((amazon_roc > 0 | amazon_adx$ADX > 20), 1, 0)
s4_amazon[is.na(s4_amazon)] <- 0

# Buy-and-hold: keep it all time. So "1", not "0"
bh_strategy <- rep(1,dim(amazon_returns)[1])

#Backtesting for performance
s1_amazon_performance <- backtest(amazon, amazon_returns$daily, from_date = '2015-01-01', to_date = '2019-12-31',
                              s1_amazon, "Strategy 1 for Amazon")
s2_amazon_performance <- backtest(amazon, amazon_returns$daily, from_date = '2015-01-01', to_date = '2019-12-31',
                                  s2_amazon, "Strategy 2 for Amazon")
s3_amazon_performance <- backtest(amazon, amazon_returns$daily, from_date = '2015-01-01', to_date = '2019-12-31',
                                  s3_amazon, "Strategy 3 for Amazon")
s4_amazon_performance <- backtest(amazon, amazon_returns$daily, from_date = '2015-01-01', to_date = '2019-12-31',
                                  s4_amazon, "Strategy 4 for Amazon")
bh_amazon_performance <- BH_backtest(amazon, amazon_returns$daily, from_date = '2015-01-01', to_date = '2019-12-31',
                                                       "Holding Amazon")

amazon_perf <- data.frame(s1 = s1_amazon_performance$`Strategy 1 for Amazon`,
                          s2 = s2_amazon_performance$`Strategy 2 for Amazon`,
                          s3 = s3_amazon_performance$`Strategy 3 for Amazon`,
                          s4 = s4_amazon_performance$`Strategy 4 for Amazon`,
                          bh = bh_amazon_performance$`Holding Amazon`)
row.names(amazon_perf) <- c('Cumulative Return','Annualized Return',
                    'Sharpe Ratio','Annualized Sharpe Ratio')

#Strategies for Apple
chartSeries(apple, subset = "last 3 years", theme = "white",
            TA = "addVo();addBBands();addMACD();addRSI();
            addTA(OpCl(apple), col = 'blue', type = 'h')")
# Strategy 1: if macd>signal, enter and stay in the market. 
# If macd<signal, exit the market.
s1_apple <- ifelse ((apple_macd$signal < apple_macd$macd) , 1, 0)
s1_apple[is.na(s1_apple)] <-0

# Strategy 2: if overbought, enter and stay in the market.
s2_apple <- ifelse ((apple_macd$signal < apple_macd$macd) & (apple_rsi$rsi > 70), 1, 0)
s2_apple[is.na(s2_apple)] <-0

# Strategy 3: Attempting to time the market by tracking change in RSI, enter and stay in the market
s3_apple <- ifelse ((apple_returns$daily > 0 & (apple_rsi$rsi - stats::lag(apple_rsi$rsi) > 0)), 1, 0)
s3_apple[is.na(s3_apple)] <-0

#Strategy 4: if any of the directional indicators are going up, enter and stay in the market.
s4_apple <- ifelse((apple_roc > 0 | apple_adx$ADX > 20), 1, 0)
s4_apple[is.na(s4_apple)] <- 0

# Buy-and-hold: keep it all time. So "1", not "0"
bh_apple <- rep(1,dim(apple_returns)[1])

#Backtesting for performance
s1_apple_performance <- backtest(apple, apple_returns$daily, from_date = '2015-01-01', to_date = '2019-12-31',
                                  s1_apple, "Strategy 1 for apple")
s2_apple_performance <- backtest(apple, apple_returns$daily, from_date = '2015-01-01', to_date = '2019-12-31',
                                  s2_apple, "Strategy 2 for apple")
s3_apple_performance <- backtest(apple, apple_returns$daily, from_date = '2015-01-01', to_date = '2019-12-31',
                                  s3_apple, "Strategy 3 for apple")
s4_apple_performance <- backtest(apple, apple_returns$daily, from_date = '2015-01-01', to_date = '2019-12-31',
                                  s4_apple, "Strategy 4 for apple")
bh_apple_performance <- BH_backtest(apple, apple_returns$daily, from_date = '2015-01-01', to_date = '2019-12-31',
                                     "Holding apple")

apple_perf <- data.frame(s1 = s1_apple_performance$`Strategy 1 for apple`,
                          s2 = s2_apple_performance$`Strategy 2 for apple`,
                          s3 = s3_apple_performance$`Strategy 3 for apple`,
                          s4 = s4_apple_performance$`Strategy 4 for apple`,
                          bh = bh_apple_performance$`Holding apple`)
row.names(apple_perf) <- c('Cumulative Return','Annualized Return',
                            'Sharpe Ratio','Annualized Sharpe Ratio')

#Strategies for Netflix
chartSeries(netflix, subset = "last 3 years", theme = "white", 
            TA = "addVo();addBBands();addMACD();addRSI();
            addTA(OpCl(netflix), col = 'blue', type = 'h')")
# Strategy 1: if macd>signal, enter and stay in the market. 
# If macd<signal, exit the market.
s1_netflix <- ifelse ((netflix_macd$signal < netflix_macd$macd) , 1, 0)
s1_netflix[is.na(s1_netflix)] <-0

# Strategy 2: if overbought, enter and stay in the market.
s2_netflix <- ifelse ((netflix_macd$signal < netflix_macd$macd) & (netflix_rsi$rsi > 70), 1, 0)
s2_netflix[is.na(s2_netflix)] <-0

# Strategy 3: Attempting to time the market by tracking change in RSI, enter and stay in the market
s3_netflix <- ifelse ((netflix_returns$daily > 0 & (netflix_rsi$rsi - stats::lag(netflix_rsi$rsi) > 0)), 1, 0)
s3_netflix[is.na(s3_netflix)] <-0

#Strategy 4: if any of the directional indicators are going up, enter and stay in the market.
s4_netflix <- ifelse((netflix_roc > 0 | netflix_adx$ADX > 20), 1, 0)
s4_netflix[is.na(s4_netflix)] <- 0

# Buy-and-hold: keep it all time. So "1", not "0"
bh_netflix <- rep(1,dim(netflix_returns)[1])

#Backtesting for performance
s1_netflix_performance <- backtest(netflix, netflix_returns$daily, from_date = '2015-01-01', to_date = '2019-12-31',
                                 s1_netflix, "Strategy 1 for netflix")
s2_netflix_performance <- backtest(netflix, netflix_returns$daily, from_date = '2015-01-01', to_date = '2019-12-31',
                                 s2_netflix, "Strategy 2 for netflix")
s3_netflix_performance <- backtest(netflix, netflix_returns$daily, from_date = '2015-01-01', to_date = '2019-12-31',
                                 s3_netflix, "Strategy 3 for netflix")
s4_netflix_performance <- backtest(netflix, netflix_returns$daily, from_date = '2015-01-01', to_date = '2019-12-31',
                                 s4_netflix, "Strategy 4 for netflix")
bh_netflix_performance <- BH_backtest(netflix, netflix_returns$daily, from_date = '2015-01-01', to_date = '2019-12-31',
                                    "Holding netflix")

netflix_perf <- data.frame(s1 = s1_netflix_performance$`Strategy 1 for netflix`,
                         s2 = s2_netflix_performance$`Strategy 2 for netflix`,
                         s3 = s3_netflix_performance$`Strategy 3 for netflix`,
                         s4 = s4_netflix_performance$`Strategy 4 for netflix`,
                         bh = bh_netflix_performance$`Holding netflix`)
row.names(netflix_perf) <- c('Cumulative Return','Annualized Return',
                           'Sharpe Ratio','Annualized Sharpe Ratio')