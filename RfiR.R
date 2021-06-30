#RFiR:


# Setup: ------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(readxl)
library(highcharter)
library(tidyquant)
library(timetk)
library(tibbletime)
library(quantmod)
library(PerformanceAnalytics)
library(scales)

# Chapter 1: Introduction -------------------------------------------------

# To move from an empty R environment to one with three portfolio returns
# objects, we will take these steps:
#   1) Import daily prices from the internet, a csv file or xls file
# 2) Transform daily prices to monthly prices
# 3) Transform monthly prices to monthly returns
# 4) Visualize monthly returns
# 5) Calculate portfolio monthly returns based on asset monthly returns
# and weights
# 6) Visualize portfolio returns
# 7) Save the data objects for use throughout this book



# Chapter 2: Returns ------------------------------------------------------

symbols <- c("SPY","EFA", "IJS", "EEM","AGG")

# from Yahoo:
prices <-
  getSymbols(symbols,
             src = 'yahoo',
             from = "2012-12-31",
             to = "2017-12-31",
             auto.assign = TRUE,
             warnings = FALSE) %>%
  map(~Ad(get(.))) %>%
  reduce(merge) %>%
  `colnames<-`(symbols)

# from csv file (like for BTC data)
prices <-
  read_csv("path to your data.csv",
           col_types =
             cols(date =
                    col_date(format = "%Y-%m-%d"))) %>%
  tk_xts(date_var = date)

# convert daily prices to monthly returns - xts

prices_monthly <- to.monthly(prices,
                             indexAt = "lastof",
                             OHLC = FALSE)
head(prices_monthly, 3)

asset_returns_xts <-
  Return.calculate(prices_monthly,
                   method = "log") %>%
  na.omit()

# convert daily prices to monthly returns - tidyverse

asset_returns_dplyr_byhand <-
  prices %>%
  to.monthly(indexAt = "lastof", OHLC = FALSE) %>%
  # convert the index to a date
  data.frame(date = index(.)) %>%
  # now remove the index because it got converted to row names
  remove_rownames() %>%
  gather(asset, prices, -date) %>%
  group_by(asset) %>%
  mutate(returns = (log(prices) - log(lag(prices)))) %>%
  select(-prices) %>%
  spread(asset, returns) %>%
  select(date, symbols)

asset_returns_dplyr_byhand <-
  asset_returns_dplyr_byhand %>%
  na.omit()

# convert daily prices to monthly returns - tidyquant

asset_returns_tq_builtin <-
  prices %>%
  tk_tbl(preserve_index = TRUE,
         rename_index = "date") %>%
  gather(asset, prices, -date) %>%
  group_by(asset) %>%
  tq_transmute(mutate_fun = periodReturn,
               period = "monthly",
               type = "log") %>%
  spread(asset, monthly.returns) %>%
  select(date, symbols) %>%
  slice(-1)

# convert daily prices to monthly returns - tibbletime

asset_returns_tbltime <-  prices %>%
  tk_tbl(preserve_index = TRUE,
         rename_index = "date") %>%
  # this is the the tibbletime function
  as_tbl_time(index = date) %>%
  as_period(period = "month",
            side = "end") %>%
  gather(asset, returns, -date) %>%
  group_by(asset) %>%
  tq_transmute(mutate_fun = periodReturn,
               type = "log") %>%
  spread(asset, monthly.returns) %>%
  select(date, symbols) %>%
  slice(-1)

# visualise - xts:

highchart(type = "stock") %>%
  hc_title(text = "Monthly Log Returns") %>%
  hc_add_series(asset_returns_xts[, symbols[1]],
                name = symbols[1]) %>%
  hc_add_series(asset_returns_xts[, symbols[2]],
                name = symbols[2]) %>%
  hc_add_series(asset_returns_xts[, symbols[3]],
                name = symbols[3]) %>%
  hc_add_series(asset_returns_xts[, symbols[4]],
                name = symbols[4]) %>%
  hc_add_series(asset_returns_xts[, symbols[5]],
                name = symbols[5]) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = TRUE)

hc_hist <- hist(asset_returns_xts[, symbols[1]],
                breaks = 50,
                plot = FALSE)

hchart(hc_hist, color = "cornflowerblue") %>%
  hc_title(text =
             paste(symbols[1],
                   "Log Returns Distribution",
                   sep = " ")) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_legend(enabled = FALSE)

hc_hist_fun <- function(n = 1, object, color){
  hc_hist <- hist(object[, symbols[n]],
                  breaks = 50,
                  plot = FALSE)
  hchart(hc_hist, color = color) %>%
    hc_title(text =
               paste(symbols[n],
                     "Log Returns Distribution",
                     sep = " ")) %>%
    hc_add_theme(hc_theme_flat()) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_legend(enabled = FALSE)
}

hc_hist_fun(1, asset_returns_xts, "cornflowerblue")
hc_hist_fun(2, asset_returns_xts, "green")
hc_hist_fun(3, asset_returns_xts, "pink")
hc_hist_fun(4, asset_returns_xts, "purple")
hc_hist_fun(5, asset_returns_xts, "yellow")
                     
map(1:5, hc_hist_fun, asset_returns_xts, "blue")

# visualise - tidyverse

asset_returns_long %>%
  ggplot(aes(x = returns, fill = asset)) +
  geom_histogram(alpha = 0.45, binwidth = .005) +
  ggtitle("Monthly Returns Since 2013")

asset_returns_long %>%
  ggplot(aes(x = returns, fill = asset)) +
  geom_histogram(alpha = 0.45, binwidth = .01) +
  facet_wrap(~asset) +
  ggtitle("Monthly Returns Since 2013") +
  theme_update(plot.title = element_text(hjust = 0.5))

asset_returns_long %>%
  ggplot(aes(x = returns, colour = asset)) +
  geom_density(alpha = 1) +
  ggtitle("Monthly Returns Density Since 2013") +
  xlab("monthly returns") +
  ylab("distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))

asset_returns_long %>%
  ggplot(aes(x = returns)) +
  geom_density(aes(color = asset), alpha = 1) +
  geom_histogram(aes(fill = asset), alpha = 0.45, binwidth = .01) +
  guides(fill = FALSE) +
  facet_wrap(~asset) +
  ggtitle("Monthly Returns Since 2013") +
  xlab("monthly returns") +
  ylab("distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))

# Chapter 3: Building a portfolio -----------------------------------------

w <- c(0.25,
       0.25,
       0.20,
       0.20,
       0.10)

w_1 <- w[1]
w_2 <- w[2]
w_3 <- w[3]
w_4 <- w[4]
w_5 <- w[5]

asset1 <- asset_returns_xts[,1]
asset2 <- asset_returns_xts[,2]
asset3 <- asset_returns_xts[,3]
asset4 <- asset_returns_xts[,4]
asset5 <- asset_returns_xts[,5]

portfolio_returns_byhand <-
  (w_1 * asset1) +
  (w_2 * asset2) +
  (w_3 * asset3) +
  (w_4 * asset4) +
  (w_5 * asset5)

names(portfolio_returns_byhand) <- "returns"

# portfolio returns - xts

portfolio_returns_xts_rebalanced_monthly <-
  Return.portfolio(asset_returns_xts,
                   weights = w,
                   rebalance_on = "months") %>%
  `colnames<-`("returns")


# portfolio returns - tidyverse
asset_returns_long %>%
  group_by(asset) %>%
  mutate(weights = case_when(asset == symbols[1] ~ w[1],
                             asset == symbols[2] ~ w[2],
                             asset == symbols[3] ~ w[3],
                             asset == symbols[4] ~ w[4],
                             asset == symbols[5] ~ w[5]))

portfolio_returns_dplyr_byhand <-
  asset_returns_long %>%
  group_by(asset) %>%
  mutate(weights = case_when(asset == symbols[1] ~ w[1],
                             asset == symbols[2] ~ w[2],
                             asset == symbols[3] ~ w[3],
                             asset == symbols[4] ~ w[4],
                             asset == symbols[5] ~ w[5]),
         weighted_returns = returns * weights) %>%
  group_by(date) %>%
  summarise(returns = sum(weighted_returns))

# portfolio returns - tidyquant world

portfolio_returns_tq_rebalanced_monthly <-
  asset_returns_long %>%
  tq_portfolio(assets_col = asset,
               returns_col = returns,
               weights = w,
               col_rename = "returns",
               rebalance_on = "months")

portfolio_returns_dplyr_byhand %>%
  rename(tidyverse = returns) %>%
  mutate(equation = coredata(portfolio_returns_byhand),
         tq = portfolio_returns_tq_rebalanced_monthly$returns,
         xts =
           coredata(portfolio_returns_xts_rebalanced_monthly)) %>%
  mutate_if(is.numeric, funs(round(., 3)))

# visualisation - xts

highchart(type = "stock") %>%
  hc_title(text = "Portfolio Monthly Returns") %>%
  hc_add_series(portfolio_returns_xts_rebalanced_monthly$returns,
                name = "Rebalanced Monthly",
                color = "cornflowerblue") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)

hc_portfolio <-
  hist(portfolio_returns_xts_rebalanced_monthly$returns,
       breaks = 50,
       plot = FALSE)
hchart(hc_portfolio,
       color = "cornflowerblue",
       name = "Portfolio") %>%
  hc_title(text = "Portfolio Returns Distribution") %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_exporting(enabled = TRUE)


# visualising - tidyverse

portfolio_returns_tq_rebalanced_monthly %>%
  ggplot(aes(x = date, y = returns)) +
  geom_point(colour = "cornflowerblue")+
  xlab("date") +
  ylab("monthly return") +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Portfolio Returns Scatter") +
  scale_x_date(breaks = pretty_breaks(n=6))

portfolio_returns_tq_rebalanced_monthly %>%
  ggplot(aes(x = returns)) +
  geom_histogram(binwidth = .005,
                 fill = "cornflowerblue",
                 color = "cornflowerblue") +
  ggtitle("Portfolio Returns Distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))

asset_returns_long %>%
  ggplot(aes(x = returns,
             fill = asset)) +
  geom_histogram(alpha = 0.15,
                 binwidth = .01) +
  geom_histogram(data = portfolio_returns_tq_rebalanced_monthly,
                 fill = "cornflowerblue",
                 binwidth = .01) +
  ggtitle("Portfolio and Asset Monthly Returns") +
  theme_update(plot.title = element_text(hjust = 0.5))

portfolio_returns_tq_rebalanced_monthly %>%
  ggplot(aes(x = returns)) +
  geom_histogram(binwidth = .01,
                 colour = "cornflowerblue",
                 fill = "cornflowerblue") +
  geom_density(alpha = 1, color = "red") +
  xlab("monthly returns") +
  ylab("distribution") +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Portfolio Histogram and Density")


# Chapter 4: Standard Deviation -------------------------------------------

# first, covariance of returns:
covariance_matrix <- cov(asset_returns_xts)
round(covariance_matrix, 5)

#We now take the square root of the transpose 
#of the weights vector times the
#covariance matrix times the weights vector. 
#To perform matrix multiplication,
#we use %*%.

sd_matrix_algebra <- sqrt(t(w) %*% covariance_matrix %*% w)
sd_matrix_algebra_percent <-
  round(sd_matrix_algebra * 100, 2) %>%
  `colnames<-`("standard deviation")
sd_matrix_algebra_percent[1,]

