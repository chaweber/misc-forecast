library(corrr)
library(ggplot2)
library(wesanderson)
library(vistime)
library(tseries)

source("R/fetch-data.R")
source("R/helpers.R")
source("R/config.R")

# desc. stats

# read data # ----
data <- fetch_data(tickers = tickers, tickernames = tickernames)

daily_stocks <- data$daily_stocks
monthly_stocks <- data$monthly_stocks

monthly_stocks %>%
  filter_time(time_formula = "2016-01-01" ~ "end") %>%
  vistime(., events = "symbol", groups = "stocktype", start = "date", end = "date")

# daily_stocks %>%
#   ggplot() + 
#   geom_segment(aes(x=Begin, xend=End, y=EventID, yend=EventID, group=Sep), size=12)

# correlations between prices / returns of diff. assets / indices (incl. corr heatmap) # ----

# get correlations
data_corr <- daily_stocks %>%
  filter(stocktype %in% c("index", "top")) %>%
  select(symbol, date, adjusted) %>%
  spread(symbol, adjusted) %>%
  drop_col(., name = "date") %>%
  correlate(use = "pairwise.complete.obs") %>%    
  rearrange(method = "HC", absolute = FALSE) %>%  
  shave() 

# plot correlation heatmap
rplot(data_corr) +
  labs(title = "Correlations between Adjusted Closing Prices", subtitle = "Indices and S&P 500 Largest Companies")

# summary of distribution of returns # ----
day_summary <- daily_stocks %>%
  group_by(symbol) %>%
  summarise(return_av = mean(log_return),
            return_var = var(log_return),
            return_skew = skewness(log_return),
            return_kurt = kurtosis(log_return))

month_summary <- monthly_stocks %>%
  group_by(symbol) %>%
  summarise(return_av = mean(log_return),
            return_var = var(log_return),
            return_skew = skewness(log_return),
            return_kurt = kurtosis(log_return))


# test for normality of returns # ----

jarque_bera(stockdata = daily_stocks, time_period = "daily")
jarque_bera(stockdata = monthly_stocks, time_period = "monthly")

# plot distribution of returns (histogram / density curve / normal qq-plot) ----

# hist 

stocks_month %>%
  filter(stocktype == "bottom") %>%
  group_by(symbol) %>%
  ggplot(., aes(x=log_return, fill = symbol)) +
  geom_histogram(aes(y=..density..), binwidth = 0.003, alpha = 0.8) +
  scale_color_manual(values = wes_palette(n=10, name="Rushmore1", type = "continuous")) +
  facet_grid(symbol ~ .) +
  coord_cartesian(xlim = c(-0.1, 0.1)) +
  labs(x = "Logarithmic Returns", 
       y = "Density", 
       title = "Distribution of Monthly Returns", 
       subtitle = "Smallest S&P 500 Companies")

# density

stocks %>%
  filter(stocktype == "index") %>%
  ggplot(., aes(x=log_return, colour = symbol, fill = symbol)) +
  stat_density(aes(y=..density..), geom = "line", alpha = 0.8) +
  scale_color_manual(values = wes_palette(n=3, name="Rushmore1")) +
  coord_cartesian(xlim = c(-0.1, 0.1)) +
  labs(x = "Logarithmic Returns", y = "Density", title = "Distribution of Log Returns for Indices")

# normal-qq plot 
stocks %>%
  filter(stocktype == "bottom") %>%
  ggplot(aes(sample = log_return, colour = symbol)) +
  stat_qq(size = 0.5) +
  scale_color_manual(values = wes_palette(n=10, name="FantasticFox1", type = "continuous")) +
  facet_wrap(symbol ~ .) +
  labs(x = "Theoretical Quantiles", 
       y = "Sample Quantiles", 
       title = "Normal QQ Plot",
       subtitle = "Daily Logarithmic Returns")




# normalise adjusted value for given period ----

calc_period <- c("2014-01-01", "end")

data <- stocks %>% normalise_by_first(calc_period = calc_period, 
                                      ticker = tickernames,
                                      stockdata = .) %>%
  normalise_by_mean(calc_period = calc_period, 
                    ticker = tickernames,
                    stockdata = .)

# plot charts ----

# facet grid prices 
data %>%
  ggplot(., aes(x = date, y = first_norm, color = symbol)) +
  geom_line(size = 0.5) +
  scale_color_manual(values = wes_palette(n=23, name="Rushmore1", type = "continuous")) +
  facet_grid(stocktype ~.) +
  labs(title = "Price Movement largest vs. smallest S&P 500 Companies", 
       y = "Normalised Prices", 
       x = "Date",
       caption = "(based on daily adjusted closing prices, normalised resp. 01-01-2014)")


# index against top / bottom

print(plot_base(ticker = c("NWSA", "UAA", "DISCA")))
