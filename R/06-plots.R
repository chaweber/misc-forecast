library(ggplot2)
library(wesanderson)
library(tseries)

# colourpal takes in any of: 
# BottleRocket1, BottleRocket2, Rushmore1, 
# Royal1, Royal2, Zissou1, Darjeeling1, Darjeeling2, 
# Chevalier1 , FantasticFox1 , Moonrise1, Moonrise2, 
# Moonrise3, Cavalcanti1, GrandBudapest1, GrandBudapest2, 
# IsleofDogs1, IsleofDogs2


## function for basic chart plot ----
plot_base <- function(stockdata = data, 
                      ticker, 
                      param = "adjusted",
                      start = "start",
                      end = "end",
                      title = "Adjusted Closing Prices", 
                      ylab = "Price", 
                      xlab = "Date", 
                      caption = NULL, 
                      colourpal = "Darjeeling1")
{
  
  plot <- stockdata %>%
    filter(symbol %in% ticker) %>%
    rownames_to_column() %>%
    filter_time(time_formula = start ~ end) %>%
    as_tbl_time(index = date) %>%
    ggplot(., aes_string(x = "date", y = param, color = "symbol")) +
    geom_line(size = 0.5) +
    theme(legend.position = "none") +
    scale_color_manual(values = wes_palette(n=4, name = colourpal)) +
    labs(title = paste(ticker, title),
         y = ylab, 
         x = xlab,
         caption = caption)
  
  return(plot)
}

## candlestick chart function ----
candlestick_chart <- function(stockdata, 
                              ticker, 
                              start = "2018-01-01", 
                              end = "end"){
  
  data <- stockdata %>%
    filter(symbol %in% ticker) %>%
    rownames_to_column() %>%
    filter_time(time_formula = start ~ end) %>%
    as_tbl_time(index = date) %>%
    mutate(change = ifelse(.$close > .$open, "up", "down"),
           flat_bar = (high == low)) 
  
  width <- c(as.numeric(difftime(data$date[2], data$date[1]), units = "days"), 
             as.numeric(difftime(data$date[3], data$date[2]), units = "days"), 
             as.numeric(difftime(data$date[4], data$date[3]), units = "days"))
  data$width <- min(width)
  ifelse((min(width) == 1), name <- "Daily", name <- "Monthly")
  
  year <- year(as.POSIXlt(start))
  
  plot <- 
    ggplot(data, aes(x = date)) +
    geom_linerange(aes(ymin = low, ymax = high, colour = change)) +
    theme_bw() +
    geom_rect(aes(xmin = date - width/2 * 0.9, 
                  xmax = date + width/2 * 0.9, 
                  ymin = pmin(open, close), 
                  ymax = pmax(open, close), fill = change)) + 
    labs(title = paste("Candlestick Chart of", name, "Price Movement"), 
         subtitle = paste(ticker), 
         x = paste(year))
  
  
  if (any(data$flat_bar)){
    
    data <- data %>% 
      filter(flat_bar == TRUE)
    
    plot <- plot + geom_segment(data, 
                                aes(x = date - width / 2 * 0.9, 
                                    y = close, 
                                    yend = close, 
                                    xend = date + width / 2 * 0.9))
  } 
  
  return(plot)
}

# plot charts ----

# facet grid of prices 
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
for (ticker in c("N225", "FTSE", "GSPC")){
  jpeg(filename = paste0("plot_", ticker, ".jpg"), width = 800, units = "px")
  plot_base(stockdata = monthly_stocks, ticker = ticker)
  dev.off()
  }


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

