# helper functions


# drops columns in df by names
# name is a character vector including the columns to drop
drop_col <- function(df, name){
  df <- df %>% select(-one_of(name))
}

# perform jarque bera test for normality
  jarque_bera <- function(stockdata){
    
      stocks_ts <- as.ts(stockdata %>% 
                  select(symbol, rel_return, date) %>% 
                  spread(symbol, rel_return))
      
      p_value <- NULL

      for (i in seq(ncol(stocks_ts)-1)){
        i <- i+1  
        test <- jarque.bera.test(na.remove(stocks_ts[, i]))
        p[i-1] <- test$p.value
      }
      
      if (any(p_value>=0.01)){
        print("For some stocks, returns might be normally distributed")
        return(p_value)
      } else {
        print("The relative returns are NOT normally distributed for any of the given stocks")
      }
      
  }


# colourpal takes in any of: 
# BottleRocket1, BottleRocket2, Rushmore1, 
# Royal1, Royal2, Zissou1, Darjeeling1, Darjeeling2, 
# Chevalier1 , FantasticFox1 , Moonrise1, Moonrise2, 
# Moonrise3, Cavalcanti1, GrandBudapest1, GrandBudapest2, 
# IsleofDogs1, IsleofDogs2

plot_base <- function(stockdata = data, 
                    ticker, 
                    title = "Adjusted Closing Prices", 
                    ylab = "Price", 
                    xlab = "Date", 
                    caption = NULL, 
                    colourpal = "Darjeeling1")
  {
  
  plot <- stockdata %>%
    filter(symbol %in% ticker) %>%
    rownames_to_column() %>%
    ggplot(., aes(x = date, y = adjusted, color = symbol)) +
    geom_line(size = 0.5) +
    scale_color_manual(values = wes_palette(n=4, name = colourpal)) +
    labs(title = title, 
         y = ylab, 
         x = xlab,
         caption = caption)
  
  return(plot)
}