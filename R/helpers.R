# helper functions


# drops columns in df by names
# name is a character vector including the columns to drop
drop_col <- function(df, name){
  df <- df %>% select(-one_of(name))
}

# Jarque Bera Test for Normality
  jarque_bera <- function(stockdata, time_period){
    
      stocks_ts <- as.ts(stockdata %>% 
                  select(symbol, rel_return, date) %>% 
                  spread(symbol, rel_return))

      p_list <- NULL

      for (i in seq(ncol(stocks_ts)-1)){
        i <- i+1  
        test <- jarque.bera.test(na.remove(stocks_ts[, i]))
        p_list$pvalue[i-1] <- test$p.value
        p_list$stock[i-1] <- colnames(stocks_ts)[i]
      }
      
      
      if (any(p_list$pvalue>=0.01)){
        
        data <- tibble(stocks = p_list$stock,
                       pvalues = p_list$pvalue) %>%
          filter(pvalues >= 0.01)
        
        print(paste("For the following assets,", time_period, "returns might follow a normal distribution:"))
        return(data)
        
      } else {
        print(paste("The", time_period, "relative returns are NOT normally distributed for any of the given assets"))
        
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