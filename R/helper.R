# helper functions

drop_col <- function(df, name){
  df <- df %>% select(-one_of(name))
}

normalise_by_first <- function(calc_period, stockdata, ticker){
  
  data <- NULL
  start <- calc_period[1]
  end <- calc_period[2]
  
  for (x in seq(length(ticker))) {
    
    tmp <- stockdata %>%
      filter(symbol == ticker[x]) %>%
      filter_time(time_formula = start ~ end) %>%
      arrange(date) %>%
      mutate(first_norm = (adjusted/first(adjusted))-1)
    
    data <- bind_rows(data, tmp)
  }
  
  data <- data %>% as_tbl_time(., index = date)
  
  return(data)
  
}


normalise_by_mean <- function(calc_period, stockdata, ticker){
  
  data <- NULL
  start <- calc_period[1]
  end <- calc_period[2]
  
  for (x in seq(length(ticker))) {
    
    tmp <- stockdata %>%
      filter(symbol == ticker[x]) %>%
      filter_time(time_formula = start ~ end) %>%
      arrange(date) %>%
      mutate(mean_norm = (adjusted/mean(adjusted))-1)
    
    data <- bind_rows(data, tmp)
  }
  
  data <- data %>% as_tbl_time(., index = date)
  
  return(data)
  
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