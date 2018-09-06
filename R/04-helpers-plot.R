# colourpal takes in any of: 
# BottleRocket1, BottleRocket2, Rushmore1, 
# Royal1, Royal2, Zissou1, Darjeeling1, Darjeeling2, 
# Chevalier1 , FantasticFox1 , Moonrise1, Moonrise2, 
# Moonrise3, Cavalcanti1, GrandBudapest1, GrandBudapest2, 
# IsleofDogs1, IsleofDogs2

#  basic chart plot ----
plot_base <- function(stockdata, 
                      type, 
                      start = "start",
                      end = "end",
                      by)
{
  
  ifelse((by == "log_return"), name <- "Logartihmic Returns", name <- "Adjusted Closing Prices")
  period <- stockdata$date[1:2] %>% diff.Date() %>% as.integer()
  ifelse((period > 5), period <- "monthly", period <- "daily")
  
  plot <- stockdata %>%
    filter(stocktype == type) %>%
    rownames_to_column() %>%
    filter_time(time_formula = start ~ end) %>%
    as_tbl_time(index = date) %>%
    ggplot(., aes_string(x = "date", y = by, colour = "symbol")) +
    geom_line(size = 0.5) +
    facet_grid(symbol~.) +
    scale_color_manual(values = wes_palette(n=10, name = "Darjeeling1", type = "continuous")) +
    theme(legend.position = "none") +
    labs(title = paste(toupper(period), toupper(name)),
         y = by, 
         x = "Date")
  
  return(plot)
  
}


# distribution of returns (histogram / density curve / normal qq-plot) ----

plot_dist <- function(stockdata, type, by){
  
  ifelse((by == "log_return"), name <- "Logartihmic Returns", name <- "Adjusted Closing Prices")
  period <- stockdata$date[1:2] %>% diff.Date() %>% as.integer()
  ifelse((period > 5), period <- "monthly", period <- "daily")
  
  if (type == "index"){
    dist <- stockdata %>%
      filter(stocktype == type) %>%
      ggplot(., aes_string(x = by, colour = "symbol", fill = "symbol")) +
      stat_density(aes(y=..density..), geom = "line", alpha = 0.8) +
      scale_color_manual(values = wes_palette(n=3, name="GrandBudapest1")) +
      coord_cartesian(xlim = c(-0.1, 0.1)) +
      labs(x = name, 
           y = "Density", 
           title = paste("Density Functions", name, "of Indices"))
  } else {
    dist <- stockdata %>%
      filter(stocktype == type) %>%
      group_by(symbol) %>%
      ggplot(., aes_string(x = by, fill = "symbol")) +
      geom_histogram(aes(y=..density..), binwidth = 0.003, alpha = 0.8) +
      scale_color_manual(values = wes_palette(n=10, name="Rushmore1", type = "continuous")) +
      facet_grid(symbol ~ .) +
      coord_cartesian(xlim = c(-0.1, 0.1)) +
      labs(x = by, 
           y = "Density", 
           title = paste("Distribution of", period, name))
  }

  
  # normal-qq plot 
  
  qq <- stockdata %>%
    filter(stocktype == type) %>%
    ggplot(aes_string(sample = by, colour = "symbol")) +
    stat_qq(size = 0.5) +
    scale_color_manual(values = wes_palette(n=10, name="FantasticFox1", type = "continuous")) +
    facet_wrap(symbol ~ .) +
    labs(x = "Theoretical Quantiles", 
         y = "Sample Quantiles", 
         title = "Normal QQ Plot",
         subtitle = paste(toupper(period), toupper(name)))

  
  return(list(dist = dist,
              qq = qq))
}