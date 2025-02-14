


# Libraries and Data ------------------------------------------------------

library(tidyverse)
library(Farben)


straddle <- read_csv("data/straddle.csv")



backtest <- function(n = 3, sort_by = signal){
  
  # Compute Top Portfolio
  low <- straddle |> 
    group_by(year, month) |> 
    arrange(signal) |> 
    slice_head(n = n) |> 
    summarize(
      strat = "Low",
      mid = sum(mid),
      spread = sum(spread),
      payoff = sum(payoff),
      hist_vola = mean(hist_vola),
      imp_vola = mean(imp_vola),
      signal = mean(signal),
      signal_rel = mean(signal_rel)
    ) |> 
    ungroup()
  
  
  # Compute Low Portfolio
  high <- straddle |> 
    group_by(year, month) |> 
    arrange({{sort_by}}) |> 
    slice_tail(n = n) |> 
    summarize(
      strat = "High",
      mid = sum(mid),
      spread = sum(spread),
      payoff = sum(payoff),
      hist_vola = mean(hist_vola),
      imp_vola = mean(imp_vola),
      signal = mean(signal),
      signal_rel = mean(signal_rel)
    ) |> 
    ungroup()
  
  
  # Combine
  df <- rbind(high, low)
  
  # Compute profit
  df <- df |> 
    mutate(
      profit = payoff - mid,
      profit_rel = profit / mid
    )
  
  return(df)
  
}


result <- backtest(sort_by = signal)


# Table of summary statistics of results
result |> group_by(strat) |> summarize(
  min = min(profit_rel),
  q1 = quantile(profit_rel, 0.25),
  median = median(profit_rel),
  mean = mean(profit_rel),
  q3 = quantile(profit_rel, 0.75),
  max = max(profit_rel)
)




# Plots -------------------------------------------------------------------


# Cost of buying volatility hedge
gg <- result |> 
  mutate(date = as.Date(paste(year, month, "15", sep = "-"))) |> 
  group_by(strat) |> 
  mutate(value = 100 + cumsum(profit_rel)) |> 
  ungroup() |> 
  select(date, strat, value)



gg <- gg |> 
  ggplot(aes(x = date, y = value, color = strat, group = strat)) +
  geom_line(linewidth = 1) +  # Line thickness for better visibility
  labs(
    title = "Strategy Performance Over Time",
    x = "Date",
    y = "Value",
    color = "Strategy"
  ) +
  theme_bw() +  # Use black-and-white theme
  scale_color_manual(values = basic) +  # Apply custom colors
  theme(
    legend.position = "bottom",  # Move legend to bottom for better readability
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Center and style title
    axis.text = element_text(size = 12),  # Adjust axis text size
    axis.title = element_text(size = 13, face = "bold"),  # Adjust axis title size
    legend.text = element_text(size = 10)  # Adjust legend text size
  )

gg
