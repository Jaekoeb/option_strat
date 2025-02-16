


# Libraries and Data ------------------------------------------------------

library(tidyverse)
library(Farben)


straddle <- read_csv("data/straddle.csv")



# remove Biohaven Outlier
straddle <- straddle |> 
  filter(!(date == "2022-09-26" & ticker == "BHVN"))



# Function ----------------------------------------------------------------

backtest <- function(n = 3, sort_by = signal){
  
  # Compute Top Portfolio
  low <- straddle |> 
    group_by(year, month) |> 
    arrange(signal) |> 
    slice_head(n = n) |> 
    summarize(
      strat = "Low",
      mid = sum(mid),
      spread = sum(spread) / sum(mid),
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
      spread = sum(spread) / sum(mid),
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




# Backtest ----------------------------------------------------------------

# Compute High & Low Portfolios
df <- backtest(sort_by = signal_rel, n = 5)


# EDA ---------------------------------------------------------------------


# Table of summary statistics of results
df |> group_by(strat) |> summarize(
  min = min(profit_rel),
  q1 = quantile(profit_rel, 0.25),
  median = median(profit_rel),
  mean = mean(profit_rel),
  q3 = quantile(profit_rel, 0.75),
  max = max(profit_rel)
)


# Compute High - Low Portfolio
result <- df |> 
  select(year, month, strat, profit_rel, spread) |> 
  pivot_wider(names_from = strat, values_from = c(profit_rel, spread)) |> 
  mutate(
    profit = profit_rel_High - profit_rel_Low,
    spread = 0.5 * (spread_High + spread_Low),
    profit_tc = profit - 0.5 * spread,
    value = cumsum(profit),
    value_tc = cumsum(profit_tc),
    return = (value - lag(value)) / lag(value),
    return_tc = (value_tc - lag(value_tc)) / lag(value_tc),
    date = as.Date(paste(year, month, "15", sep = "-"))
  ) |> 
  select(date, profit, profit_tc, spread, value, value_tc, return, return_tc)


# Plots -------------------------------------------------------------------


# Cost of buying volatility hedge
gg <- df |> 
  mutate(date = as.Date(paste(year, month, "15", sep = "-"))) |> 
  group_by(strat) |> 
  mutate(value = 100 + cumsum(profit_rel)) |> 
  ungroup() |> 
  select(date, strat, value) |> 
  ggplot(aes(x = date, y = value, color = strat, group = strat)) +
  geom_line(linewidth = 1) +  # Line thickness for better visibility
  labs(
    title = "Buying Straddles is expensive",
    subtitle = "but there are discounts",
    x = "Date",
    y = "Value",
    color = "Strategy"
  ) +
  theme_bw() +  # Use black-and-white theme
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  scale_color_manual(values = space[c(1,3)]) +  # Apply custom colors
  theme(
    legend.position = "bottom",  # Move legend to bottom for better readability
    plot.title = element_text(size = 12, face = "bold"),  # Center and style title
    axis.text = element_text(size = 10),  # Adjust axis text size
    axis.title = element_text(size = 12),  # Adjust axis title size
    legend.text = element_text(size = 10)  # Adjust legend text size
  )

ggsave(gg,
       filename = "results/hedging.pdf",
       unit = "cm",
       height = 12,
       width = 18)

ggsave(gg,
       filename = "results/hedging.png",
       unit = "cm",
       height = 12,
       width = 18,
       dpi = 320)



# Strategy Performance
gg <- result |> 
  ggplot(aes(x = date, y = value)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Strategy Performance",
    x = "Date",
    y = "Value"
  ) +
  theme_bw() +  # Use black-and-white theme
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  theme(
    legend.position = "bottom",  # Move legend to bottom for better readability
    plot.title = element_text(size = 12, face = "bold"),  # Center and style title
    axis.text = element_text(size = 10),  # Adjust axis text size
    axis.title = element_text(size = 12),  # Adjust axis title size
    legend.text = element_text(size = 10)  # Adjust legend text size
  )

ggsave(gg,
       filename = "results/strategy.pdf",
       unit = "cm",
       height = 12,
       width = 18)


ggsave(gg,
       filename = "results/strategy.png",
       unit = "cm",
       height = 12,
       width = 18,
       dpi = 320)




# Save dataframe ----------------------------------------------------------


result <- result |> select(date, value, return)

write_csv(result, file = "data/strategy.csv")

