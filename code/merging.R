

# Libraries and load data -------------------------------------------------

library(tidyverse)


option <- read_csv("data/option.csv")
equity <- read_csv("data/equity_clean.csv")



# add year and week column to option data
option <- option |> 
  mutate(
    year = year(date),
    week = week(date)
  )



# Merge by current date
option <- left_join(option, equity |> select(-date), join_by(year == year, week == week, ticker == ticker))

# rename and select certain columns
option <- option |> 
  mutate(strike = strike_price / 1000,
         imp_vola = impl_volatility,
         year = year(exdate),
         week = week(exdate)) |> 
  select(symbol, optionid, ticker, issuer, industry_group,
         cp_flag, date, exdate, mid, spread, strike, price,
         hist_vola, imp_vola, delta, gamma, vega, theta, year, week)


# Now merge equity again this time to obtain the price of underlying at expiration
equity <- equity |> 
  rename(ex_price = price) |> 
  select(year, week, ticker, ex_price)

option <- left_join(option, equity, join_by(year == year, week == week, ticker == ticker))

rm(equity)

# Filter out missing observations
option <- option |> 
  filter(
    !is.na(price),
    !is.na(ex_price),
    !is.na(strike),
    !is.na(hist_vola),
    !is.na(imp_vola)
  )


# Compute HV - IV
option <- option |> 
  mutate(
    signal = hist_vola - imp_vola,
    signal_rel = hist_vola / imp_vola
  )


# Compute and filter moneyness
option <- option |> 
  mutate(
    moneyness = ifelse(cp_flag == "C",
                       price / strike,
                       strike / price)
  ) |> 
  filter(
    moneyness <= 1.1,
    moneyness >= 0.9
  )




# Keep for each company only the closest ATM Call and Put
option <- option |> 
  group_by(year(date), month(date), ticker, cp_flag) |> 
  mutate(help = abs(moneyness - 1)) |> 
  arrange(help) |> 
  slice_head(n = 1) |> 
  ungroup()


# Compute Payoff
option <- option |> 
  mutate(
    payoff = ifelse(cp_flag == "C",
                    ex_price - strike,
                    strike - ex_price),
    payoff = ifelse(payoff < 0, 0, payoff)
  )



# Reorder columns
option <- option |> 
  select(
    symbol,
    optionid,
    ticker,
    issuer,
    industry_group,
    cp_flag,
    date,
    exdate,
    mid,
    spread,
    strike,
    price,
    ex_price,
    payoff,
    moneyness,
    hist_vola,
    imp_vola,
    delta,
    gamma,
    vega,
    theta,
    signal,
    signal_rel
  )



# compute straddle data frame
straddle <- option |> 
  mutate(year = year(date), month = month(date)) |> 
  group_by(year, month, ticker) |> 
  filter(n() > 1) |>
  summarize(
    ticker = first(ticker),
    issuer = first(issuer),
    industry_group = first(industry_group),
    date = first(date),
    exdate = first(exdate),
    mid = sum(mid),
    spread = sum(spread),
    strike = mean(strike),
    payoff = sum(payoff),
    hist_vola = mean(hist_vola),
    imp_vola = mean(imp_vola),
    delta = sum(delta),
    gamma = sum(gamma),
    vega = sum(vega),
    theta = sum(theta),
    signal = mean(signal),
    signal_rel = mean(signal_rel)
  ) |> 
  ungroup()

write_csv(option, "data/option_clean.csv")
write_csv(straddle, "data/straddle.csv")
