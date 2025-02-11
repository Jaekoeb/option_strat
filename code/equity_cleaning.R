



# Libraries ---------------------------------------------------------------

library(tidyverse)
library(zoo)


equity <- read_csv("data/equity.csv", col_types = cols(date = col_date(format = "%Y-%m-%d"), 
                                                       RET = col_double()))

colnames(equity) <- c("permno", "date", "ticker", "name", "price", "return")

# Cleaning ----------------------------------------------------------------



# change negative price to positive
# in CRSP they mean that no closing price available and instead average bid/ask was used
equity <- equity |> 
  mutate(price = abs(price))


check1 <- equity |> 
  group_by(ticker) |> 
  summarise(missing = sum(is.na(price)))


# filter out all observations with missing price and return
equity <- equity |> 
  filter(!is.na(price), !is.na(return))


# Compute rolling volatility
equity <- equity |> 
  arrange(ticker, date) |> 
  group_by(ticker) |> 
  mutate(vola = rollapply(return, width = 250, FUN = sd, fill = NA, align = "right"))

# Annualize volatility
equity <- equity |> 
  mutate(hist_vola = sqrt(250)*vola) |> 
  select(-c(permno, name, vola))


# remove observations with missing historical vola
equity <- equity |> 
  filter(!is.na(hist_vola))


# write equity data
write_csv(equity, file = "equity_clean.csv")
