


# Libraries and Data ------------------------------------------------------

library(tidyverse)
library(xts)
library(PerformanceAnalytics)
library(stargazer)
library(Farben)



strategy <- read_csv("data/strategy.csv")

zz <- read_delim("data/zz.csv", delim = ";", 
                 escape_double = FALSE, col_types = cols(date = col_date(format = "%d.%m.%Y")), 
                 trim_ws = TRUE)

sp <- read_csv("data/sp500.csv")

vix <- read_csv("data/vix.csv", col_types = cols(DATE = col_date(format = "%m/%d/%Y")))


ff3 <- read_csv("data/ff3.CSV", col_types = cols(...1 = col_date(format = "%Y%m")), 
                skip = 3)



# Clean and Returns -------------------------------------------------------



# compute daily returns
sp <- sp |> 
  mutate(
    return = (sp500 - lag(sp500)) / lag(sp500),
    vola = 100 * sqrt(250) * rollapply(return, width = 250, FUN = sd, fill = NA, align = "right")
  )


sp <- sp |> 
  mutate(
    year = year(date),
    month = month(date)
  ) |> 
  group_by(year, month) |> 
  slice_min(order_by = abs(day(date) - 15), n = 1, with_ties = F) |> 
  ungroup() |> 
  mutate(
    date = as.Date(paste(year, month, "15", sep = "-")),
    sp500 = (sp500 - lag(sp500)) / lag(sp500)
  ) |> 
  select(date, sp500, vola)



vix <- vix |> 
  rename(
    date := DATE,
    vix := CLOSE
  ) |>
  mutate(
    year = year(date),
    month = month(date)
  ) |> 
  group_by(year, month) |> 
  slice_min(order_by = abs(day(date) - 15), n = 1, with_ties = F) |> 
  ungroup() |> 
  mutate(
    date = as.Date(paste(year, month, "15", sep = "-"))
  ) |> 
  select(date, vix)



zz <- zz |> 
  mutate(
    year = year(date),
    month = month(date)
  ) |> 
  group_by(year, month) |> 
  slice_min(order_by = abs(day(date) - 15), n = 1, with_ties = F) |> 
  ungroup() |> 
  mutate(
    date = as.Date(paste(year, month, "15", sep = "-")),
    zz = (zz - lag(zz)) / lag(zz)
  ) |> 
  select(date, zz)
  


ff3 <- ff3 |> 
  mutate(
    year = year(`...1`),
    month = month(`...1`),
    date = as.Date(paste(year, month, "15", sep = "-")),
    rf = RF / 100
  ) |> 
  select(date, rf)




strategy <- strategy |> 
  left_join(sp, join_by(date == date)) |> 
  left_join(vix, join_by(date == date)) |> 
  left_join(zz, join_by(date == date)) |> 
  left_join(ff3, join_by(date == date))



perf <- xts(strategy[, c("return", "rf")], order.by = strategy$date)

rm(sp, vix, zz, ff3)

# Analysis ----------------------------------------------------------------



# Basic Performance Analysis
perf <- rbind(
  Return.annualized(perf),
  StdDev.annualized(perf),
  maxDrawdown(perf),
  SharpeRatio.annualized(perf, Rf = perf$rf)
)



# Regressions for betas

# compute excess return
strategy <- strategy |> mutate(sp500 = sp500 - rf)

sp500 <- lm(return ~ sp500, data = strategy)
summary(sp500)

vola <- lm(return ~ vola, data = strategy)
summary(vola)

vix <- lm(return ~ vix, data = strategy)
summary(vix)


zz <- lm(return ~ zz, data = strategy)
summary(zz)


stargazer(sp500, vola, vix, zz, 
          out = "results/regression.html")

rm(sp500, vix, vola, zz)



# Correlations

# Create a function to compute correlation and p-value
cor_with_pval <- function(x, y) {
  test <- cor.test(x, y, use = "complete.obs")  # Handle missing values
  c(correlation = test$estimate, p_value = test$p.value)
}

# Apply the function to all columns except "return"
corre <- sapply(strategy[, c("sp500", "vola", "vix", "zz")], 
                  function(col) cor_with_pval(strategy$return, col))

rm(cor_with_pval)
