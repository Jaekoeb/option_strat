


# Libraries ---------------------------------------------------------------

library(dplyr)
library(lubridate)


# Cleaning Function -------------------------------------------------------

cleaning <- function(file_source){
  
  option <- readr::read_csv(file_source, show_col_types = FALSE)
  
  # Keep only the first observation of each option
  option <- option |>
    arrange(date) |> 
    group_by(optionid) |> 
    slice_head(n = 1) |> 
    ungroup()
  
  # For each month and company keep the Call / Put option
  # with the lowest / highest strike
  # option <- suppressWarnings({
  #   option |> 
  #     mutate(month = month(date)) |> 
  #     group_by(ticker, month) |> 
  #     filter((cp_flag == "C" & strike_price == min(strike_price[cp_flag == "C"])) |
  #              (cp_flag == "P" & strike_price == max(strike_price[cp_flag == "P"]))) |> 
  #     # Keep only groups that have both a Call and a Put
  #     filter(n_distinct(cp_flag) == 2) |>
  #     ungroup()
  # }) 
  
  return(option)
}



# Cleaning ----------------------------------------------------------------


file_paths <- c(paste0("data/clean/option_", sprintf("%02d", 1:25), ".csv"))
k <- 1



# Remove the file if it exists to start fresh
if (file.exists("data/option.csv")) {
  file.remove("data/option.csv")
}

# Loop through each file path pair and apply the cleaning function
for (path in file_paths) {
  
  start_time <- Sys.time()
  
  # Check if it's the first iteration
  write_header <- !file.exists("data/option.csv")
  
  df <- cleaning(path)
  
  readr::write_csv(df, file = "data/option.csv", append = TRUE, col_names = write_header)
  
  end_time <- Sys.time()
  beepr::beep(1)
  writeLines("__________________________________________________")
  writeLines(paste("Finished cleaning", k, "of 25"))
  k <- k + 1
  elapsed_time <- end_time - start_time
  print(elapsed_time)
  writeLines("__________________________________________________")
  writeLines("Starting new file")
  
}

