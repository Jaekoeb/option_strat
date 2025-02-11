

# Libraries ---------------------------------------------------------------

library(data.table)

# Function --------------------------------------------------------------


cleaning <- function(file_name, output_file, chunk_size = 100000){
  
  # Read the header to get the column names.
  # fread() with nrows = 0 returns a data.table with the proper column names.
  # header <- c("V1", "secid", "date", "symbol", "exdate", "cp_flag", "strike_price", "best_bid", "best_offer", "volume", "open_interest", "impl_volatility", "delta", "gamma", "vega", "theta", "optionid", "ss_flag", "ticker", "sic", "index_flag", "industry_group", "issuer", "exercise_style")
  header <- names(fread(file_name, nrows = 0))
  
  # Read the number of rows
  # num_rows <- 397259589
  num_rows <- fread(file_name, select = NULL)[, .N] + 1
  
  
  # Initialize the starting line.
  # We set skip = 1 so that we skip the header line when reading chunks.
  skip <- 1
  
  # Initialize counter
  counter <- 1
  
  # Optionally, remove any pre-existing output file.
  if (file.exists(output_file)) file.remove(output_file)
  
  
  
  ## Cleaning Parameters -----------------------------------------------------
  
  # Days left until expiration
  # should be between this interval
  maturity <- c(0, 40)
  
  
  
  # Chunk Filtering ---------------------------------------------------------
  
  repeat {
    
    # Read a chunk from the file.
    # We use header = FALSE because we already grabbed the header.
    chunk <- fread(file_name, skip = skip, nrows = chunk_size, header = FALSE)
    
    # Break out of the loop if no rows were read.
    if (nrow(chunk) == 0) break
    
    # Assign the header names to the data.table.
    setnames(chunk, header)
    
    # Convert the 'date' and 'exdate' columns to Date.
    chunk[, date := as.Date(date)]
    chunk[, exdate := as.Date(exdate)]
    
    # Compute Mid and Spread
    chunk[, mid := (best_offer + best_bid)/2]
    chunk[, spread := best_offer - best_bid]
    
    # Compute the difference in days between 'exdate' and 'date'.
    # Coerce to numeric for easy filtering.
    chunk[, diff := as.numeric(exdate - date)]
    
    
    # Filter out observations
    chunk <- chunk[
      diff <= maturity[2]
      & diff >= maturity[1]
      & best_bid > 0
      & spread > 0
      & open_interest > 0
      & !is.na(mid)
      & !is.na(spread)
    ]
    
    # Write the cleaned chunk to disk.
    # For the first chunk, create a new file; for subsequent chunks, append.
    fwrite(chunk, output_file, append = TRUE)
    
    # Increase the skip counter by the number of rows read per chunk.
    skip <- skip + chunk_size
    
    # Stop if skip is larger than total number of lines
    if (skip > num_rows) {break}
    
    # Show Progress
    writeLines(paste("Finished Chunk", counter))
    counter <- counter + 1
    
  }
  
  
}



# Cleaning ----------------------------------------------------------------


file_paths <- list(
  paste0("data/split/option_", sprintf("%02d", 1:25), ".csv"),
  paste0("data/clean/option_", sprintf("%02d", 1:25), ".csv")
)


# Loop through each file path pair and apply the cleaning function
for (k in seq_along(1:25)) {
  
  start_time <- Sys.time()
  
  cleaning(file_paths[[1]][k], file_paths[[2]][k])
  
  end_time <- Sys.time()
  beepr::beep(1)
  writeLines("__________________________________________________")
  writeLines(paste("Finished cleaning", k, "of 25"))
  elapsed_time <- end_time - start_time
  elapsed_time
  writeLines("__________________________________________________")
  writeLines("Starting new file")
  
}

