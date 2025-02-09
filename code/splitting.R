

# Clear all
rm(list = ls())
gc()

# Track Time
start_time <- Sys.time()

# Split the CSV into 25 files
bread::bfile_split(file = "data/option.csv", by_nfiles = 25, write_dir = "data/split/")

# Show final time and play notification
end_time <- Sys.time()
elapsed_time <- end_time - start_time
beepr::beep(5)
elapsed_time
