#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(purrr)})


##### Define paths#####


project_dir  <- "/mainfs/lyceum/ng6g22/WOA_project"
species_path <- file.path(project_dir, "newspecies_sf.rds")
woa_path     <- file.path(project_dir, "woa_sf.rds")
out_dir      <- file.path(project_dir, "joined_output")
final_path   <- file.path(project_dir, "species_with_WOA_temperature.rds")

cat("\n=== Starting WOA–species temperature join ===\n")
cat("Project directory: ", project_dir, "\n")

# Create the output folder if needed
dir.create(out_dir, showWarnings = FALSE)



###### Define decades and months #######


decades <- c("1955-1964", "1965-1974", "1975-1984",
  "1985-1994", "1995-2004", "2005-2014", "2015-2022")

months <- 1:12


###### Load datasets #####


cat("Loading species and WOA datasets...\n")

species_sf <- readRDS(species_path)
woa_sf     <- readRDS(woa_path)

cat("Species records loaded: ", nrow(species_sf), "\n")
cat("WOA cells loaded: ", nrow(woa_sf), "\n")


###### Loop and then process all the decade–month combinations ######


output_files <- c()

for (decade_now in decades) {
for (month_now in months) {
    
cat("\n---------------------------------------------\n")
cat("Processing decade:", decade_now, "month:", month_now, "\n")
    
# Subset species!
species_sub <- species_sf %>%
filter(decade == decade_now,
month_recorded == month_now)
    
    if (nrow(species_sub) == 0) {
      cat("  No species records → skipping.\n")
      next}
    
# Subset WOA data
woa_sub <- woa_sf %>%
filter(decade == decade_now,
month == month_now)
    
if (nrow(woa_sub) == 0) {
      cat("  No WOA grid cells → skipping.\n")
      next}
    
    cat("  Species rows:", nrow(species_sub), "\n")
    cat("  WOA grid rows:", nrow(woa_sub), "\n")
    cat("  Performing nearest-neighbour join...\n")
    
# Spatial join
joined <- st_join(species_sub, woa_sub, join = st_nearest_feature)
    
# Save file
outfile <- sprintf("joined_%s_month_%02d.rds", decade_now, month_now)
outfile_path <- file.path(out_dir, outfile)
    
saveRDS(joined, outfile_path)
output_files <- c(output_files, outfile_path)
    
cat("  Saved file:", outfile_path, "\n")}}

#### Combine all chunk files into ONE dataset ###


cat("\n=== Merging all output RDS files into one dataset ===\n")

if (length(output_files) == 0) {
  cat("ERROR: No output files were generated. Nothing to merge.\n")
  quit(status = 1)
}

# Read and merge everything
all_data <- map(output_files, readRDS) %>% bind_rows()

cat("Final merged rows:", nrow(all_data), "\n")

saveRDS(all_data, final_path)

cat("\n=== FINAL DATASET SAVED ===\n")
cat("File: ", final_path, "\n")
cat("===========================================\n")
