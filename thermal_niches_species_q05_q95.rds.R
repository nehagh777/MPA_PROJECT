
#!/usr/bin/env Rscript




  # by removing NA temperatures we drop all the occurrences where the
  # WOA SST join failed. N/A temps can be due to points falling on land
  # (no WOA ocean temp), points falling outside the WOA ocean grid
  
  data_clean <- species_with_WOA_temperature %>% filter(!is.na(temp))
  
  
  
  
  #following Chaudhary methods, we DO NOT filter out deep species, but only remove 
  # physically impossible values. we do't want to bias the datset heavily toward 
  # shallow species , remove entire clades (eg. deep invertebrates) and also make 
  # methods inconsistent with the paper we are basing our work on.
  
  
  
  
  cat("Total cleaned records:", nrow(data_clean), "\n")
  
  # we have 7247454 cleaned records
  
  
  
data_clean <- data_clean %>%
rename(decade = decade.x) %>%
    
select(
# Keep these names as others are not necessary:
id, ValidName, Kingdom, Phylum, Class, Order, Family, Genus,
lon.x, lat.x, temp, DateCollected, month_recorded, year, decade, lon.y, lat.y,
Depth, geometry,
# Drop WOA-only columns:
-decade.y)
  
saveRDS(data_clean, "species_with_WOA_temperature_clean.rds")
  
  




library(dplyr)
library(sf)

cat("Loading cleaned dataset...\n")

data_clean <- readRDS("/mainfs/lyceum/ng6g22/WOA_project/species_with_WOA_temperature_clean.rds")
data_clean <- data_clean %>% st_drop_geometry()

cat("Rows in cleaned data:", nrow(data_clean), "\n")
cat("Unique species:", length(unique(data_clean$ValidName)), "\n")

# 1. Compute latitude summaries

cat("Computing per-species latitude summaries...\n")

species_lat <- data_clean %>%
group_by(ValidName) %>%
summarise(
mean_lat = mean(lat.x, na.rm = TRUE),
abs_lat = abs(mean_lat),
n_occ = n()) %>%
ungroup()

cat("Species lat table complete. Species:", nrow(species_lat), "\n")


# Compute thermal niches

cat("Computing thermal niche metrics...\n")

thermal_niches <- data_clean %>%
group_by(ValidName) %>%
summarise(
    n_occ = n(),
    mean_temp = mean(temp, na.rm = TRUE),
    median_temp = median(temp, na.rm = TRUE),
    min_temp = min(temp, na.rm = TRUE),
    max_temp = max(temp, na.rm = TRUE),
    thermal_range = max_temp - min_temp,
    sd_temp = sd(temp, na.rm = TRUE),
    q25_temp = quantile(temp, 0.25, na.rm = TRUE),
    q75_temp = quantile(temp, 0.75, na.rm = TRUE)
    q05_temp = quantile(temp, 0.05, na.rm = TRUE),
    q95_temp = quantile(temp, 0.95, na.rm = TRUE)) %>%
ungroup()

cat("Thermal niche table complete. Species:", nrow(thermal_niches), "\n")

# Combine both tables, so each species row has both its thermal niche, its average 
# latitude and absolute latitude

cat("Joining thermal niches with latitude metrics...\n")

thermal_niches <- thermal_niches %>%
left_join(species_lat, by = "ValidName")


# Save output

outfile <- "/mainfs/lyceum/ng6g22/WOA_project/thermal_niches_species.rds"
saveRDS(thermal_niches, outfile)

cat("Saved thermal niches to:", outfile, "\n")
cat("Finished\n")
