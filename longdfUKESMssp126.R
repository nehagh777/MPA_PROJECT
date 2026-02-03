#!/usr/bin/env Rscript

library(RNetCDF)

cat("Opening NetCDF file...\n")

nc <- open.nc(
  "/mainfs/lyceum/ng6g22/WOA_project/data_input/CMIP6_raw/tos_UKESM1-0-LL_ssp126.nc"
)

# Read variables
lat <- var.get.nc(nc, "latitude")
lon <- var.get.nc(nc, "longitude")
tos <- var.get.nc(nc, "tos")

cat("Dimensions of tos:\n")
print(dim(tos))   # lon x lat x time

n_time <- dim(tos)[3]

# Build empty dataframe
sst_long <- data.frame(
  lon = numeric(),
  lat = numeric(),
  sst = numeric(),
  timestep = integer(),
  month = integer(),
  year = integer()
)

START_YEAR <- 2025
month <- 1
year  <- START_YEAR

cat("Looping over timesteps...\n")

for (t in seq_len(n_time)) {
  
  cat("Timestep:", t, "\n")
  
  slice <- tos[ , , t]
  
  df <- data.frame(
    lon = as.vector(lon),
    lat = as.vector(lat),
    sst = as.vector(slice)
  )
  
  df <- df[is.finite(df$sst), ]
  
  df$timestep <- t
  df$month    <- month
  df$year     <- year
  
  sst_long <- rbind(sst_long, df)
  
  # advance month/year
  if (month < 12) {
    month <- month + 1
  } else {
    month <- 1
    year  <- year + 1
  }
}

close.nc(nc)

cat("Finished.\n")
cat("Rows in SST dataframe:", nrow(sst_long), "\n")

saveRDS(
  sst_long,
  "/mainfs/lyceum/ng6g22/WOA_project/CMIP6_SST_long_UKESM_ssp126.rds")
