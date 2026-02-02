
# by removing NA temperatures we drop all the occurrences where the
# WOA SST join failed. N/A temps can be due to points falling on land
# (no WOA ocean temp), points falling outside the WOA ocean grid
  
ata_clean <- species_with_WOA_temperature %>% filter(!is.na(temp))
  
  

  
  #following Chaudhary methods, we DO NOT filter out deep species, but only remove 
  # physically impossible values. we do't want to bias the datset heavily toward 
  # shallow species , and also make 
  # methods inconsistent with the paper we are basing our work on.
  



cat("Total cleaned records:", nrow(data_clean), "\n")

# we have 7247454 cleaned records



data_clean <- data_clean %>%
  rename(
    decade = decade.x
  ) %>%
  
  select(
    # Keep these:
    id, ValidName, Kingdom, Phylum, Class, Order, Family, Genus,
    lon.x, lat.x, temp, DateCollected, month_recorded, year, decade, lon.y, lat.y,
    Depth, geometry,
    # Drop WOA-only columns:
     -decade.y
  )

saveRDS(data_clean, "species_with_WOA_temperature_clean.rds")

