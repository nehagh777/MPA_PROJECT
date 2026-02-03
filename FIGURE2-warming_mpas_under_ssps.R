#!/usr/bin/env Rscript

suppressPackageStartupMessages({
library(dplyr)
library(ggplot2)
library(readr)})


# Paths ###########################

project_dir <- "/mainfs/lyceum/ng6g22/WOA_project"
in_rds  <- file.path(project_dir, "MPA_annual_SST_all_models_ssps_buffer25km.rds")
out_png <- file.path(project_dir, "Fig3A_MPA_warming_anomaly_timeseries_noGrey.png")


# Load #########################

cat("Reading:", in_rds, "\n")
dat <- readRDS(in_rds)


req <- c("WDPAID","ssp","model","year","annual_mean_sst")
miss <- setdiff(req, names(dat))
if (length(miss) > 0) stop("Missing columns: ", paste(miss, collapse=", "))

dat <- dat %>%
mutate(
WDPAID = as.character(WDPAID),
year = as.integer(year),
annual_mean_sst = as.numeric(annual_mean_sst),
ssp = as.character(ssp),
model = as.character(model)) %>%
filter(!is.na(annual_mean_sst))

# Ensemble mean across models within each MPA-year-SSP #################

mpa_year_ssp_ens <- dat %>%
group_by(WDPAID, ssp, year) %>%
summarise(
sst_ens_mean = mean(annual_mean_sst, na.rm = TRUE),
n_models = n_distinct(model),
.groups = "drop")




#  Compute anomaly relative to 2025 baseline per MPA (within SSP) ##########

baseline_2025 <- mpa_year_ssp_ens %>%
  filter(year == 2025) %>%
  select(WDPAID, ssp, baseline_2025 = sst_ens_mean)

mpa_anom <- mpa_year_ssp_ens %>%
  left_join(baseline_2025, by = c("WDPAID","ssp")) %>%
  filter(!is.na(baseline_2025)) %>%
  mutate(anom = sst_ens_mean - baseline_2025)


# Summarise across MPAs each year (mean + median + IQR) #################

summ <- mpa_anom %>%
group_by(ssp, year) %>%
summarise(
mean_anom   = mean(anom, na.rm = TRUE),
median_anom = median(anom, na.rm = TRUE),
p25 = as.numeric(quantile(anom, 0.25, na.rm = TRUE)),
p75 = as.numeric(quantile(anom, 0.75, na.rm = TRUE)),
n_mpas = n_distinct(WDPAID),
.groups = "drop"
)

cat("MPAs per SSP (median across years):\n")
print(summ %>% group_by(ssp) %>% summarise(median_n_mpas = median(n_mpas)))

# PLOT THE GRAPH- change in annual mean relative to 2025 ###################################
p <- ggplot(summ, aes(x = year)) +
  geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.20) +
  geom_line(aes(y = mean_anom), linewidth = 1.2) +
  geom_line(aes(y = median_anom), linewidth = 1.0, linetype = "dashed") +
  facet_wrap(~ ssp, ncol = 1) +
  labs(
  title = "Warming of MPAs under CMIP6 SSP scenarios",
  subtitle = "Anomaly relative to each MPA's 2025 ensemble-mean SST (band = IQR; solid = mean; dashed = median)",
  x = "Year",
  y = expression(Delta*" annual mean SST in MPAs (°C)")) +
  theme_minimal(base_size = 10) +
  theme(
  legend.position = "none",
  strip.text = element_text(face = "bold"),
  plot.title = element_text(face = "bold"),
  panel.grid.minor = element_blank())

ggsave(out_png, p, width = 8.5, height = 10, dpi = 300)
cat("Saved:", out_png, "\n")
