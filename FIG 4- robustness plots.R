#!/usr/bin/env Rscript

suppressPackageStartupMessages({
library(dplyr)
library(ggplot2)})


# Paths #############################

project_dir <- "/mainfs/lyceum/ng6g22/WOA_project"

filtered_mpas_path <- file.path(project_dir, "MPA_filter_WDPAIDs_species10_models3.rds")
mpa_species_path   <- file.path(project_dir, "MPA_species_list_with_niches.rds")
mpa_climate_path   <- file.path(project_dir, "MPA_annual_SST_all_models_ssps_buffer25km.rds")

out_dir <- file.path(project_dir, "FIG5_robustness_9plots_viable2025_minSp10_OVERLAY")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)


# Settings ###########################

YEAR_MIN  <- 2025
YEAR_MAX  <- 2100
BASE_YEAR <- 2025

SSPS <- c("ssp126", "ssp245", "ssp585")

# 3×3 = 9 combos
species_metrics <- c(
q75 = "q75_temp",
q95 = "q95_temp",
max = "max_temp")

mpa_metrics <- c(
annual_mean = "annual_mean_sst",
annual_p95  = "annual_p95_monthly_mean_sst",
annual_max  = "annual_max_monthly_mean_sst"
)

# SSP colours (use SAME palette across all figures) ##########
ssp_cols <- c(
"ssp126" = "#1F77B4",  # blue
"ssp245" = "#FF7F0E",  # orange
"ssp585" = "#D6278B"   # magenta/pink)


# Load filtered MPAs (minSp10 + models3) ################

cat("\n=== Loading filtered MPAs ===\n")
mpas_keep <- readRDS(filtered_mpas_path) %>%
mutate(WDPAID = as.character(WDPAID)) %>%
distinct(WDPAID)

cat("Filtered MPAs:", nrow(mpas_keep), "\n")


# Load MPA-species list + niches #####################

cat("\n=== Loading MPA-species niches ===\n")
mpa_species <- readRDS(mpa_species_path) %>%
mutate(WDPAID = as.character(WDPAID)) %>%
select(WDPAID, NAME, ValidName, all_of(unname(species_metrics))) %>%
distinct() %>%
semi_join(mpas_keep, by = "WDPAID")

cat("MPA-species rows (distinct, filtered):", nrow(mpa_species), "\n")


# Load climate (annual MPA SST) and prep ##################

cat("\n=== Loading annual MPA climate ===\n")
clim_raw <- readRDS(mpa_climate_path) %>%
mutate(
WDPAID = as.character(WDPAID),
model  = as.character(model),
ssp    = as.character(ssp),
year   = as.integer(year)) %>%
filter(year >= YEAR_MIN, year <= YEAR_MAX) %>%
select(WDPAID, NAME, model, ssp, year, all_of(unname(mpa_metrics))) %>%
semi_join(mpas_keep, by = "WDPAID") %>%
filter(ssp %in% SSPS)

cat("Climate rows (filtered):", nrow(clim_raw), "\n")

# Collapse duplicates: 1 row per WDPAID×model×ssp×year ###############
cat("\n=== Collapsing climate duplicates (WDPAID×model×ssp×year) ===\n")
clim_collapsed <- clim_raw %>%
group_by(WDPAID, model, ssp, year) %>%
summarise(
NAME = dplyr::first(na.omit(NAME)),
across(all_of(unname(mpa_metrics)), ~ mean(.x, na.rm = TRUE)),
.groups = "drop")

cat("Climate rows after collapse:", nrow(clim_collapsed), "\n")

# Ensemble mean across models: 1 row per WDPAID×ssp×year ############
cat("\n=== Computing multi-model mean exposure per WDPAID×ssp×year ===\n")
clim_ens <- clim_collapsed %>%
group_by(WDPAID, ssp, year) %>%
summarise(
NAME = dplyr::first(na.omit(NAME)),
across(all_of(unname(mpa_metrics)), ~ mean(.x, na.rm = TRUE)),
.groups = "drop")

cat("Climate rows (ensemble):", nrow(clim_ens), "\n")


#MAIN FINDING----- compute % excluded per MPA-year-SSP with baseline viability filter #####

compute_mpa_year_exclusion_viable <- function(species_col, mpa_col) {
  
dat <- mpa_species %>%
select(WDPAID, NAME, ValidName, tol = all_of(species_col)) %>%
filter(is.finite(tol)) %>%
inner_join(
clim_ens %>% select(WDPAID, ssp, year, exposure = all_of(mpa_col)),
by = "WDPAID") %>%
filter(is.finite(exposure)) %>%
mutate(excluded = tol < exposure)
  
# Baseline excluded species per MPA+SSP in 2025
baseline_excl <- dat %>%
filter(year == BASE_YEAR, excluded) %>%
select(WDPAID, ssp, ValidName) %>%
distinct()
  
# Remove baseline-excluded species for all years (MPA-specific, SSP-specific)
dat_viable <- dat %>%
anti_join(baseline_excl, by = c("WDPAID", "ssp", "ValidName"))
  
# Summarise % excluded among baseline-viable species
dat_viable %>%
group_by(WDPAID, ssp, year) %>%
summarise(
n_species = n_distinct(ValidName),
pct_excluded = 100 * mean(excluded, na.rm = TRUE),
.groups = "drop")}


# Aggregate across MPAs per year-SSP (median + mean + IQR) ###############

aggregate_across_mpas <- function(df_mpa_year) {
df_mpa_year %>%
group_by(ssp, year) %>%
summarise(
mean_excl   = mean(pct_excluded, na.rm = TRUE),
median_excl = median(pct_excluded, na.rm = TRUE),
p25 = as.numeric(quantile(pct_excluded, 0.25, na.rm = TRUE, names = FALSE)),
p75 = as.numeric(quantile(pct_excluded, 0.75, na.rm = TRUE, names = FALSE)),
n_mpas = n_distinct(WDPAID),
.groups = "drop")}


# Plot + save (OVERLAY SSPs, non facted) #####################

make_plot_overlay <- function(df_agg, title_main, out_file) {
  
df_agg$ssp <- factor(df_agg$ssp, levels = c("ssp126","ssp245","ssp585"))
  
p <- ggplot(df_agg, aes(x = year)) +
    geom_ribbon(aes(ymin = p25, ymax = p75, fill = ssp), alpha = 0.18, colour = NA) +
    geom_line(aes(y = median_excl, colour = ssp), linewidth = 1.15) +
    geom_line(aes(y = mean_excl, colour = ssp), linewidth = 0.95, linetype = "dashed") +
    scale_colour_manual(values = ssp_cols, drop = FALSE) +
    scale_fill_manual(values = ssp_cols, drop = FALSE) +
    coord_cartesian(ylim = c(0, 100)) +
    labs(
    title = title_main,
    subtitle = paste0(
    "Baseline-viable filter: removed species already excluded in ", BASE_YEAR,
    " within each MPA (and SSP). Solid=median; dashed=mean; ribbon=IQR across MPAs."),
    x = "Year",
    y = "% species excluded (among baseline-viable species)",
    colour = "SSP",
    fill = "SSP") +
    theme_minimal(base_size = 12) +
    theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "right")
  
ggsave(out_file, plot = p, width = 10, height = 6, dpi = 300)
}


# Main loop: 9 plots (3×3) - ROBUSTNESS PLOTS #############

cat("\n=== Generating 9 OVERLAY plots (viable baseline species; minSp10 MPAs) ===\n")

for (sm_name in names(species_metrics)) {
for (mm_name in names(mpa_metrics)) {
    
species_col <- species_metrics[[sm_name]]
mpa_col <- mpa_metrics[[mm_name]]
    
cat("\n--- Combo: species=", sm_name, "(", species_col, ") | exposure=", mm_name, "(", mpa_col, ") ---\n")
    
df_mpa_year <- compute_mpa_year_exclusion_viable(species_col, mpa_col)
    
if (nrow(df_mpa_year) == 0) {
cat("No rows for this combo (skipping).\n")
next}
    
df_agg <- aggregate_across_mpas(df_mpa_year)
    
title_main <- paste0("Robustness (viable baseline): ", sm_name, " tolerance vs ", mm_name, " exposure")
    
out_file <- file.path(
out_dir,
paste0("FIG5_OVERLAY_viable2025_minSp10_species_", sm_name, "_mpa_", mm_name, ".png"))
    
make_plot_overlay(df_agg, title_main, out_file)
cat("Saved:", out_file, "\n")}}

cat("\n=== Done. Saved 9 OVERLAY ROBUSTNESS plots in: ", out_dir, " ===\n", sep = "")
