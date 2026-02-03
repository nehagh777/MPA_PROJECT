#!/usr/bin/env Rscript

suppressPackageStartupMessages({
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)})



project_dir <- "/mainfs/lyceum/ng6g22/WOA_project"   # change if needed
out_dir     <- file.path(project_dir, "FIG6_thresholdCrossing_percent_facetedSSP")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)


in_rds <- file.path(project_dir, "diagnostics_filters_q95_vs_annualMean", "mpa_year_viable2025_minSp10.rds")

# Output file
out_png <- file.path(out_dir, "Fig6_MPA_threshold_crossing_percent_facetedSSP.png")

# Colours for thresholds (NOT SSPs)!!!!
# Light → dark = 10% → 50% → 100% (colourblind-friendly sequential blues)

thr_cols <- c(
  "10"  = "#c6dbef",
  "50"  = "#6baed6",
  "100" = "#08519c")


# Load + standardise to LONG format #################

df <- readRDS(in_rds)

# If it's already long with threshold + percent, standardise names
if (all(c("ssp", "year", "threshold") %in% names(df)) &&
any(c("pct_mpas", "percent", "pct") %in% names(df))) {
  
pct_col <- intersect(c("pct_mpas", "percent", "pct"), names(df))[1]
  
df_long <- df %>%
transmute(
ssp = as.character(ssp),
year = as.integer(year),
threshold = as.integer(as.character(threshold)),
pct_mpas = as.numeric(.data[[pct_col]]))
  

df_long <- df %>%
mutate(
ssp = as.character(ssp),
year = as.integer(year)) %>%
pivot_longer(
cols = all_of(thr_candidates),
names_to = "threshold",
values_to = "pct_mpas") %>%
mutate(
# Extract 10/50/100 from the column name
threshold = as.integer(gsub(".*?(10|50|100)$", "\\1", threshold)),
pct_mpas = as.numeric(pct_mpas)) %>%
select(ssp, year, threshold, pct_mpas)}

# Keep only your SSPs + thresholds
df_long <- df_long %>%
filter(ssp %in% c("ssp126", "ssp245", "ssp585"),
threshold %in% c(10, 50, 100)) %>%
  mutate(threshold = factor(threshold, levels = c(10, 50, 100)))


# Plot: percent over time, faceted by SSP, coloured by threshold (BLUES)

p <- ggplot(df_long, aes(x = year, y = pct_mpas, colour = threshold)) +
geom_line(linewidth = 1.2) +
facet_wrap(~ ssp, ncol = 1) +
scale_colour_manual(values = thr_cols, name = "Threshold (%)") +
coord_cartesian(ylim = c(0, 100)) +
labs(
title = "MPAs crossing species-loss thresholds over time",
    x = "Year",
    y = "% of MPAs") +
theme_minimal(base_size = 12) +
theme(
plot.title = element_text(face = "bold"),
legend.position = "bottom",
strip.text = element_text(face = "bold"))

ggsave(out_png, p, width = 8.5, height = 10, dpi = 300)
message("Saved: ", out_png)
