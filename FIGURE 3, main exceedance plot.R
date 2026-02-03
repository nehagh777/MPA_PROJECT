#!/usr/bin/env Rscript

suppressPackageStartupMessages({
library(dplyr)
library(ggplot2)
library(readr)})


# MPA-year dataset that contains pct_excluded for each MPA and year ########

in_rds <- "/mainfs/lyceum/ng6g22/WOA_project/mpa_year_viable2025_minSp10.rds"


# Output path ###################
out_png <- "Fig4_main_exceedance_pctExcluded_facetedSSP.png"


df <- readRDS(in_rds)


# WDPAID, mpa_name, ssp, year, n_species, pct_excluded ##############
stopifnot(all(c("ssp","year","pct_excluded") %in% names(df)))

df <- df %>%
mutate(
ssp = as.character(ssp),
year = as.integer(year),
pct_excluded = as.numeric(pct_excluded)) %>%
filter(is.finite(pct_excluded))


# SUMMARISE (mean, median, IQR) per SSP-year across MPAs#############

summ <- df %>%
  group_by(ssp, year) %>%
  summarise(
    n_mpas = n_distinct(WDPAID),
    mean_excl   = mean(pct_excluded, na.rm = TRUE),
    median_excl = median(pct_excluded, na.rm = TRUE),
    p25 = as.numeric(quantile(pct_excluded, 0.25, na.rm = TRUE, names = FALSE)),
    p75 = as.numeric(quantile(pct_excluded, 0.75, na.rm = TRUE, names = FALSE)),
    .groups = "drop"
  )


# COLOURS (colorblind-safe & consistent across all the figures) ######


ssp_cols <- c(
"ssp126" = "#0072B2",  # blue
"ssp245" = "#E69F00",  # orange
"ssp585" = "#CC79A7"   # magenta)

# Keep facet order stable
summ$ssp <- factor(summ$ssp, levels = c("ssp126","ssp245","ssp585"))


# PLOT (3 panels, median solid, mean dashed, IQR ribbon)- FACETED ##############

p <- ggplot(summ, aes(x = year)) +
geom_ribbon(
    aes(ymin = p25, ymax = p75, fill = ssp),
    alpha = 0.25,
    colour = NA) +
geom_line(
    aes(y = median_excl, colour = ssp),
    linewidth = 1.1) +
geom_line(
    aes(y = mean_excl, colour = ssp),
    linewidth = 0.9,
    linetype = "dashed") +
facet_wrap(~ssp, ncol = 1, scales = "fixed") +
scale_colour_manual(values = ssp_cols, drop = FALSE) +
scale_fill_manual(values = ssp_cols, drop = FALSE) +
labs(
    title = "Projected species exclusion through time",
    subtitle = "Solid = median, dashed = mean, ribbon = IQR across MPAs",
    x = "Year",
    y = "% species excluded") +
coord_cartesian(ylim = c(0, 100)) +
theme_minimal(base_size = 13) +
theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank())

ggsave(out_png, p, width = 10, height = 7, dpi = 300)
message("Saved: ", out_png)
