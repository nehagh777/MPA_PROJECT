


#### with filtered species- USING THIS in final figure 1 graphs####

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(sf)
  library(readr)})


# Paths - locally ##########################

project_dir <- "C:/Users/nehag/Downloads/WOA_project"

# Species niche metrics (per species)
niche_path <- file.path(project_dir, "thermal_niches_species.rds")

# MPA-species table from intersection script
mpa_species_niche_path <- file.path(project_dir, "MPA_species_list_with_niches.rds")

# Annual CMIP6 SST summaries (contains model and ssp coverage per MPA) 
annual_sst_path <- file.path(project_dir, "MPA_annual_SST_all_models_ssps_buffer25km.rds")

# Output directory
out_dir <- file.path(project_dir, "figures_final", "Figure1_species_niches_RETAINED")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Filtering settings 
MIN_SPECIES_PER_MPA <- 10
REQUIRE_ALL_MODELS  <- TRUE
REQUIRE_ALL_SSPS    <- TRUE  

# Load data#########################

cat("Loading niches...\n")
niches <- readRDS(niche_path)
if ("sf" %in% class(niches)) niches <- sf::st_drop_geometry(niches)

cat("Loading MPA-species table...\n")
mpa_species_niche <- readRDS(mpa_species_niche_path)
if ("sf" %in% class(mpa_species_niche)) mpa_species_niche <- sf::st_drop_geometry(mpa_species_niche)

cat("Loading annual SST table...\n")
annual_sst <- readRDS(annual_sst_path)
if ("sf" %in% class(annual_sst)) annual_sst <- sf::st_drop_geometry(annual_sst)


# fixes WDPAID join issues ####

mpa_species_niche <- mpa_species_niche %>% mutate(WDPAID = as.character(WDPAID))
annual_sst        <- annual_sst        %>% mutate(WDPAID = as.character(WDPAID))


# Identify retained MPAs, actually used throughout the study


# (a) MPAs with >= MIN_SPECIES_PER_MPA species (from  species-MPA join)
mpa_counts <- mpa_species_niche %>%
group_by(WDPAID) %>%
summarise(
n_species = n_distinct(ValidName),
.groups = "drop") %>%
filter(n_species >= MIN_SPECIES_PER_MPA)

cat("MPAs with >= ", MIN_SPECIES_PER_MPA, " species: ", nrow(mpa_counts), "\n", sep="")

# (b) MPAs present in all climate models 
coverage <- annual_sst %>%
  group_by(WDPAID) %>%
  summarise(
    n_models = n_distinct(model),
    n_ssps   = n_distinct(ssp),
    .groups = "drop")

# Determine required numbers from the dataset itself
REQ_MODELS <- length(unique(annual_sst$model))
REQ_SSPS   <- length(unique(annual_sst$ssp))

if (!REQUIRE_ALL_MODELS) REQ_MODELS <- 1
if (!REQUIRE_ALL_SSPS)   REQ_SSPS   <- 1

retained_mpas <- mpa_counts %>%
inner_join(coverage, by = "WDPAID") %>%
filter(n_models >= REQ_MODELS,
n_ssps   >= REQ_SSPS)

cat("MPAs after model/SSP coverage filter: ", nrow(retained_mpas), "\n", sep="")

retained_mpa_ids <- retained_mpas$WDPAID


###### Identify retained species (species that occur in retained MPAs) #######

retained_species <- mpa_species_niche %>%
filter(WDPAID %in% retained_mpa_ids) %>%
distinct(ValidName) %>%
pull(ValidName)

cat("Retained species (occur in retained MPAs): ", length(retained_species), "\n", sep="")


# Filter the niche table to retained species ##############

# abs_lat sometimes missing; compute from mean_lat if needed
if (!("abs_lat" %in% names(niches))) {
if ("mean_lat" %in% names(niches)) {
niches <- niches %>% mutate(abs_lat = abs(mean_lat))} else {
stop("No abs_lat or mean_lat column found in thermal_niches_species.rds")}}

# Unify n_occ column if present
if (!("n_occ" %in% names(niches))) {
if ("n_occ.x" %in% names(niches)) niches <- niches %>% rename(n_occ = n_occ.x)
if ("n_occ.y" %in% names(niches) && !("n_occ" %in% names(niches))) niches <- niches %>% rename(n_occ = n_occ.y)
}

# Ensure required columns exist
stopifnot("q95_temp" %in% names(niches))
stopifnot("thermal_range" %in% names(niches))
stopifnot("abs_lat" %in% names(niches))
stopifnot("ValidName" %in% names(niches))

plot_df <- niches %>%
filter(ValidName %in% retained_species) %>%
transmute(
ValidName,
q95_temp      = as.numeric(q95_temp),
thermal_range = as.numeric(thermal_range),
abs_lat       = as.numeric(abs_lat),
n_occ         = if ("n_occ" %in% names(niches)) as.numeric(n_occ) else NA_real_) %>%
filter(is.finite(q95_temp), is.finite(thermal_range), is.finite(abs_lat))

cat("Rows in plot_df (retained species niche rows): ", nrow(plot_df), "\n", sep="")


#### plot + colourblind-safe so have used purple ############

base_theme <- theme_minimal(base_size = 12) +
theme(
plot.title    = element_text(face = "bold", size = 14),
plot.subtitle = element_text(size = 10),
axis.title    = element_text(size = 14),
axis.text     = element_text(size = 12))

# colour
pt_col  <- "#B834D5"


# Save Fig 1a–c (retained species only that have been used in the study) ##########


# Fig 1a: distribution of q95
p1a <- ggplot(plot_df, aes(x = q95_temp)) +
geom_histogram(bins = 50, colour = NA, fill = pt_col, alpha = 0.85) +
labs(
    title = paste0("a) Estimated species upper thermal tolerance (q95)"),
    x = "Upper thermal tolerance, q95 (°C)",
    y = "Number of species") +
base_theme

ggsave(file.path(out_dir, "Fig1a_retained_species_q95_distribution.png"),
p1a, width = 7.2, height = 4.8, dpi = 600)

# Fig 1b: q95 vs abs latitude
p1b <- ggplot(plot_df, aes(x = abs_lat, y = q95_temp)) +
geom_point(alpha = 0.08, size = 0.9, colour = pt_col) +
geom_smooth(method = "loess", se = FALSE, linewidth = 1.0, span = 0.9) +
labs(
    title = paste0("b) Estimated upper thermal tolerance declines with latitude"),
    x = "Absolute latitude (°)",
    y = "Upper thermal tolerance, q95 (°C)") +
  base_theme

ggsave(file.path(out_dir, "Fig1b_retained_q95_vs_absLatitude.png"),
p1b, width = 7.2, height = 4.8, dpi = 600)

# Fig 1c: distribution of thermal niche breadth
p1c <- ggplot(plot_df, aes(x = thermal_range)) +
  geom_histogram(bins = 50, colour = NA, fill = pt_col, alpha = 0.85) +
  labs(
    title = paste0("c) Estimated species thermal niche breadth"),
    x = "Thermal niche breadth (°C)",
    y = "Number of species"
  ) +
  base_theme

ggsave(file.path(out_dir, "Fig1c_retained_species_thermalRange_distribution.png"),
p1c, width = 7.2, height = 4.8, dpi = 600)

# Save a  diagnostic table ##################

diag <- tibble(
  min_species_per_mpa = MIN_SPECIES_PER_MPA,
  require_all_models  = REQUIRE_ALL_MODELS,
  require_all_ssps    = REQUIRE_ALL_SSPS,
  n_retained_mpas     = length(retained_mpa_ids),
  n_retained_species  = length(retained_species),
  req_models          = REQ_MODELS,
  req_ssps            = REQ_SSPS
)

write_csv(diag, file.path(out_dir, "Figure1_retained_diagnostics.csv"))

cat("\nSaved retained Figure 1 panels to:\n", out_dir, "\n", sep="")
cat("Also wrote: Figure1_retained_diagnostics.csv\n")

