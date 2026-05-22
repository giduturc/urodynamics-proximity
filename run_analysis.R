get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }

  frame_files <- vapply(sys.frames(), function(frame) {
    if (!is.null(frame$ofile)) frame$ofile else NA_character_
  }, character(1))
  frame_files <- frame_files[!is.na(frame_files)]
  if (length(frame_files) > 0) {
    return(dirname(normalizePath(frame_files[length(frame_files)])))
  }

  normalizePath(getwd())
}

repo_dir <- get_script_dir()
project_path <- function(...) file.path(repo_dir, ...)
use_cached_geocodes <- !tolower(Sys.getenv("URODYNAMICS_REFRESH_GEOCODES", "false")) %in% c("1", "true", "yes")
female_population_var <- "B01001_026"
female_population_year <- 2023
miles_per_meter <- 0.000621371
distance_breaks_miles <- c(0, 5, 10, 25, 50, 100, 200, 500, 1000, 100000)

packs <- c("dplyr", "data.table", "janitor", "stringr", "Hmisc",
           "tidygeocoder", "tidycensus", "ggplot2", "ggpubr", "ggthemes",
           "ggbeeswarm", "geosphere", "sf", "usmap", "patchwork")
lapply(packs, require, character.only = TRUE)

cat("=== Loading Medicare data ===\n")
x <- fread(project_path("Medicare_Urodynamics_Data.csv")) %>% data.table
cat("Rows:", nrow(x), "\n")

cat("=== Cleaning column names ===\n")
x <- clean_names(x)
colnames(x) <- colnames(x) %>% str_remove_all("rndrng_|prvdr_")
cat("Columns:", paste(colnames(x), collapse=", "), "\n")

x[, city_state_zip := paste(city, state_abrvtn, zip5, sep = ", ")]
collapse_address <- function(...) {
  parts <- list(...)
  vapply(seq_along(parts[[1]]), function(i) {
    vals <- vapply(parts, function(part) {
      val <- part[[i]]
      if (is.na(val)) "" else as.character(val)
    }, character(1))
    vals <- trimws(vals)
    vals <- vals[nzchar(vals)]
    paste(vals, collapse = ", ")
  }, character(1))
}

normalize_address_string <- function(address) {
  parts <- strsplit(address, ",", fixed = TRUE)
  vapply(parts, function(part) {
    part <- trimws(part)
    part <- part[nzchar(part)]
    paste(part, collapse = ", ")
  }, character(1))
}

load_cached_or_fetch_female_population <- function(geography, cache_path = NULL) {
  if (!is.null(cache_path) && file.exists(cache_path)) {
    cat("Loading cached female population data from", basename(cache_path), "\n")
    return(fread(cache_path) %>% as.data.table())
  }

  cat("Fetching", female_population_year, "ACS 5-year female population for", geography, "from Census API\n")
  if (identical(geography, "tract")) {
    tract_states <- c(state.abb, "DC", "PR")
    female_pop <- rbindlist(lapply(tract_states, function(state_code) {
      cat("  State:", state_code, "\n")
      get_acs(
        geography = "tract",
        state = state_code,
        year = female_population_year,
        survey = "acs5",
        variables = female_population_var
      ) %>%
        as.data.table()
    }), fill = TRUE)
  } else {
    female_pop <- get_acs(
      geography = geography,
      year = female_population_year,
      survey = "acs5",
      variables = female_population_var
    ) %>%
      as.data.table()
  }

  if (!is.null(cache_path)) {
    fwrite(female_pop, cache_path)
    cat("Saved female population cache to", basename(cache_path), "\n")
  }

  female_pop
}

x[, full_original_st := collapse_address(st1, st2)]

cat("=== Cleaning addresses ===\n")
x[, st1 := st1 %>%
    str_replace_all(c("\\bSq\\b"  = "Square",
                      "\\bAve\\b"  = "Avenue",
                      "\\bPlz\\b"  = "Plaza",
                      "\\bRd\\b"  = "Road",
                      "\\bPkwy\\b"  = "Parkway",
                      "\\bBlvd\\b"  = "Boulevard",
                      "\\bBl\\b"  = "Boulevard",
                      "\\bSte\\b"  = "Street",
                      "\\bSt\\b"  = "Street",
                      "\\bFwy\\b"  = "Freeway",
                      "\\bDr\\b"  = "Drive",
                      "\\bCir\\b"  = "Circle",
                      "\\bBldg\\b" = "Building",
                      "\\bLn\\b" = "Lane",
                      "\\bHls\\b" = "Hills",
                      "\\bTrl\\b" = "Trail",
                      "\\bCtr\\b" = "Center",
                      "\\bHwy\\b" = "Highway",
                      "\\bMem\\b" = "Memorial",
                      "\\." = "",
                      "," = "",
                      "-" = "",
                      "#" = "",
                      " [0-9+]+$" = "",
                      "(?i) [a-z+][0-9+]+$" = "",
                      " .$" = "",
                      "(?<=[0-9])[a-z]" = "")) %>%
    str_remove_all("Square|Avenue|Plaza|Road|Boulevard|Street|Drive|Parkway|Freeway|Suite|Circle|Building|Lane|Hills|Trail|Highway|\\b..\\b") %>%
    str_squish]

cat("=== Coalescing by location ===\n")
x[, address := collapse_address(st1, city, state_abrvtn, zip5)]

###Convert tot_benes, tot_srvcs, and avg_mdcr_pymt_amt to numeric class type
x$tot_benes <- as.numeric(x$tot_benes)
x$tot_srvcs <- as.numeric(x$tot_srvcs)
x$avg_mdcr_pymt_amt <- str_remove_all(x$avg_mdcr_pymt_amt, "[$]") ###removes dollar sign
x$avg_mdcr_pymt_amt <- as.numeric(x$avg_mdcr_pymt_amt)

locs <- x[, .(tot_benes = sum(tot_benes),
      tot_srvcs = sum(tot_srvcs),
      avg_mdcr_pymt_amt = mean(avg_mdcr_pymt_amt),
      city = city %>% first,
      st = full_original_st %>% first,
      state = state_abrvtn %>% first,
      zip5 = zip5 %>% first),
  by = address]
locs[, address := collapse_address(st, city, state, zip5)]

# Recoalesce
locs <- locs[, .(tot_benes = sum(tot_benes),
      tot_srvcs = sum(tot_srvcs),
      avg_mdcr_pymt_amt = mean(avg_mdcr_pymt_amt),
      city = city %>% first,
      st = st %>% first,
      state = state %>% first,
      zip5 = zip5 %>% first),
  by = address]

cat("Unique locations:", nrow(locs), "\n")
cat("States represented:", locs[, uniqueN(state)], "\n")

cat("=== Geocoding ===\n")
geocode_cache_path <- project_path("geocoded_urodynamics_locations.csv")
cache_cols <- c("address", "lat", "long")

if (use_cached_geocodes && file.exists(geocode_cache_path)) {
  cat("Loading cached geocodes from geocoded_urodynamics_locations.csv\n")
  cached_geocodes <- fread(geocode_cache_path) %>%
    as.data.table() %>%
    .[, address := normalize_address_string(address)] %>%
    .[, ..cache_cols] %>%
    unique(by = "address")
} else {
  cached_geocodes <- data.table(address = character(), lat = numeric(), long = numeric())
}

geo_codes_arc <- merge(locs, cached_geocodes, by = "address", all.x = TRUE, sort = FALSE)
missing_addresses <- geo_codes_arc[is.na(lat) | is.na(long), .(address)]

if (!use_cached_geocodes && nrow(cached_geocodes) > 0) {
  missing_addresses <- locs[, .(address)]
}

if (nrow(missing_addresses) > 0) {
  cat("Geocoding", nrow(missing_addresses), "addresses from ArcGIS\n")
  fresh_geocodes <- missing_addresses %>%
    geocode(address = address,
            method = "arcgis",
            verbose = TRUE) %>%
    as.data.table() %>%
    .[, ..cache_cols]

  cached_geocodes <- rbindlist(list(cached_geocodes, fresh_geocodes), fill = TRUE) %>%
    unique(by = "address")
  geo_codes_arc <- merge(locs, cached_geocodes, by = "address", all.x = TRUE, sort = FALSE)
  fwrite(geo_codes_arc, geocode_cache_path)
  cat("Saved geocoded locations to geocoded_urodynamics_locations.csv\n")
} else {
  cat("All geocodes resolved from cache.\n")
}

cat("Geocoded rows:", nrow(geo_codes_arc), "\n")
cat("Missing coordinates:", sum(is.na(geo_codes_arc$lat)), "\n")

# States summary
states_n <- locs[, .(n = .N), by = state]
states_n <- states_n[, state.abb[state.abb %nin% state]] %>%
  add_row(states_n, state = ., n = 0)

cat("\n=== States with 0 urodynamics centers ===\n")
print(states_n[n == 0])

cat("\n=== Top 10 states by center count ===\n")
print(states_n %>% arrange(-n) %>% head(10))

cat("\n=== Generating boxplot ===\n")
f1 <- ggplot(data = locs, aes(y = tot_benes)) +
  geom_beeswarm(data = locs, alpha = 0.4, size = 6, cex = 4,
                color = "lightblue3", aes(y = tot_benes, x= 0)) +
  geom_boxplot(outlier.shape = NA, fill = NA, color = "maroon", linewidth = 2) +
  scale_y_continuous(trans = "log",
                     name = "Total number of beneficiaries (patients)",
                     breaks = c(10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)) +
  scale_x_continuous(name = NULL, labels = NULL, breaks = NULL) +
  theme_pubclean() +
  theme(text = element_text(size = 23, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.y = element_text(size = 25, face = "bold"),
        axis.line = element_line(colour = "black", linewidth = 1.2))
ggsave(f1, filename = "Boxplot.png", dpi = 600, height = 10, width = 10)
cat("Boxplot saved.\n")

cat("\n=== Generating state N map ===\n")
points_sf <- usmap_transform(data = geo_codes_arc, input_names = c("long", "lat"))
coords <- sf::st_coordinates(points_sf)
points_df <- cbind(as.data.frame(points_sf), x = coords[,1], y = coords[,2])

f2 <- plot_usmap(data = states_n, values = "n") +
  scale_fill_gradient_tableau(name = "Number of urodynamics testing centers") +
  geom_point(data = points_df, aes(x = x, y = y),
             col = "maroon", alpha = 0.4, size = 5) +
  ggtitle("State-level availability of urodynamics testing centers") +
  theme(legend.text = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.6, 0.001),
        plot.title=element_text(face = "bold", hjust = 0.5, size = 25),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  guides(fill = guide_colourbar(direction = "horizontal", title.position = "top",
                                barwidth = 20, barheight = 3,
                                ticks.colour = "black", ticks.linewidth = 0))
ggsave(filename = "N per state.png", bg = "white", dpi = 600, width = 16, height = 9)
cat("State N map saved.\n")

cat("\n=== State-level density (Figure 1A) ===\n")
states_n <- states_n %>%
  left_join(data.frame(
              state_full = c(state.name, "District of Columbia", "Puerto Rico"),
              state_abb = c(state.abb, "DC", "PR")
            ),
            by = c("state" = "state_abb"))
state_pop <- load_cached_or_fetch_female_population("state")
states_n <- states_n %>%
  left_join(state_pop %>% select(NAME, estimate) %>% rename(pop = estimate),
            by = c("state_full" = "NAME"))
states_n[, uro_density := 1000000*(n/pop)]
states_n[, state := factor(state, states_n %>% arrange(-uro_density) %>% pull(state))]

f1a <- plot_usmap(data = states_n, values = "uro_density") +
  geom_point(data = points_df, aes(x = x, y = y),
             col = "grey20", alpha = 0.4, size = 3) +
  scale_fill_distiller(palette = "RdYlBu", direction = +1,
                       name = "Number of urodynamics testing centers per million women") +
  ggtitle(waiver()) +
  theme(legend.text = element_text(size = 18, face = "bold"),
        legend.title = element_text(size = 16, face = "bold"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.6, 0.001),
        plot.title=element_text(face = "bold", hjust = 0.5, size = 25),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) +
  guides(fill = guide_colourbar(direction = "horizontal", title.position = "top",
                                barwidth = 20, barheight = 3,
                                ticks.colour = "black", ticks.linewidth = 0))
ggsave(filename = "Figure 1A.png", bg = "white", dpi = 1000, width = 16, height = 9)
cat("Figure 1A saved.\n")

cat("\n=== Rural/Urban analysis ===\n")
rudata <- sf::read_sf(project_path("RU Data/tl_2021_us_cbsa.shp"))
rudata <- rudata %>%
  rename(lon = INTPTLON, lat = INTPTLAT) %>%
  mutate(across(.cols = c("lon", "lat"), ~ as.numeric(.))) %>%
  clean_names %>%
  filter(lsad == "M1")
geo_uro_points <- geo_codes_arc %>% st_as_sf(coords = c("long", "lat"))
st_crs(geo_uro_points) <- "WGS84"
geo_uro_points <- st_transform(geo_uro_points, crs = "NAD83")
ints <- st_intersects(rudata, geo_uro_points)
metro <- lapply(1:length(ints), function(i) length(ints[[i]]) > 0) %>%
  unlist %>% factor %>% summary
cat("Metropolitan areas with urodynamics centers:\n")
print(metro)

cat("\n=== Census tract distance analysis ===\n")
gdata <- sf::st_read(dsn = project_path("tlgdb_2021_a_us_substategeo.gdb"), layer = "Census_Tract")
gdata <- gdata %>%
  rename(lon = INTPTLON, lat = INTPTLAT) %>%
  mutate(across(.cols = c("lon", "lat"), ~ as.numeric(.)))

cat("Computing nearest-center distances by census tract...\n")
tract_points <- st_as_sf(gdata, coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  st_transform(5070)
uro_points <- st_as_sf(geo_codes_arc, coords = c("long", "lat"), crs = 4326, remove = FALSE) %>%
  st_transform(5070)
nearest_uro_idx <- st_nearest_feature(tract_points, uro_points)
gdata$dist <- as.numeric(
  st_distance(tract_points, uro_points[nearest_uro_idx, ], by_element = TRUE)
) * miles_per_meter

female_tract_cache_path <- project_path("ACS 2023 5-year Female Tract Population.csv")
ct_data <- load_cached_or_fetch_female_population("tract", female_tract_cache_path)
ct_data <- ct_data %>% rename(pop = estimate)
ct_data[, state := fifelse(NAME %>% str_detect(";"),
                           word(NAME, -1, sep = "; "),
                           word(NAME, -1, sep = ", "))]
gdata$GEOID = str_remove(gdata$GEOID, "^0+") ### removes leading 0's from gdata$GEOID
gdata <- merge(gdata, ct_data, by = "GEOID")
rm(ct_data)
gdata <- gdata %>% mutate(pop = pop %>% as.numeric)

cat("\n=== Generating Figure 1B (distance map) ===\n")
make_dist_plot <- function(data, show_legend = TRUE) {
  p <- ggplot(data = data,
       aes(fill = cut(dist, distance_breaks_miles),
            color = after_scale(fill))) +
    geom_sf(color = NA, linewidth = 0) +
    scale_fill_brewer(palette = "RdYlBu", direction = -1,
                      name = "Distance (miles)",
                      labels = c("Less than 5", "5 to 10", "10 to 25",
                                 "25 to 50", "50 to 100", "100 to 200",
                                 "200 to 500", "500 or greater")) +
    theme(legend.text = element_text(size = 18, face = "bold"),
          legend.title = element_text(size = 16, face = "bold"),
          legend.background = element_blank(),
          legend.key = element_blank(),
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank(),
          axis.title = element_blank())
  if (!show_legend) p <- p + theme(legend.position = "none")
  p
}

f1b <- make_dist_plot(gdata %>% filter(state %nin% c("Hawaii", "Alaska", "Puerto Rico"))) +
  theme(legend.position = c(0.9, 0.25)) + ggtitle(waiver())

pr <- make_dist_plot(gdata %>% filter(state == "Puerto Rico"), FALSE) +
  coord_sf(xlim = c(-68.5, -64.5), ylim = c(17.8, 18.8))
hw <- make_dist_plot(gdata %>% filter(state == "Hawaii"), FALSE) +
  coord_sf(xlim = c(-180, -150), ylim = c(18, 30))
ak <- make_dist_plot(gdata %>% filter(state == "Alaska"), FALSE) +
  coord_sf(xlim = c(-180, -130), ylim = c(51, 72))

f1b <- f1b +
  inset_element(ak, left = 0, right = 0.3, top = 0.3, bottom = 0) +
  inset_element(hw, left = 0.2, right = 0.4, top = 0.3, bottom = 0) +
  inset_element(pr, left = 0.75, right = 0.85, top = 0.1, bottom = 0)
ggsave(filename = "Figure 1B.png", dpi = 1000, width = 16, height = 9)
cat("Figure 1B saved.\n")

cat("\n=== Summary Statistics ===\n")
cat("Total unique urodynamics testing centers:", locs[, uniqueN(address)], "\n")

gdata_dt <- gdata %>% select(pop, state, dist) %>% data.table
cat("\nWeighted median distance for women (25th/50th/75th percentile):\n")
print(gdata_dt[, wtd.quantile(dist, pop, probs = c(0.25, 0.5, 0.75, 1)) %>% round(1)])

cat("\nWomen >100 miles from urodynamics center:")
cat("\n  Millions:", round(gdata_dt[dist > 100, pop %>% sum]/1000000, 1))
cat("\n  Percentage:", round(100*gdata_dt[dist > 100, pop %>% sum]/gdata_dt[, pop %>% sum], 1), "%\n")

cat("\nMedian distance by state for women:\n")
print(gdata_dt[, .(mdist = wtd.quantile(dist, pop, probs = 0.5) %>% round(0)), by = state] %>% arrange(-mdist))

cat("\nMetro areas with urodynamics:\n")
print(metro)
cat("Percent:", round(100*metro[1]/sum(metro[1:2]), 1), "%\n")

ggplot(gdata_dt, aes(x=1, y=dist, weight=pop))+
    geom_boxplot(outliers=FALSE)+
   labs(x = "United States Population", y = "Median Weighted Distance (miles)")+
  ggtitle("Median Weighted Distance") + 
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
   

cat("\n=== ANALYSIS COMPLETE ===\n")
