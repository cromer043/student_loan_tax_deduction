# =====================
# 📦 Setup and Load Dependencies
# =====================
install_and_load <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    } else {
      library(pkg, character.only = TRUE)
    }
  }
}

install_and_load(c("tidyverse", "sf", "sp", "tigris", "stringr"))
options(tigris_use_cache = TRUE)

# =====================
# 🌍 CRS and FIPS Setup
# =====================
map_crs <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
non_conus_fips <- c("02", "15", "60", "66", "69", "72", "78")

# ========================================
# 🔁 Reposition Territories for Mapping
# ========================================
transform_and_reposition <- function(sf_obj, fips_col, fips_code, rotation = 0, scale_div = 1, shift = c(0,0), quiet = FALSE) {
  piece <- sf_obj[sf_obj[[fips_col]] == fips_code | str_detect(sf_obj[[fips_col]], fips_code), ]
  
  if (nrow(piece) == 0) {
    if (!quiet) warning(paste("No match found for", fips_code, "in column", fips_col, "- skipping."))
    return(NULL)
  }
  
  piece <- st_transform(piece, crs = map_crs)
  sp_piece <- as(piece, "Spatial")
  
  suppressWarnings({
    sp_piece <- sp::elide(sp_piece, rotate = rotation)
    sp_piece <- sp::elide(sp_piece, scale = max(apply(sp::bbox(sp_piece), 1, diff)) / scale_div)
    sp_piece <- sp::elide(sp_piece, shift = shift)
  })
  
  proj4string(sp_piece) <- proj4string(as(sf_obj, "Spatial"))
  sf_geom <- st_as_sf(sp_piece)
  
  bbox_poly <- st_as_sfc(st_bbox(sf_geom))
  bbox_sf <- st_sf(geometry = bbox_poly, region = fips_code, crs = st_crs(sf_geom))
  
  return(list(geometry = sf_geom, bbox = bbox_sf))
}

# ============================================
# 🧱 Build Full Map with Repositioned Features
# ============================================
build_map_layer <- function(layer_fun, layer_name = "", year = 2024) {
  message(paste0("Building ", layer_name, " map for ", year, "..."))
  sf_data <- layer_fun(year = year, cb = TRUE) %>% st_transform(crs = map_crs)
  
  if ("STATEFP" %in% names(sf_data)) {
    fips_col <- "STATEFP"
    base_layer <- sf_data[!sf_data$STATEFP %in% non_conus_fips, ]
    match_codes <- c("02", "15", "72", "78", "66", "69", "60")
  } else if ("NAME" %in% names(sf_data)) {
    fips_col <- "NAME"
    base_layer <- sf_data[!str_detect(sf_data$NAME, ", AK|, HI|, PR|, VI|, GU|, MP|, AS"), ]
    match_codes <- c(", AK", ", HI", ", PR", ", VI", ", GU", ", MP", ", AS")
  } else {
    warning(paste0("Layer '", layer_name, "' does not match known structure—territory repositioning skipped."))
    return(list(map = sf_data, boxes = NULL))
  }
  rotation_list <- c("02" = -50, "15" = -35, "72" = 13, "78" = 13, "66" = -65, "69" = -55, "60" = -55,
                     ", AK" = -50, ", HI" = -35, ", PR" = 13, ", VI" = 13, ", GU" = -65, ", MP" = -55, ", AS" = -55)
  scale_list <- c("02" = 1.8, "15" = 1, "72" = 0.5, "78" = 0.25, "66" = 0.15, "69" = 0.85, "60" = 0.25,
                  ", AK" = 1.8, ", HI" = 1, ", PR" = 0.5, ", VI" = 0.25, ", GU" = 0.15, ", MP" = 0.85, ", AS" = 0.25)
  shift_list <- list(
    "02" = c(-2700000, -3200000), "15" = c(5800000, -2500000), "72" = c(600000, -3000000),
    "78" = c(1500000, -3000000), "66" = c(1200000, -3600000), "69" = c(300000, -3800000),
    "60" = c(-2300000, -3800000), ", AK" = c(-2400000, -3200000), ", HI" = c(5800000, -2500000),
    ", PR" = c(600000, -3000000), ", VI" = c(1500000, -3000000), ", GU" = c(1200000, -3600000),
    ", MP" = c(300000, -3800000), ", AS" = c(-2300000, -3800000)
  )
  
  repositioned <- lapply(match_codes, function(code) {
    transform_and_reposition(
      sf_obj = sf_data, fips_col = fips_col, fips_code = code,
      rotation = rotation_list[[code]], scale_div = scale_list[[code]], shift = shift_list[[code]]
    )
  })
  
  repositioned_clean <- Filter(Negate(is.null), repositioned)
  geom_list <- lapply(repositioned_clean, `[[`, "geometry")
  bbox_list <- lapply(repositioned_clean, `[[`, "bbox")
  
  full_map <- rbind(base_layer, do.call(rbind, geom_list))
  bbox_layer <- do.call(rbind, bbox_list)
  
  return(list(map = full_map, boxes = bbox_layer))
}

# =====================
# 🤷🏼‍♂️ Build and Save Map with Labels
# =====================
state_out <- build_map_layer(tigris::states, "State")
county_out <- build_map_layer(tigris::counties, "County")
cbsa_out <- build_map_layer(tigris::core_based_statistical_areas, "CBSA")
tract_out <- build_map_layer(tigris::tracts, "Tract")
place_out <- build_map_layer(tigris::places, "Place")
zcta_out <- build_map_layer(tigris::zctas, "ZCTA", year = 2020)

territory_codes <- c("AK", "HI", "PR", "VI", "GU", "MP", "AS")

# Create inset boxes using custom size logic
make_box <- function(geometry, region = NA_character_, padding = 30000) {
  geom_parts <- st_cast(st_geometry(geometry), "POLYGON")
  name <- region
  
  if (!is.na(name) && name == "AS") {
    areas <- as.numeric(st_area(geom_parts))
    top_parts <- geom_parts[order(areas, decreasing = TRUE)[1:min(3, length(geom_parts))]]
  } else {
    top_parts <- geom_parts[as.numeric(st_area(geom_parts)) > 1e7]
    if (length(top_parts) == 0) top_parts <- geom_parts
  }
  
  unioned <- st_union(top_parts)
  bounds <- st_bbox(unioned)
  
  coords <- matrix(c(
    bounds$xmin - padding, bounds$ymin - padding,
    bounds$xmax + padding, bounds$ymin - padding,
    bounds$xmax + padding, bounds$ymax + padding,
    bounds$xmin - padding, bounds$ymax + padding,
    bounds$xmin - padding, bounds$ymin - padding
  ), ncol = 2, byrow = TRUE)
  
  st_polygon(list(coords))
}

custom_boxes <- st_sf(
  STUSPS = territory_codes,
  geometry = st_sfc(
    lapply(territory_codes, function(code) {
      geom <- state_out$map %>% filter(STUSPS == code)
      make_box(geom, region = code, padding = 30000)
    }),
    crs = st_crs(state_out$map)
  )
)

# Harmonize attributes for combining
missing_cols <- setdiff(names(state_out$map), names(custom_boxes))
for (col in missing_cols) custom_boxes[[col]] <- NA
custom_boxes <- custom_boxes[, names(state_out$map)]

# Combine states and inset boxes
combined_map <- rbind(state_out$map, custom_boxes)

# Compute centroids for repositioned boxes instead of state geometries
real_boxes <- custom_boxes %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble() %>%
  mutate(STUSPS = territory_codes) %>%
  group_by(STUSPS) %>%
  slice(1) %>%
  ungroup()

# Label names
state_names <- tibble(
  STUSPS = territory_codes,
  label = c("Alaska", "Hawai‘i", "Puerto Rico", "U.S. Virgin Islands", "Guam", "Northern Mariana Islands", "American Samoa")
)

# Join labels and shift below boxes
label_coords <- real_boxes %>%
  left_join(state_names, by = "STUSPS") %>%
  mutate(Y = case_when(
    STUSPS == "AK" ~ Y - 685000,
    STUSPS == "HI" ~ Y - 260000,
    STUSPS == "PR" ~ Y - 150000,
    STUSPS == "VI" ~ Y - 250000,
    STUSPS == "GU" ~ Y - 220000,
    STUSPS == "MP" ~ Y - 450000,
    STUSPS == "AS" ~ Y - 190000,
    TRUE ~ Y
  ))

centers_sf <- st_as_sf(label_coords, coords = c("X", "Y"), crs = map_crs)

# Save shapefiles
write_sf(combined_map, "state_layers.gpkg", layer = "states")
write_sf(centers_sf, "state_layers.gpkg", layer = "labels", append = TRUE)
write_sf(county_out$map, "state_layers.gpkg", layer = "counties", append = TRUE)
write_sf(cbsa_out$map, "state_layers.gpkg", layer = "cbsa", append = TRUE)
write_sf(tract_out$map, "state_layers.gpkg", layer = "tracts", append = TRUE)
write_sf(place_out$map, "state_layers.gpkg", layer = "places", append = TRUE)
write_sf(zcta_out$map, "state_layers.gpkg", layer = "ZCTA", append = TRUE)

# ====================================
# 🚫 Remove Territories in Inset Boxes
# ====================================
territory_abbrevs <- c("AK", "HI", "PR", "VI", "GU", "MP", "AS")

# Load full map layer
full_states <- read_sf("state_layers.gpkg", layer = "states")

# Remove boxed states/territories
states_no_boxes <- full_states %>%
  filter(!STUSPS %in% territory_abbrevs)

# Save to new GPKG layer
write_sf(states_no_boxes, "state_layers.gpkg", layer = "states_no_boxes", append = TRUE)

# ===================
# 🗉️ Plot with Labels
# ===================
map_sf <- read_sf("state_layers.gpkg", layer = "states")
label_sf <- read_sf("state_layers.gpkg", layer = "labels")

ggplot() +
  geom_sf(data = map_sf, fill = NA, color = "black") +
  geom_sf_text(data = label_sf, aes(label = label), size = 3, fontface = "bold") +
  coord_sf(crs = st_crs(map_sf)) +
  theme_minimal() +
  labs(title = "U.S. States and Territories with Labels Below Insets") +
  theme_void()+
  theme(
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
