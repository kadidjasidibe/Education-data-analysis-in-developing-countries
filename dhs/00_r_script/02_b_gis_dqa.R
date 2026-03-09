# ------------------------------------------------------
# ---------------- DATA QUALITY ASSESSMENT -------------
# ------------------------------------------------------

# This script will be about performing data quality assessment for GIS shapefiles.
# The plan is as follow:


### 1) Checking for geometry validity
### 2) Harmonizing Coordinate Reference System (CRS) 
### 3) OSM completeness check
### 4) Raster check


# Indeed, as an analyst, these checks are import to assess the data quality before running any models

# ------------------------------------------------------


# ---------------------#

### 1) Checking for geometry validity

# ---------------------#

# GADM files often have broken geometries
civ_regions <- st_make_valid(civ_regions)
districts_ci <- st_make_valid(districts_ci)
districts_dhs_shp <- st_make_valid(districts_dhs_shp)

# ---------------------#

### 2) Coordinate Reference System (CRS) harmonization

# ---------------------#

# We should ensure global compatibility by making sure CRS are aligned. 

# We use the Raster CRS as the master to avoid transformation errors in exact_extract

if (st_crs(civ_regions) != crs(pop_raster)) {
  civ_regions <- st_transform(civ_regions, crs(pop_raster))
}
if (st_crs(educ_facilities) != crs(pop_raster)) {
  educ_facilities <- st_transform(educ_facilities, crs(pop_raster))
}
if (st_crs(districts_ci) != crs(pop_raster)) {
  districts_ci <- st_transform(districts_ci, crs(pop_raster))
}

if (st_crs(districts_dhs_shp) != crs(pop_raster)) {
  districts_dhs_shp <- st_transform(districts_dhs_shp, crs(pop_raster))
}



# ---------------------#

### 3) OSM completeness check

# ---------------------#

#OSM is often not complete that's why performing this DQA step is important
# The unique OSM shapefile is the shapefile about the number of schools (educ_facilities)

osm_completeness <- sum(!is.na(educ_facilities$operator_t)) / nrow(educ_facilities) * 100
print(paste0("OSM Completeness: ", round(osm_completeness, 2), "%"))

# We see that the completeness is just 22% for the type of school (public or 
# private etc)which is very low. Hence, I will only 
# use total school counts for the district-level supply variable, not the 
# specific school sectors

# ---------------------#

### 4) Raster check

# ---------------------#

# Looking for negative values or unexpected NAs in pop data

if (any(minmax(pop_raster)[1,] < 0)) {
  print("Warning: Negative values found in the raster population dataset")
}

# ---------------------#


message("GIS Data Quality Assessment script completed")