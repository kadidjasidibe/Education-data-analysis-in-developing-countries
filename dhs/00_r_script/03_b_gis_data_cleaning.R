# ------------------------------------------------------
# ---------------- DATA CLEANING --------------
# ------------------------------------------------------

# This script will be about cleaning GIS datasets based on the DQA. The plan is 
# as follow: 

### 1) Clean shape file' string variable 
### 2) Calculating Supply indicators

# ------------------------------------------------------


# ---------------------#

### 1) Clean shape file' string variable 
# ---------------------#

# By creating a function (universal for any country)

clean_geo_names <- function(x) {
  toupper(stringi::stri_trans_general(x, "Latin-ASCII")) %>% 
    str_replace_all("[[:punct:]]", " ") %>% 
    str_squish()
}

civ_regions <- civ_regions %>% mutate(NAME_1 = clean_geo_names(NAME_1))
districts_ci <- districts_ci %>% mutate(NAME_1 = clean_geo_names(NAME_1), NAME_2 = clean_geo_names(NAME_2))


# Merge education data with shape file using region names
ci_merged <- civ_regions %>%
  left_join(educ_by_region, by = c("NAME_1" = "hv024_name"))


write.csv(ci_merged, file.path(path_dhs, "dhs", "02_Presentations", "Visualization", "Tables", paste0(country, "_ci_merged.csv")))

saveRDS(ci_merged, file.path(path_dhs, "dhs", "02_Presentations", "Visualization", "Tables", paste0(country, "_ci_merged.rds")))


# In the table 'educ_facilities_cleaned', the variable operator_t is the one 
# reporting the sector of the school. I am recoding it into broader categories
# I am doing this just to create subtables for filters using Leaflet in the 05_data_visualization
# R script, otherwise only ~20% on the data reports the sector which is too low 
# hence not really relevant in the context of our analysis

educ_facilities_cleaned <- educ_facilities %>%
  mutate(
    sector_group = case_when(
      operator_t %in% c("government", "public", "local_authority") ~ "Public",
      operator_t == "private" ~ "Private",
      operator_t %in% c("religious", "community") ~ "Community / Religious",
      operator_t == "ngo" ~ "NGO",
      is.na(operator_t) ~ "Unknown Sector",
      TRUE ~ "Other"
    )
  )


### 2) Calculating Supply indicators (ratio of schools to population and density)
# ! We do this here so the variable is available for the merge !

# Calculate total population (from the raster) per district using a function

districts_ci$population <- exact_extract(pop_raster, districts_ci, fun = function(values, coverage_fraction) {
  sum(values * coverage_fraction, na.rm = TRUE)
})

# Assign each school to its district using a spatial join

schools_with_district <- st_join(educ_facilities, districts_ci, join = st_within)

# Count number of schools per district
schools_per_district <- schools_with_district %>%
  st_drop_geometry() %>% #by dropping geom, the data becomea normal datasets (no longer a spatial data)
  group_by(NAME_2) %>%  # district var
  summarise(n_schools = n(), .groups = "drop") #.groups = drop <=> ungroup() so that later joins calculations won't mess with the data 

#  Merge number of schools into districts_ci to calculate the final ratios
districts_schools_ci <- districts_ci %>%
  left_join(schools_per_district, by = "NAME_2") %>%
  mutate(
    n_schools = ifelse(is.na(n_schools), 0, n_schools),        
    schools_per_10000 = n_schools / population * 10000,         
    # Need to project to meters for accurate km2 area
    pop_density = population / (as.numeric(st_area(.)) / 1e6)
  )

saveRDS(districts_schools_ci, file.path(path_dhs, "dhs", "02_Presentations", "Visualization",  "Tables",paste0(country, "_districts_schools_ci.rds")))

# -----------------------------------------------------------------------------#


### III) Join DHS and GIS datasets

# This brings the supply (schools) into the demand (individual children)

# 1) Spatial join
# Here the goal is to link DHS points with the District polygons so that each 
# district and 'schools_per_10000' are associated to a GPS point

link_dhs_district <- st_join(districts_dhs_shp, districts_schools_ci, join = st_within)

# 2) Convert to a regular table 
# because here we just need the data (not the map coordinates)

link_dhs_district_table <- link_dhs_district %>%
  st_drop_geometry() %>%
  select(DHSCLUST, NAME_1, NAME_2, schools_per_10000, pop_density)


#3) join to my study sample

# In DHS data hv001 is the cluster ID.
# In the GPS file DHSCLUST is the cluster ID.

study_sample_final <- study_sample_clean %>%
  left_join(link_dhs_district_table, by = c("hv001" = "DHSCLUST"))


# Let's perform a quick check to see if any children failed to match a district

missing_check <- sum(is.na(study_sample_final$schools_per_10000))
print(paste0("Number of children with missing district data: ", missing_check))

# out of 24775 obs, just 27 have missing district data which is negligible and won't bias results
# The possible reasons for this missingness: 
# *** Border clusters : DHS offsets GPS coordinates by a few kilometers for 
# privacy, those 27 children might belong to a cluster that "jumped" outside 
# the national border
# *** Missing GPS coordinate

# Saving the clean final dataset
write.csv(study_sample_final, file.path(path_dhs, "dhs", country, "02_Clean", paste0(country, "_study_sample_final.csv")))
saveRDS(study_sample_final, file.path(path_dhs, "dhs", country, "02_Clean", paste0(country, "_study_sample_final.rds")))


# ---------------------#

message("GIS Data Cleaning script completed")