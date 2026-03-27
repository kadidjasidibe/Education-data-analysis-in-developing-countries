# ------------------------------------------------------
# ---------------- DATA JOINING --------------
# ------------------------------------------------------

# This script will be about joining the DHS and GIS datasets. 
# This brings the supply (schools) into the demand (individual children)

# -----------------------------------------------------------------------------#


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

# To avoid issues when defining the desgin in the analysis script, I will delete 
# the missing observations now

study_sample_final <- study_sample_final %>%
  filter(!is.na(NAME_2))

# Saving the clean final dataset
write.csv(study_sample_final, file.path(path_dhs, "dhs", country, "02_Clean", paste0(country, "_study_sample_final.csv")))
saveRDS(study_sample_final, file.path(path_dhs, "dhs", country, "02_Clean", paste0(country, "_study_sample_final.rds")))


# ---------------------#

message("Spatial merge completed. Master dataset ready for analysis")