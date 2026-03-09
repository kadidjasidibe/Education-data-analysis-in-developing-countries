# ------------------------------------------------------
# 01_Import.R - DHS Côte d'Ivoire - 2021
# ------------------------------------------------------

# This script allow us to import all the datasets we will need, select the vars of study
# and save them for later use.

# Note for users: 
# Raw DHS data is not included in this repository due to data privacy. 
# To run this, please place the Cote d'Ivoire 2021 DTA files 
# in the 'dhs/Cote_d_Ivoire/01_Raw/' folder.

### --------- Loading the raw datasets
# Here I directly use the function "clean_names()" to avoid dealing with capital letters later

# Household members
pr <- read_dta(file.path(path_dhs, "dhs", country, "01_Raw", "CIPR81FL.DTA")) %>% clean_names()

# Household
hr <- read_dta(file.path(path_dhs, "dhs", country, "01_Raw", "CIHR81FL.DTA")) %>% clean_names()

# Individual Women
ir <- read_dta(file.path(path_dhs, "dhs", country, "01_Raw", "CIIR81FL.DTA")) %>% clean_names()

# Children under 5
kr <- read_dta(file.path(path_dhs, "dhs", country, "01_Raw", "CIKR81FL.DTA")) %>% clean_names()


### --------- Selecting the relevant variables regarding my research question

# Household members (PR) - info in child specific vars
pr_sel <- pr %>% select(hv001, hv002, hv005, hv007, hvidx, hv104, hv105, hv106, hv107, hv108, hv109, hv140, hv121, hv237)

# Note: I am keeping these variables for my main analsysis but also to investigate the link between 
# birth registration (hv140) and school attendance.I also want explore structural barriers like child labor (hv237).



# Household (HR) - household variables -> related to socio- economic context (ex: hh size, wealth index, access to improved water/sanitation etc.)

hr_sel <- hr %>% select(hv001, hv002, hv021, hv022, hv024, hv025, hv009, hv270, 
                        hv201, hv204, hv205, hv206, hv219, hv220)



# Women (IR) - to investigate the link between maternal education and empowerment
ir_sel <- ir %>% select(caseid, v001, v002, v003, v106, v133, v155, v714, 
                        v743a, v743b, v743c, v743d, v743e, v743f)



# Children under 5 (KR) -  nutrition and health (vaccinations) variables (hw and h avriables)
kr_sel <- kr %>% select(v001, v002, bidx, b4, b8, b5, b7, 
                        hw70, hw71, hw72, h2, h3, h4, h5, h9, h11, h22)




## Merging the datasets

# I merge PR and HR first to get the base "Child + Home" dataset. It also avoids mistakes in matching mothers and children
# This covers my main focus: school attendance and age-for-grade.
#  hv001/hv002 are the unique household identifier.

pr_hr <- left_join(pr_sel, hr_sel, by = c("hv001", "hv002"))

# Check: Did the merge work? 
# If the number of rows in pr_hr is the same as pr, we are good.
if(nrow(pr_hr) != nrow(pr_sel)) {
  warning("Merge alert: Row count changed during PR-HR join!")
}

## !! I will later on merge mothers (IR) and child (KR) depending on IDs !!




###### Saving the merged pr_hr dataset


write_dta(pr_hr,file.path(path_dhs, "dhs", country, "01_Raw", 
                          paste0("pr_hr_", country, "_2021_raw.dta")))

message(paste("child and HH datasets successfully merged and saved for", country))


## Import GIS Datasets 

# Import DHS shape file
districts_dhs_shp <- st_read(file.path(path_dhs, "dhs", country, "01_Raw", "GIS", "CIGE81FL.shp"))

# GADM administrative boundaries (level 1 and 2)

civ_regions <- st_read(file.path(path_dhs, "dhs", country, "01_Raw", "GIS", "gadm41_CIV_1.shp"))
districts_ci <- st_read(file.path(path_dhs, "dhs", country, "01_Raw", "GIS", "district", "gadm41_CIV_2.shp"))


# EDUC FACILITIES shape file (OpenStreetMap)
educ_facilities <- st_read(file.path(path_dhs, "dhs", country, "01_Raw", "GIS", "educ_facilities", "hotosm_civ_education_facilities_points_shp.shp"))

# Population raster
pop_raster <- rast(file.path(path_dhs, "dhs", country,"01_Raw","GIS","raster", "civ_pop_2024_100m.tif"))

message(paste("GIS shapefiles successfully imported for", country))

message("Import script executed")