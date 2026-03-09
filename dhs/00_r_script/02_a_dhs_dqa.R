# ------------------------------------------------------
# ---------------- DATA QUALITY ASSESSMENT -------------
# ------------------------------------------------------

# This script will be about performing data quality assessment for DHS dataset.
# The plan is as follow:


### 1) DUPLICATES CHECKS
### 2) OUTLIERS CHECKS
### 3) RANGE CONSTRAINTS CHECKS
### 4) DATA TYPES CHECKS (AND CONVERSIONS!! SO THAT THE OTHER CHECKS CAN BE PERFORMED SAFELY )
### 5) MISSINGNESS CHECKS
### 6) COMPLETENESS CHECKS
### 7) INCONSISTENCY CHECKS IN CHARACTER AND CATEGORICAL variableS (not necessary for DHS)

# Indeed, as an analyst, these checks are import to assess the data quality before running any models

# ------------------------------------------------------

## Define path and load the dataset 
## personal note: necessary for later subdivision of my script into master, import, dqa, regressions etc ...



pr_hr_raw <- haven::read_dta(file.path(path_dhs, "dhs", country, "01_Raw", paste0("pr_hr_", country, "_2021_raw.dta")))

# to not overwrite on raw data
pr_hr <- pr_hr_raw

#------------------------------------------------------------------------------#
#                    DATA EXPLORATION AND SAMPLE SELECTION 
#------------------------------------------------------------------------------#


# ---------------
### STORING LABELS FOR LATER USE 
# ---------------

# Before I change the data types, I store the variable labels. Indeed, when converting
# to factors, the labels are lost, so I save them now for my ggplot visuals later.

# Variables of interest
selected_vars_viz <- c(
  "hv001", "hv002", "hv005", "hv007", "hvidx", # IDs/Weights
  "hv104","hv105","hv106","hv107","hv108","hv109", "hv121", "hv140", # Member vars
  "hv021", "hv022", "hv024","hv025", "hv009","hv270", # Survey/HH vars
  "hv201","hv204", "hv205","hv206","hv219","hv220", "hv237", # WASH/Health/Other
  "education_level"
)


# Extract and clean labels
var_labels_viz <- sapply(selected_vars_viz, function(v) {
  lbl <- attr(pr_hr[[v]], "label")
  
  
  # shortening long DHS strings for visuals
  lbl <- str_replace_all(lbl, c(
    "household" = "hh",
    "educational" = "educ",
    "education" = "educ",
    "type of place of residence" = "type of place",
    "anything done to water to make safe to drink" = "tentative safe water"
  ))
  return(lbl)
})

# Final list of labels for my DQA tables and plots
var_labels_viz_df <- tibble(
  variable = names(var_labels_viz),
  label = unname(var_labels_viz)
)


# Region labels (hv024) (to make sure we get the names of the regions instead of the numbers)
region_labels <- tibble(
  code = as.numeric(pr_hr$hv024),
  name = names(attr(pr_hr$hv024, "labels"))[match(as.numeric(pr_hr$hv024), attr(pr_hr$hv024, "labels"))]
)

# Type of place labels (hv025) (to make sure we get urban rural instead of the number)
place_labels <- tibble(
  code = as.numeric(pr_hr$hv025),
  name = names(attr(pr_hr$hv025, "labels"))[match(as.numeric(pr_hr$hv025), attr(pr_hr$hv025, "labels"))]
)



# ------------------------ #

### 1) DUPLICATES CHECKS

# ------------------------ #

# Iuse the janitor function "get_dupes" to automatically flag duplicates and count occurrences


duplicates <- pr_hr %>%
  janitor::get_dupes(hv001, hv002, hvidx) 

#Get total count for the percentage calculation
total_obs <- nrow(pr_hr)

# Summarize results in a clean table
if (nrow(duplicates) > 0) {
  duplicates_summary <- duplicates %>%
    group_by(hv001, hv002, hvidx) %>%
    # dupe_count is created automatically by janitor
    summarise(instances = n(), 
              perc_of_total_data = round((instances / total_obs) * 100, 4),
              .groups = "drop") %>%
    arrange(desc(instances))
} else {
  # Create an empty dataframe with a message if no duplicates exist
  duplicates_summary <- tibble(Status = "No duplicates found", N = 0)
}


# Save duplicates table
write_csv(duplicates_summary, file.path(path_dhs, "dhs", "01_Output","DQA","Tables", paste0(country, "_duplicates.csv")))


# ---------------------#

### 2) OUTLIERS CHECKS (IQR method)

#------------------------ #

# Define variables to exclude from outlier checks.
# These are: IDs, and any variable whose primary role is joining.
vars_to_exclude <- c(
  "hv001", "hv002", "hv005", "hv007", "hv021", "hv022", "hvidx" # Survey IDs/Weights
)



# Define the list of numeric variables to analyze
numeric_vars <- pr_hr %>%
  select(where(is.numeric)) %>%
  select(-any_of(vars_to_exclude)) %>%
  names()

# Calculate Outliers and Summary Stats
outliers_summary <- pr_hr %>%
  select(all_of(numeric_vars)) %>%
  summarise(across(everything(), list(
    min    = ~min(., na.rm = TRUE),
    q1     = ~quantile(., 0.25, na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    mean   = ~mean(., na.rm = TRUE),
    q3     = ~quantile(., 0.75, na.rm = TRUE),
    max    = ~max(., na.rm = TRUE),
    sd     = ~sd(., na.rm = TRUE)
  ), .names = "{.col}--{.fn}")) %>%
  pivot_longer(everything(), names_to = "temp_name", values_to = "value") %>%
  separate(temp_name, into = c("variable", "stat"), sep = "--") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(
    iqr = q3 - q1,
    lower_bound = q1 - 1.5 * iqr,
    upper_bound = q3 + 1.5 * iqr,
    outliers_detected = ifelse(min < lower_bound | max > upper_bound, "Yes", "No")
  )

# Identify outlier records with IDs
outliers_vars <- outliers_summary %>% pull(variable)

outliers_records <- pr_hr %>%
  select(hv001, hv002, hvidx, all_of(outliers_vars)) %>%
  pivot_longer(
    cols = all_of(outliers_vars), 
    names_to = "variable", 
    values_to = "value"
  ) %>%
  left_join(outliers_summary, by = "variable") %>%
  mutate(
    # We apply the 0-floor to ANY variable that isn't allowed to be negative in DHS
    # This includes Age, Counts, and categorical codes (which are always greater than 0)
    can_be_negative = FALSE, 
    
    # Making sure the bound is at least 0
    analytic_lower_bound = ifelse(!can_be_negative & lower_bound < 0, 0, lower_bound),
    
    is_outlier = case_when(
      value > upper_bound ~ TRUE,
      value < analytic_lower_bound ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  filter(is_outlier == TRUE) %>%
  select(hv001, hv002, hvidx, variable, value, lower_bound, analytic_lower_bound, upper_bound)


# Save results
write_csv(outliers_summary, file.path(path_dhs, "dhs",  "01_Output", "DQA", "Tables", paste0(country, "_outlier_summary.csv")))
write_csv(outliers_records, file.path(path_dhs, "dhs",  "01_Output","DQA","Tables", paste0(country, "_outliers_flagged.csv")))


# ---------------------#

### 03) RANGE CONSTRAINT CHECKS

# ------------------------ #

# Function to standardize range checks using constraints
perform_standardized_range_check <- function(data, constraints_list, special_codes_list, check_type = "numeric") {
  
  vars_to_check <- names(constraints_list)
  
  # Here, I am using purrr::map_df for a cleaner loop and binding
  map_df(vars_to_check, function(var) {
    
    # CLEANING STEP 
    # Retrieve the variable from the dataset
    var_data <- data[[var]]
    
    # First, handle the special codes (for CI, if the variable has them defined)
    if (var %in% names(special_codes_list)) {
      var_data[var_data %in% special_codes_list[[var]]] <- NA
    }
    
    # Filter out NAs before checking the range
    data_clean <- tibble(value = var_data) %>%
      filter(!is.na(value))
    
    if (nrow(data_clean) == 0) return(NULL) 
    
    # CHECKING THE TYPES
    if (check_type == "numeric") {
      limits <- constraints_list[[var]]
      
      # Identify violations
      violations <- data_clean %>%
        filter(value < limits[1] | value > limits[2]) %>%
        pull(value)
      
      if (length(violations) > 0) {
        return(tibble(
          variable = var,
          violated_value = unique(violations),
          constraint_details = paste0("[", limits[1], ", ", limits[2], "]"),
          constraint_type = "Numeric Range"
        ))
      }
      
    } else if (check_type == "categorical") {
      allowed <- constraints_list[[var]]
      
      # Convert factor/labelled value to numeric code for comparison
      violations <- data_clean %>%
        filter(!as.numeric(value) %in% allowed) %>%
        pull(value)
      
      if (length(violations) > 0) {
        return(tibble(
          variable = var,
          violated_value = unique(violations),
          constraint_details = paste(allowed, collapse = ", "),
          constraint_type = "Categorical Levels"
        ))
      }
    }
    return(NULL)
  })
}


# Define plausible ranges to compute the range constraint checks based on DHS standards

num_range_constraints <- list(
  #Identifiers (numeric/integer)
  hv001 = c(1, Inf),    # Cluster number (PSU)
  hv002 = c(1, Inf),    # Hh number within cluster
  hv021 = c(1, Inf),    # PSU ID
  hv022 = c(1, Inf),    # Sample strata
  hvidx = c(1, 99),     # Hh member line number
  
  # hh structure (factors)
  hv105 = c(0, 120),    # Age of hh member (years)
  hv220 = c(0, 120),    # Age of hh head (years)
  hv009 = c(1, 60),     # Hh size
  
  # Educ indic (ordinal / numeric)
  hv107 = c(0, 30),     # Years of educ completed
  hv108 = c(0, 30),          # Education completed in single years
  hv109 = c(0, 30)     # Years of educ completed by hh head
)
# Define allowed values for categorical variables based on factor levels

categorical_constraints <- list(
  hv106 = c(0, 1, 2, 3, 4, 5, 8, 9),      # Highest educ level attained -> 0:None, 1:Primary , 2:Secondary, 3:Higher, 4: Vocational/Technical, 5: , 8:Don't Know, 9:Missing
  hv140 = c(0, 1,2,3,7,8,9),           # Birth certificate status -> 0:No, 1:Seen, 2:Not seen, 7:Other,  8:Don't Know, 9:Missing
  hv237 = c(0, 1, 8, 9),           # Child labor indicator -> 0:No, 1:Yes, 8:Don't Know, 9:Missing
  hv201 = c(11:14, 21, 31, 32, 41:43, 51, 61, 62, 71, 72, 96, 97, 98, 99), # Source of drinking water
  hv205 = c(11:15, 21:23, 31, 41:43, 96, 97, 98, 99),  # Type of toilet facility 
  hv206 = c(0, 1, 9),           # Electricity availability ("no","yes", "missing")
  hv104 = 1:2,           # Sex (male/female)
  hv025 = 1:2,           # Type of residence (urban/rural)
  hv219 = 1:2,           # Sex of household head (male/female)
  hv024 = 1:50           # Region code
)
# Known DHS special codes for missing/N/A 
special_codes <- list(
  hv105 = c(98, 99),
  hv220 = c(98, 99),
  hv107 = c(98, 99),
  hv108 = c(98, 99),
  hv109 = c(98, 99),
  hv106 = c(8, 9),
  hv140 = c(8, 9),
  hv237 = c(8, 9),
  hv201 = c(96, 97, 98, 99),
  hv205 = c(96, 97, 98, 99),
  hv206 = c(9)
)


# Check range issues for NUMERIC variables using the new function
range_issues_numeric <- perform_standardized_range_check(
  data = pr_hr, # Use the converted data for consistency
  constraints_list = num_range_constraints,
  special_codes_list = special_codes,
  check_type = "numeric"
)


# Check range issues for CATEGORICAL variables using the new function
range_issues_categorical <- perform_standardized_range_check(
  data = pr_hr,
  constraints_list = categorical_constraints,
  special_codes_list = special_codes,
  check_type = "categorical"
)

# combining ranges issues for num and cat vars
range_issues <- bind_rows(
  range_issues_numeric,
  range_issues_categorical
)

range_issues

# Save results
write_csv(range_issues, file.path(path_dhs, "dhs",  "01_Output", "DQA", "Tables", paste0(country, "_range_issues.csv")))                  # ---------------------#

# Note on range check in CI :
# The code flagged values 0 for hv140 and 14 for hv201 as issues. But I checked 
# the labels in the data and they are actually correct. For this survey, 0 means 
# 'no certificate or registered' and 14 is 'public tap'. So I kept them because 
# they are valid answers for Côte d'Ivoire

# ------------------------ #

### 4) DATA TYPES CHECKS & CONVERSION

# ------------------------ #

## Creating the structure of the variable table

var_table <- data.frame(
  variable = names(pr_hr),
  current_type = sapply(pr_hr, function(x) class(x)[1]),
  key_for_DQA = NA,
  expected_type = NA,
  Purpose = NA,
  Notes = NA,
  convert_to_factor = NA,
  stringsAsFactors = FALSE
)

## Fill key_for_DQA and expected_type and Notes
var_table <- var_table %>%
  mutate(
    key_for_DQA = case_when(
      variable %in% c("hv001","hv002","hvidx","hv104","hv105","hv106","hv107","hv108",
                      "hv021","hv022","hv024","hv025","hv270") ~ "Yes",
      TRUE ~ "Secondary Analysis / Robustness Check"
    ), 
    variable_name = sapply(variable, function(x) {
      lbl <- attr(pr_hr[[x]], "label")
      if (is.null(lbl)) lbl <- "Label not defined"
      lbl
    }),
    
    expected_type = case_when(
      variable %in% c("hv001","hv002","hv021","hv022") ~ "numeric", #identifiers
      variable %in% c("hv104","hv025","hv140","hv219","hv237") ~ "factor",  #cat vars
      variable %in% c("hv005","hvidx","hv105","hv107","hv109","hv009","hv220") ~ "numeric",
      variable %in% "" ~ "integer",
      variable %in% c("hv106","hv108","hv270","hv201","hv205","hv206","hv024") ~ "factor", # ordinal car vars
      TRUE ~ "Check manually"
    ),
    
    Notes = case_when(
      variable == "hv001" ~ "Cluster: needed for calculating correct standard errors",
      variable == "hv002" ~ "HH number: to merge with other DHS datasets (IR/KR)",
      variable == "hv005" ~ "Weights: to make data representative of the whole country",
      variable == "hvidx" ~ "ID for HH members: used for merging data",
      variable == "hv104" ~ "Sex: Used for gender-gap analysis and to check interaction effects with HH wealth",
      variable == "hv105" ~ "Age: used to filter the sample up to primary school age only",
      variable == "hv106" ~ "Highest education level: to check if children are in the right grade for their age",
      variable == "hv107" ~ "Years in school: to see if they repeated any years / check their schooling history",
      variable == "hv108" ~ "Current grade: to identify those actually in school right now",
      variable == "hv109" ~ "Mother educ: Proxy for impact of mother education on child schooling",
      variable == "hv140" ~ "Birth certificate: checking if lack of papers blocks enrollment",
      variable == "hv121" ~ "Main variable: current school attendance to calculate enrollment rates",
      variable == "hv237" ~ "Dirty water: might cause sickness and keep kids home",
      variable == "hv021" ~ "PSU: required for the survey design setup",
      variable == "hv022" ~ "Strata: required for the survey design setup",
      variable == "hv024" ~ "Region: to compare differences across regions",
      variable == "hv025" ~ "Urban or Rural: check location gap",
      variable == "hv009" ~ "HH size: used to see if having many kids reduce school money?",
      variable == "hv270" ~ "Wealth level: main control for household budget",
      variable == "hv201" ~ "Source of water: proxy for HH living standards",
      variable == "hv205" ~ "Toilet: proxy for HH living standards",
      variable == "hv206" ~ "Electricity: proxy for HH living standards and study environment",
      variable == "hv219" ~ "Sex of HH head: check if woman as head helps education",
      variable == "hv220" ~ "Age of HH head: control variable",
      variable == "hv204" ~ "Distance to water: more time fetching water means less school time",
      variable == "education_level" ~ "Recoded version of hv106 for the final model",
      TRUE ~ NA_character_
    )
  ) %>%
  select(variable, variable_name, current_type, expected_type, key_for_DQA, Notes, convert_to_factor)   # Reorder columns


## Detect and mark haven-labelled variables
var_table$convert_to_factor <- sapply(names(pr_hr), function(x) {
  if (is.labelled(pr_hr[[x]])) {
    "Haven-labelled → convert to factor"
  } else if (is.factor(pr_hr[[x]])) {
    "Already factor"
  } else {
    "No conversion needed"
  }
})

## Check type mismatches (storage vs expected concept)
var_table <- var_table %>%
  mutate(
    type_mismatch = case_when(
      expected_type %in% "factor" & !(current_type %in% c("factor", "character")) ~ "⚠ Convert to factor",
      expected_type %in% c("numeric", "integer") & !(current_type %in% c("numeric", "integer")) ~ "⚠ Check numeric",
      TRUE ~ ""
    )
  )



## function to store vars that are not in the right format to later make the change quickly

change_to_factor <- var_table %>%
  filter(grepl("Convert to factor", type_mismatch)) %>%
  pull(variable)

change_to_numeric <- var_table %>%
  filter(grepl("Check numeric", type_mismatch)) %>%
  pull(variable)


cat("\nvariables to convert to factor:", length(change_to_factor), "\n")
cat(paste(change_to_factor, collapse = ", "), "\n")
cat("\nvariables to check/convert to numeric:", length(change_to_numeric), "\n")
cat(paste(change_to_numeric, collapse = ", "), "\n\n")

# we can see that in the datasets vars are either stored as numeric or haven-labelled
# link with data that have been downloaded in a STATA format



###  convert and store results in a dataset

pr_hr_with_conversion <- pr_hr %>%
  mutate(across(all_of(change_to_factor), ~ as_factor(.x))) %>%
  mutate(across(all_of(change_to_numeric), ~ as.numeric(as.character(.x)))) %>%

#Manual fix for hv121 and hv204 for which the type stayed as "haven_labelled" because the 
# automatic detection missed them.

# I use the function as_factor() to make sure we keep the "Yes/No" labels from DHS

mutate(
  hv121 = as_factor(hv121),
  hv204 = as_factor(hv204))


# quick check if conversion was done correctly 

class(pr_hr_with_conversion$hv121)
class(pr_hr_with_conversion$hv204)

var_table <- var_table %>%
  mutate(
    type_after_conversion = sapply(variable, function(x) class(pr_hr_with_conversion[[x]])[1]),
    # Define OK if types match conceptually (factor ≈ labelled/factor, numeric ≈ numeric/integer)
    consistency = case_when(
      expected_type == "factor" & type_after_conversion == "factor" ~ "OK",
      expected_type %in% c("numeric", "integer") & type_after_conversion %in% c("numeric", "integer") ~ "OK",
      TRUE ~ "Check"
    )
  )
# export and store results in csv

write.csv(var_table, file.path(path_dhs, "dhs",  "01_Output", paste0("pr_hr_", country, "_variable_table_with_data_type.csv")),
          row.names = FALSE)
saveRDS(var_table, file.path(path_dhs, "dhs",  "02_Presentations", "Visualization",  "Tables", paste0(country, "_var_table.rds")))



#Splitting the table into 2 (data quality assessment and research variables)

#DQA table
dqa_table <- var_table %>%
  select(variable, current_type, expected_type, type_mismatch) %>%
  filter(type_mismatch != "OK") %>% # Only show the ones that required work!
  rename(
    "Variable" = variable,
    "Raw Type" = current_type,
    "Expected Type" = expected_type,
    "Cleaning Action" = type_mismatch
  )

saveRDS(dqa_table, file.path(path_dhs, "dhs",  "02_Presentations", "Visualization",  "Tables", paste0(country, "_dqa_table.rds")))

#Research question variables
research_var_table <- var_table %>%
  select(variable, variable_name, Notes) %>%
  rename(Variable = variable, Description = variable_name, `Analytical justification` = Notes)

saveRDS(research_var_table, file.path(path_dhs, "dhs",  "02_Presentations", "Visualization",  "Tables",paste0(country, "_research_var_table.rds")))

                        # ---------------------#



# ------------------------ #

### 5) MISSINGNESS CHECKS

# ------------------------ #

### Diagnostic to assess missingness considering DHS special codes
# Exclude design vars i.e. IDs, strata, weight
design_vars <- c("hv001", "hv002", "hv005", "hv021", "hv022", "hvidx")

# Run the check
special_code_checks <- pr_hr_with_conversion %>%
  
  # Exclude IDs/Weights so we don't get false positives
  select(-all_of(design_vars)) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  
  # Filter on common DHS text labels
  filter(value %in% c("94", "95", "96", "97", "98", "99", 
                      "994", "995", "996", "997", "998", "999",
                      "9994", "9995", "9996", "9997", "9998", "9999") |
           str_detect(tolower(value), "don't know|missing|other|on premises|inconsistent|refused")) %>%
  group_by(variable, value) %>%
  summarise(n_cases = n(), .groups = "drop") %>%
  arrange(desc(n_cases))

print(special_code_checks)



# Define the missingness check function
fun_missingness_checks <- function(data, group_var) {
  
  # variables where 94, 95, 96 are REAL numbers (Age, HH size, etc.)
  age_and_count_vars <- c("hv105", "hv220", "hv009", "hv012", "hv013")
  
  missing_data <- data %>%
    group_by(across(all_of(group_var))) %>%
    summarise(
      across(starts_with("hv"), 
             ~ {
               val_char <- as.character(.)
               # For Age/Counts, only 998 or 999 are missing
               if (cur_column() %in% age_and_count_vars) {
                 sum(is.na(.) | val_char %in% c("998", "999") | 
                       str_detect(tolower(val_char), "don't know|missing"))
               } 
               # For everything else, we include 94, 95, 97 etc.
               else {
                 sum(is.na(.) | val_char %in% c("94", "95", "97", "98", "99", "994", "995", "997", "998", "999") | 
                       str_detect(tolower(val_char), "don't know|missing|inconsistent|refused"))
               }
             }, 
             .names = "n_missing_{.col}"),
      total_obs = n(),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = starts_with("n_missing"),
      names_to = "variable",
      names_prefix = "n_missing_",
      values_to = "n_missing"
    ) %>%
    mutate(missing_percent = round(100 * n_missing / total_obs, 2)) %>%
    filter(n_missing > 0)
  
  return(missing_data)
}


# A) NATIONAL MISSINGNESS (no grouping)


missingness_national <- pr_hr_with_conversion %>%
  mutate(total = "National") %>% # Create a temporary column for easy grouping
  fun_missingness_checks(group_var = "total")

# B. MISSINGNESS BY PLACE OF RESIDENCE (hv025)
# This will show if NAs or 'Don't know' are more common in rural or urban areas

missingness_by_residence <- pr_hr_with_conversion %>%
  fun_missingness_checks(group_var = "hv025")

# Saving the results in a table
write.csv(
  missingness_national,
  file.path(path_dhs, "dhs",  "01_Output", "DQA", "Tables", paste0(country, "_missingness_national.csv")),
  row.names = FALSE
)

write.csv(
  missingness_by_residence,
  file.path(path_dhs, "dhs",  "01_Output", "DQA", "Tables", paste0(country, "_missingness_by_residence.csv")),
  row.names = FALSE
)

# Map variables names and get readable labels
labels_map <- var_labels_viz[names(var_labels_viz) %in% unique(missingness_by_residence$variable)]

missingness_by_residence <- missingness_by_residence %>%
  mutate(
    variable_label = factor(variable, levels = names(labels_map), labels = labels_map),
    variable_label_wrapped = str_wrap(variable_label, width = 20),
    place_name = factor(hv025, labels = names(attr(pr_hr$hv025, "labels")))
  )

write.csv(
  missingness_by_residence,
  file.path(path_dhs, "dhs",  "01_Output", "DQA", "Tables", paste0(country, "_missingness_by_residence.csv")),
  row.names = FALSE
)
saveRDS(missingness_by_residence,
        file.path(path_dhs, "dhs",  "01_Output", "DQA", "Tables", paste0(country, "_missingness_by_residence.rds")))




# C) MISSINGNESS BY REGION (hv024)
missingness_by_region <- pr_hr_with_conversion %>%
  fun_missingness_checks(group_var = "hv024")

# Adding variables names and get readable labels
labels_map_region <- var_labels_viz[names(var_labels_viz) %in% unique(missingness_by_region$variable)]

missingness_by_region <- missingness_by_region %>%
  mutate(
    variable_label = factor(variable, levels = names(labels_map_region), labels = labels_map_region),
    variable_label_wrapped = str_wrap(variable_label, width = 20),
    place_name = factor(hv024, labels = names(attr(pr_hr$hv024, "labels")))
  )
write.csv(
  missingness_by_region,
  file.path(path_dhs, "dhs",  "01_Output", "DQA", "Tables", paste0(country, "_missingness_by_region.csv")),
  row.names = FALSE
)
saveRDS(missingness_by_region,
        file.path(path_dhs, "dhs",  "01_Output", "DQA", "Tables", paste0(country, "_missingness_by_region.rds")))


# ---------------------#

### 06) COMPLETENESS CHECKS

# ------------------------ #

# Create the completeness check function 
fun_completeness_checks <- function(data, group_var = NULL) {
  # Start with the data and calculate total obs
  if (is.null(group_var)) {
    data_grouped <- data %>% mutate(group_col = "Total") %>% group_by(group_col)
    group_var <- "group_col"
  } else {
    data_grouped <- data %>% group_by(across(all_of(group_var)))
  }
  
  # Calculate completeness which is defined as the percentage of non-missing values
  completeness_data <- data_grouped %>%
    summarise(
      across(
        where(~ !is.character(.)), # Check only non-character vars for NAs
        ~ 100 * mean(!is.na(.)),
        .names = "completeness_{.col}"
      ),
      .groups = "drop"
    ) %>%
    # Convert the dataset from wode to long format for easy analysis/plotting
    pivot_longer(
      cols = starts_with("completeness_"),
      names_to = "variable",
      names_prefix = "completeness_",
      values_to = "completeness_percent"
    )
  
  return(completeness_data)
}

#A) NATIONAL COMPLETENESS (no grouping)

completeness_national <- pr_hr_with_conversion %>%
  fun_completeness_checks() %>%
  arrange(completeness_percent) %>% select(-group_col)

# Save completeness per variable
write_csv(completeness_national, file.path(path_dhs, "dhs",  "01_Output","DQA","Tables", paste0(country, "_completeness_national.csv")))



#B) Completeness by variable and region 

completeness_by_region <- pr_hr_with_conversion %>%
  fun_completeness_checks(group_var = "hv024") %>%
  #creating the numeric version of hv024 for the join as currently stored as a factor
  mutate(hv024_num = as.numeric(hv024)) %>%
  left_join(var_labels_viz_df, by = "variable") %>%
  mutate(
    label = sapply(label, function(l) ifelse(is.list(l) || is.null(l), as.character(l), l)),
    variable_label_wrapped = str_wrap(label, width = 20)
  ) %>%
  left_join(
    region_labels %>% distinct(code, .keep_all = TRUE) %>% rename(region_name = name),
    # Join on the new numeric column (hv024_code)
    by = c("hv024_num" = "code")
  )

saveRDS(completeness_by_region,
        file.path(path_dhs, "dhs",  "02_Presentations", "Visualization", "Tables", paste0(country, "_completeness_by_region.rds")))
            
                          
                        # ---------------------#


message("DHS Data Quality Assessment script completed")