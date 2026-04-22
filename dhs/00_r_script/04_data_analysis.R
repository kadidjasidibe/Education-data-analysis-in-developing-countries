
# ------------------------------------------------------
# ---------------- DATA ANALYSIS --------------
# ------------------------------------------------------

# This will be about analyzing the cleaning data set. It implies in our case: 

### 1) DESCRIPTIVE STATISTICS 
### 2) CORRELATION MATRIX
### 3) SPATIAL HETEREGOGENEITY CHECK
### 3) REGRESSION ANALYSIS : determinants of schooling delay (age-gap)
### 4) Robustness and heterogeneity checks (interaction effects)
### 5) TREND ANALYSIS (when I will include a year variable)

# --------------------------------

### LOAD THE DATA

study_sample_final <- read_csv(file.path(path_dhs, "dhs", country, "02_Clean", paste0(country, "_study_sample_final.csv")))


### Define the analysis variables

# I realized that I have many proxies for wealth which can cause multicolinearity
# Hence, I have removed hv201 (source of drinking water), and hv205 (type of toilet)
# from the list of my variables
# ! I also removed hv121 (current attendance) ! -> not sure about the numbers reported
# ! will check again later! I already have age_gap (outcome var) and hv108 for 
# educ completed in single years

analysis_vars <- c(
  "age_gap",   # My main outcome
  "female",    # Gender gap
  "urban",     
  "poor_hh",   # HH wealth
  "hv140",     # Birth Certificate ("fairer start" blocked by admisnitrative papers?)
  "hv219",     # Sex of HH Head
  "hv009",     # Household Size 
  "hv204",     # Time to water (Proxy for child labor)
  "hv206",     # Electricity ( <=> Study environment)
  "hv109",     # Educ attainment (more precisely mother educ)
  "hv108",     # current grade
 # "hv121",     # current attendance 
 # "hv201",
 # "hv205",
  "hv237",     # water safety
  "hv220",      # age of the HH head
 "schools_per_10000", 
 "pop_density"   
)

### making sure all variables are stored as numeric whether or not they were already
# in the correct format thanks to an automaion loop.
# Goal: make the variables ready for analyses


for (v in analysis_vars) {
  # We focus on the variables that we know are categories (text/factors)
  if (v %in% c("hv140", "hv206", "hv219", "female", "urban", "poor_hh", "hv121", "hv237")) {
    
    study_sample_final[[v]] <- case_when(
      study_sample_final[[v]] %in% c("yes", "female", "urban", "has certificate", "registered", 1, "1") ~ 1,
      study_sample_final[[v]] %in% c("no", "male", "rural", "no certificate", "not registered", 0, "0") ~ 0,
      TRUE ~ 0 # This ensures "Don't know" or "Missing" doesn't break the math
    )
    
  } else {
    # Ensuring numeric variables are stored as numeric
    study_sample_final[[v]] <- as.numeric(as.character(study_sample_final[[v]]))
  }
}



### Define the survey design to correctly handle weight, stratat and cluster effects



study_sample_design <- svydesign(
  data = study_sample_final,
  ids = ~hv021,        # Cluster ID ((=PSU)
  weights = ~weight,   # Normalized survey weight
  strata = ~hv022,     # Stratification variable (can vary by DHS dataset, ALWYAS DOUBLE CHECK!!)
  nest = TRUE          # Important when using stratified multi-stage sampling
)


# ---------------------#

### 1) DESCRIPTIVE STATISTICS

# ---------------------#

# Descriptive statistics are important to assess data structure and interpret patterns


## A) NATIONAL LEVEL

# Compute weighted means median and SD for my analysis vars as medians/ min/ max 
# don't change much given sample selection

# National means
national_means <- svymean(as.formula(paste0("~", paste(analysis_vars, collapse="+"))), 
                          study_sample_design, na.rm = TRUE)

national_means_table <- as.data.frame(t(as.matrix(national_means))) %>%
  mutate(hv024 = "National") %>%
  select(hv024, everything())

# National medians

# defining the list of var fir which the median calculation is relevant (only 
# relevant for continuous vars not dummies)

cont_vars <- c("hv009", "hv204", "hv220", "schools_per_10000", "pop_density")

national_medians <- svyquantile(as.formula(paste0("~", paste(cont_vars, collapse="+"))), 
                                design = study_sample_design, 
                                quantiles = 0.5, na.rm = TRUE)
national_desc_stats_table <- data.frame(
  Variable = names(national_means),
  Mean = as.numeric(national_means),
  SE = SE(national_means)
)

# Adding the medians only for numeric variables

national_desc_stats_table$Median <- ifelse(national_desc_stats_table$Variable %in% cont_vars, 
                            as.numeric(unlist(national_medians)), 
                            NA)

saveRDS(national_desc_stats_table, file.path(path_dhs, "dhs", "01_Output", "Analysis", "national_stats_master.rds"))


## B) REGIONAL LEVEL

# regional mean
regional_means <- svyby(
  as.formula(paste0("~", paste(analysis_vars, collapse="+"))), 
  by = ~hv024, 
  design = study_sample_design, 
  FUN = svymean, 
  na.rm = TRUE
)

regional_means_clean <- as.data.frame(regional_means)
colnames(regional_means_clean)[-1] <- paste0(colnames(regional_means_clean)[-1], "_mean")

# regional median

regional_medians <- svyby(
  as.formula(paste0("~", paste(cont_vars, collapse="+"))), 
  by = ~hv024, 
  design = study_sample_design, 
  FUN = svyquantile, 
  quantiles = 0.5, 
  keep.var = FALSE, # keeps the table cleaner
  na.rm = TRUE
)


regions_list <- as.character(regional_medians[[1]])
regional_medians_clean <- data.frame(hv024 = regions_list, stringsAsFactors = FALSE)

# We fill the columns one by one. Goal : avoid "median 1, median2 etc... issue) 
for (i in 1:length(cont_vars)) {
  var_name <- cont_vars[i]
  # take column i+1, and tranform it in a simple vector (unlist)
  # then we change the type to numeric
  # goal: delete all structure of survey residuals
  values <- as.numeric(unlist(regional_medians[, i + 1]))
  
  regional_medians_clean[[paste0(var_name, "_median")]] <- values
}


# join regional mean and median

regional_desc_stats_table <- regional_means_clean %>%
  left_join(regional_medians_clean, by = "hv024", suffix = c("_mean", "_median")) %>%
  # Pivot longer for all columns except regions
  pivot_longer(
    cols = -hv024, 
    names_to = "stat_type", 
    values_to = "Value"
  ) %>%
  #cleaning the name by idetifying the suffix
  mutate(
    Stat = case_when(
      grepl("_mean$", stat_type) ~ "Mean",
      grepl("_median$", stat_type) ~ "Median",
      TRUE ~ "Delete" # otherwise the table will be too messy, we just keep our descriptive stats of interest
    ),
    # we delete suffix to just keep the exact descritive stat type (mean or median)
    Variable = gsub("_mean$|_median$|se\\.", "", stat_type)
  ) %>%
  filter(Stat != "Delete") %>%
  group_by(hv024, Variable, Stat) %>%
  summarise(Value = first(Value), .groups = "drop") %>% 
  pivot_wider(
    names_from = Stat, 
    values_from = Value
  ) %>%
  arrange(hv024, Variable)


# Save for the visualization
saveRDS(regional_desc_stats_table, file.path(path_dhs, "dhs", "01_Output", "Analysis", "regional_stats_master.rds"))



# -------- DESCRIPTIVE STATS KEY TAKEAWAYS

# Abidjan (the better of) -> 
# *** almost entirely urban (92.5%), lowest poverty rate (11.6%)
# *** highest ccess to electricity  (88%) 

# Zanzan -> urban: only 7.7% and poverty rate 84.1%.

# Savanes, Woroba: lowest access to electricity -> 40%

# # Denguélé has the highest HH size (9.7) and the highest age_gap (0.43). 
# This aligns with the theory that larger households might struggle more with education costs.

# Abidjan has the lowest age_gap (0.24) and the highest education (hv108 = 2.4 years <=> in primary school).
# Highest education is overall very low

# Highest school density in Abidjan (1.62) and Vallée du Bandama (1;09)

# data distribution: 
# *** skewed distrib for pop_desnity (mean and median are far apart for some regions such as Vallée du Bandama) meaning that some region might impact mroe the model / results
# *** binary variables -> median not calculated intentionally as for these types of var we only care about the mean (percentage)

# --------



# ---------------------#

### 2) CORRELATION MATRIX

# ---------------------#

# We use my list of ~ 10 variables to compute the weighted correlation matrix 

correlation_vars <- study_sample_final %>%
  select(all_of(analysis_vars))


correlation_matrix <- wtd.cors(correlation_vars, weight = study_sample_final$weight)

# Cleaning to make number more readable

correlation_matrix_rounded <- round(correlation_matrix, 2)

# Convert the matrix in a data frame to save it in CVS/ RDS format
correlation_matrix_table <- as.data.frame(correlation_matrix_rounded)

correlation_matrix_table <- cbind(Variable = rownames(correlation_matrix_table), correlation_matrix_table)

write_csv(correlation_matrix_table, file.path(path_dhs, "dhs", "01_Output", "Analysis", paste0(country, "_correlation_matrix.csv")))

saveRDS(correlation_matrix_table, file.path(path_dhs, "dhs", "01_Output", "Analysis",paste0(country, "_correlation_matrix.csv")))

# ---------------------#
## CORRELATION COMMENTS

# age_gap & hv140 (Birth Cert) correlation is -0.37 -> highest in the whole matrix (for the relation between the depedent and explanatory var). 
# Suggests that missing papers is a much bigger "hard" barrier than I thought. 
# It's even stronger than the poverty correlation (0.16).

# age_gap correlation with poor_hh (0.16) -> seems like in Côte d'Ivoire, having 
# your papers might matter more for starting on time than how much money the 
# family has.

# curious: 'female' has 0.00 correlation with age_gap.
# For my sample (Preschool/ primary school) -> being a girl doesn't seem to be 
# a barrier for enrollment. 
# !check later if this changes in the secondary school sample.

# age_gap and hv204 (Water time) is basically zero. 
# Seems like the time spent on chores isn't what's delaying kids

# Other comments / potential issues

# **** poor_hh and hv206 (Electricity) have a -0.68 correlation. 
# It makes sense (poor people don't have power), but it's very high. 
# If I use both in the regression, the model might get unstable due to multicollinearity. 
# Might need to pick one as the main wealth proxy.

# **** poor_hh and urban are at -0.61. 
# Strong link between being rural and being in the bottom wealth quintiles. 
# Again, need to be careful with these two together in the model.

# correlation between hv121 (current year attendance) and hv140 -> - 0.47 
# => kids without papers are getting blocked from the current school year.
# Supports the "administrative barrier" argument
# ---------------------#


### 3) SPATIAL HETEREGOGENEITY CHECK

# Check the variability of schools within districts to show that homogeneity between
# region can hide heterogeneity between districts within a region 
school_variability_sum <- study_sample_final %>%
  group_by(hv024) %>% # Region
  summarise(
    min_district_supply = min(schools_per_10000, na.rm = TRUE),
    max_district_supply = max(schools_per_10000, na.rm = TRUE),
    n_districts = n_distinct(NAME_2)
  )

print(school_variability_sum)

# ------------- CORRELATION MATRIX - KEYTAKEAWAYS

# Regions like Vallee du Bandama show massive internal gaps 
# (0.10 vs 2.05 schools/10k) which confirms that regional means is misleading 
# for some regions (aggreation bias)

# Woroba has districts with zero schools

# Policy Implication: because supply varies a lot within regions,  I am going to 
# add the district-level variable (NAME_2) in our regression to capture the true
# effect of infrastructure on child outcomes.

#------- notes on potential multicolinearity issues

# Correlation between Education Variables (hv109 and hv108) => 0.89, too high
# -> decided to keep hv109 (mother education) -> best predictor of LT hh human 
# capital +  age_gap already contain some ifno on chidlren grade

# Correlation between wealth and urban status also high -> 
# *** poor_hh & hv206 (Electricity): -0.68
# *** poor_hh & urban: -0.61
# keeping just poor_hh and urban for the regression (<=> electricity = perfect 
# proxy for wealth)


# Correlation between the GIS var (schools_per_10000 & pop_density) is high -> 0.69
# I will just keep schools_per_10k

# ------------

# ---------------------#

### 4) REGRESSION ANALYSIS : determinants of schooling delay (age-gap)

# ----------------------------------------------------------------------- #
# STRATEGY: I follow a two-step approach. 
# A) Test for structural barriers (Birth Registration).
# B) Estimate socio-economic drivers using a Survey-weighted logistic regression.
# C) Check if adding spatial dimension  (school and pop density) impact the coefficients 
# ----------------------------------------------------------------------- #

# ---------------------#

# First let's do a professional check : Why do we have lonely PSUs?
# We check how many districts (NAME_2) exist per stratum (hv022)
check_strata <- study_sample_final %>%
  group_by(hv022) %>%
  summarise(n_districts = n_distinct(NAME_2)) %>%
  filter(n_districts == 1)

print(check_strata) 
# The list isn't empty in the CI case, which means that svyglm will crash 
# without the 'adjust' option to center singleton strata at the sample mean.

# Hence, we adjust for singleton strata (lonely PSUs) created by the move to 
# district-level clustering. 
options(survey.lonely.psu = "adjust")


# A) THE IMPACT OF BIRTH REGISTRATION  ON AGE-GAP: existence of a structural barrier?


# We use family = quasibinomial() for 0/1 outcomes in survey data
model_A <- svyglm(
  age_gap ~ hv140 + female + urban + poor_hh + hv009 + hv206 + hv204 + hv220 + 
    schools_per_10000, 
  design = study_sample_design, 
  family = quasibinomial()
)


#
odds_ratios_A <- exp(cbind(OR = coef(model_A), confint(model_A)))
round(odds_ratios_A, 3)


#  Convert rax coeff to Odds Ratio,  add P_value (cf significance of coeff)
results_A <- summary(model_A)$coefficients
odds_ratios_A <- exp(cbind(OR = coef(model_A), confint(model_A)))

# Creating stars based on p-values (column 4 of summary)
stars_A <- ifelse(results_A[,4] < 0.01, "***", ifelse(results_A[,4] < 0.05, "**", ifelse(results_A[,4] < 0.1, "*", "")))

coeff_model_A <- data.frame(
  Variable = rownames(odds_ratios_A),
  Model_A_OR = paste0(round(odds_ratios_A[,1], 3), stars_A),
  stringsAsFactors = FALSE
)


# -----------------#
# OBSERVATION

#Or for hv140 are 0.000
# If in a regression, OR = 0.0000, it can mean one of the 2 following:

# *** Perfect predictor: Every single child with a birth certificate has an 
# age_gap of 0 (or vice versa), making the math "break" because it's too perfect.

# *** Data issue: The variable hv140 might still be mostly 0s or NAs in the 
# design object, even though we thought we fixed it.

# Check the raw relationship
table(study_sample_final$hv140, study_sample_final$age_gap)
#  
#      0     1
# 0 10677  7975
# 1  6122     0

# And we can perfectly see here that we have 0 child in our sample with a birth 
# certificate and an age_gap (perfect predictor issue)

# Might mean in this case that birth certificate are required for enrollment 
# !make research to confirm (or not)!!

#----------


# B) FINAL MULTIVARIATE MODEL (SOCIO-ECONOMIC DRIVERS)

# I exclude hv140 from the regression to allow the model to converge 
# and to isolate the impact of other household-level variables.

model_B <- svyglm(
  age_gap ~ female + urban + poor_hh + hv009 + hv206 + hv204 + hv220, 
  design = study_sample_design, 
  family = quasibinomial()
)

# Extract the Odds Ratios, add P_value (cf significance of coeff) and store in a table 
results_B <- summary(model_B)$coefficients
odds_ratios_B <- exp(cbind(OR = coef(model_B), confint(model_B)))

stars_B <- ifelse(results_B[,4] < 0.01, "***", ifelse(results_B[,4] < 0.05, "**", ifelse(results_B[,4] < 0.1, "*", "")))

coeff_model_B <- data.frame(
  Variable = rownames(odds_ratios_B),
  Model_B_OR = paste0(round(odds_ratios_B[,1], 3), stars_B)
)


### Merge results from model_A and model_B andd add labels


# Sotring the labels in a vector for the final clean table (add title on the table -> "Determinants of Age-gap")
var_labels <- c(
  "(Intercept)" = "Constant",
  "hv140"       = "Birth Registration (Reference: No)",
  "female"      = "Gender (Ref: Male)",
  "urban"       = "Residence (Ref: Rural)",
  "poor_hh"     = "Poverty status (Reference: Rich)",
  "hv009"       = "Household size",
  "hv206"       = "Electricity access",
  "hv204"       = "Time to water source",
  "hv220"       = "Age of household head",
  "schools_per_10000" = "School Density (per 10k)",
  "pop_density"       = "Population Density"
)

# here we use the function full_join otherwise we will lose hv140 which is not in Model B

reg_table_model_A_B <- full_join(coeff_model_A, coeff_model_B, by = "Variable") %>%
  filter(Variable != "(Intercept)") %>%
  mutate(Variable = recode(Variable, !!!var_labels))

# Clean up the perfect predictor display for the client
reg_table_model_A_B <- reg_table_model_A_B %>%
  mutate(Model_A_OR = ifelse(Variable == "Birth Registration (Reference: No)", "Perfect Predictor", Model_A_OR)) %>%
  replace(is.na(.), "-") 


# Export the table
write_csv(reg_table_model_A_B, file.path(path_dhs, "dhs", "01_Output", "Analysis", 
                    "Table_Determinants_of_Education_Age_Gap.csv"))

#--------- FINDINGS REGRESSION B

# Wealth (poor_hh): Highly significant (p<0.01). Strong evidence of credit constraints
# affecting school entry/progression.
# Electricity (hv206): Significant at 5%. Proxy for study environment.
# HH Size (hv009): Significant. Supports "Resource Dilution" theory: larger hh 
# have less financial resource to support educ of younger children
# Gender/Urban: Not significant once wealth is controlled. 

# School density (-0.11) and population density (-0.15) have a negative 
# relationship with age-gap. This makes sense: where there are more schools and 
# more infrastructure (proxied by density), children are less likely to start 
# school late.

# But => strong correlation between urban status and school density (0.36) 
# <=> classic "Multicollinearity" risk in economics. 
# Could mean that the "urban advantage" is just a wealth/infrastructure effect <=>
#  more schools in cities (add paper reference)

# My Model C will try to disentangle this

# C) SPATIAL IMPACT 

# I add the district level variables on top of all the controls from model C to 
# see how the model will be impacted (add ref!! for proof of why I am testing this)



model_C <- svyglm(
  age_gap ~ female + urban + poor_hh + hv009 + hv206 + hv204 + hv220 + 
    schools_per_10000,
  # removed 'pop_density' from the model because the OR is exactly 1 and not significant, 
  design = study_sample_design, 
  family = quasibinomial()
)

# Extracting results for Model c
results_C <- summary(model_C)$coefficients
odds_ratios_C <- exp(cbind(OR = coef(model_C), confint(model_C)))
stars_C <- ifelse(results_C[,4] < 0.01, "***", ifelse(results_C[,4] < 0.05, "**", ifelse(results_C[,4] < 0.1, "*", "")))

coeff_model_C <- data.frame(
  Variable = rownames(odds_ratios_C),
  Model_C_OR = paste0(round(odds_ratios_C[,1], 3), stars_C)
)

# Merge Model A, B, and the new Full Model F
reg_table_model_A_B_C <- coeff_model_A %>% 
  full_join(coeff_model_B, by = "Variable") %>%
  full_join(coeff_model_C, by = "Variable") %>%
  filter(Variable != "(Intercept)") %>%
  mutate(Variable = recode(Variable, !!!var_labels)) %>%
  # Handle the Birth Registration "Perfect Predictor" manually
  mutate(Model_A_OR = ifelse(Variable == "Birth Registration (Reference: No)", 
                             "Perfect Predictor", Model_A_OR)) %>%
  # Replace NAs with a dash for the final presentation
  replace(is.na(.), "-")

# Export to CSV
write_csv(reg_table_model_A_B_C, file.path(path_dhs, "dhs", "01_Output", "Analysis", 
                                       "Final_Determinants_Comparison_Table.csv"))

# Even after controlling everything, including the number of schools in the 
# district, poor households are still 41% more likely to have a schooling delay. 
# This is a proof that: 
# *** "Supply" (building schools) is only half the battle
# *** "Demand" (poverty) is the real barrier.
# hh size negatively (statistically significant at the 1% level) impact age_gap 

# School density and population density do not statistically impact the age-gap 
# once all the other var such as poverty, hh size are controlled
# This mean that just "adding more schools" won't fix the delay if families 
# can't afford the costs of education or don't have birth certificates.


# ---------------------#

### 5) Robustness and heterogeneity checks (interaction effects)

# ---------------------#

# I'm testing 4 hypotheses to see if the school supply effect is "hidden" 
# by interaction with other socio-economic factors.

# Gender (female) was not significant in the Model A, B and C. 

## Hypothesis 1: Interaction between gender and poverty : the double disadvantage (!!ref!!)
model_h1 <- svyglm(age_gap ~ female * poor_hh + urban + hv009 + hv206 + hv204 + hv220 + 
                     schools_per_10000 + pop_density, 
                   design = study_sample_design, family = quasibinomial())

# Hypothesis 2: The "Urban Poverty Trap" (Residence * Poverty) (!!ref!!)
model_h2 <- svyglm(age_gap ~ urban * poor_hh + female + hv009 + hv206 + hv204 + hv220 + 
                     schools_per_10000 + pop_density, 
                   design = study_sample_design, family = quasibinomial())

# Hypothesis 3: The "Supply Sensitivity" (Residence * School Density)
# Does building more schools help rural areas more than urban? (!!add ref!!)
model_h3 <- svyglm(age_gap ~ urban * schools_per_10000 + female + poor_hh + 
                     hv009 + hv206 + hv204 + hv220 + pop_density, 
                   design = study_sample_design, family = quasibinomial())

# Hypothesis 4: The resource dilution theory (Gender * HH Size)  (!!add ref!!)

model_h4 <- svyglm(
  age_gap ~ female * hv009 + urban + poor_hh + hv206 + hv204 + hv220 + 
    schools_per_10000 + pop_density, 
  design = study_sample_design, 
  family = quasibinomial()
)


# Just extracting the interation term for a summary table 

get_interaction <- function(model, term) {
  res <- summary(model)$coefficients
  if (!term %in% rownames(res)) return("-")
  p_val <- res[term, 4]
  or_val <- round(exp(res[term, 1]), 3)
  stars <- ifelse(p_val < 0.01, "***", ifelse(p_val < 0.05, "**", ifelse(p_val < 0.1, "*", "")))
  return(paste0(or_val, stars))
}

hetero_checks_summary <- data.frame(
  "Hypothesis" = c("Gender & Poverty", "Urban & Poverty", "Urban & School supply", "Gender & Household size"),
  "Interaction_OR" = c(
    get_interaction(model_h1, "female:poor_hh"),
    get_interaction(model_h2, "urban:poor_hh"),
    get_interaction(model_h3, "urban:schools_per_10000"),
    get_interaction(model_h4, "female:hv009")
  )
)

# Creating a final clean table report for appendix

hetero_check_report <- hetero_checks_summary %>%
  rename(
    "Testing Hypothesis" = Hypothesis,
    "Interaction Odds Ratio" = Interaction_OR
  ) %>%
  mutate(Interpretation = c(
    "Poverty trap is gender-neutral",
    "Poverty is slightly more 'punishing' in cities",
    "School supply is more effective in urban zones",
    "Large HH size affects boys and girls equally"
  ))

# Save this as a clean CSV for the client
write.csv(hetero_check_report, file.path(path_dhs, "dhs", "01_Output", "Analysis", paste0(country, "_summary_heterogeneity_checks.csv")))


#-------- Interpretation of results

##All hypothesis are not statistically significant 
# but "null results" are also important
# "Double Disadvantage" hypothesis => Poverty hits boys and girls equally. 

# Urban & Poverty hypothesis => OR sign suggest that being poor in a city 
# is slightly more impeding school than being poor in a village (probably 
# related to higher cost of living in big cities) 

# Urban & School Supply => confirms earlier finding => increasing the number of 
# has a positive impact on age_gap in wealthier cities only


#------------------------------------------------------------------------------#
#------------------------- Policy implications --------------------------------#

# ** Birth registration barrier **: 
# Birth registration should be integrated into maternity or any public health 
# center by the government. Also, if a child does not have a certificate by 
# 5 yo, they should be able to start a fast legal procedure to get it before the 
# school year starts. Indeed, without these options even if the districts has 
# the best schools, it will be useless to them

# ** Poverty and infrastructure **: 
# Building more schools is not effective in poor areas. A more effective method 
# could be to incentivize on-time enrollment through cash transfer (Conditional
# Cash transfer <=> income effect ) to the poorest hh. 
# Combine this method with behavioral nudges ( <=> social effect) such as 
# * social recognition : "School Attendance Board" -> attendance rate of each 
# grade  to build a kind of community pride
# * targeted sms reminders: to remind parents of the long-term benefit of going 
# to school : "Is your child in school today? Every day in class is a step toward a better job."

# ** Strategies in urban vs rural areas **:
# There should be *different strategies* depending on the type of place. 
# in *rural areas* (ex: Woroba, Savanes), the goal should be to reduce costs 
# associated with schooling because already existing 
# schools are not used to full capcacity as hh can't bear educ cost 
# (ex of reducing cost: -school kits leasing with the cost returned at 
# the end of the year, -canteen meals (porbably the most effective nudge, give ref!!) 
# In *urban areas* (ex: Abidjan, Bouaké), schools are overcrowded hence increasing 
# schools can reduce congestion and delay

# ** Household size (resource dilution) **:

# Because the marginal cost of sending a child at school inacrease  when the 
# number of children increase, a "progressive subsidy (!!ref!!) should be established
# such as more money is given for the child with higher drop out risk

# ----- other policy implications 
# In CI, we don't need "girls-only" scholarships to solve the age-gap but rather
# support for poor Hhs


                          # ---------------------#


### 6) TREND ANALYSIS

# For when I will add more years

                          # ---------------------#








                         #-------------------#





                        # ---------------------#

message("Data Analysis script completed")







# ------------------------------------------------------ 
