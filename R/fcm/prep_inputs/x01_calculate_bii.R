library(dplyr) # for easy data manipulation
library(tidyr) # ditto
library(tibble) # ditto
library(magrittr) # for piping
library(lme4) # for mixed effects models
library(car) # for logit transformation with adjustment
library(betapart) # for calculating balanced bray-curtis dissimilarity
library(raster) # for working with raster data
library(geosphere) # calculating geographic distance between sites
library(purrr) # running loops
library(furrr) # running loops in parallel
library(viridis) # figure colours for colour blindness

# Load data and filter
db_rds <- here::here(data_dir, 'raw_data', 'biodiversity', 'predicts', "database.rds")
diversity <- readRDS(db_rds) %>%
  filter(UN_region == "Americas") %>%
  filter(str_detect(Biome, regex('forest', ignore_case = TRUE)))

diversity %>% distinct(Biome)
glimpse(diversity)
table(diversity$Predominant_land_use, diversity$Use_intensity)

# ID fields:
# SS = Study number and source ID
# SSB = SS and Block
# SSBS = SSB and site number

# Simplify the land use and intensity classes for exploration ----
# make a level of Primary minimal. Everything else gets the coarse land use
# collapse the secondary vegetation classes together
# change cannot decide into NA
# relevel the factor so that Primary minimal is the first level (so that it is the intercept term in models)
diversity <- diversity %>%
  mutate(
    LandUse = ifelse(Predominant_land_use == "Primary vegetation" & Use_intensity == "Minimal use",
                     "Primary minimal",
                     paste(Predominant_land_use)),
    LandUse = ifelse(grepl("secondary", tolower(LandUse)),
                     "Secondary vegetation",
                     paste(LandUse)),
    LandUse = ifelse(Predominant_land_use == "Cannot decide",
                     NA, 
                     paste(LandUse)),
    LandUse = factor(LandUse),
    LandUse = relevel(LandUse, ref = "Primary minimal")
  )

# Total abundance ----
# Filter to abundance measures
# Group by SSBS (site) and total the abundance measurements
# Get unique sites
# Group by Study ID and get maximum abundance for each study
# Rescale so that abundance varies from 0 to 1 within each study.
abundance_data <- diversity %>%
  filter(Diversity_metric_type == "Abundance") %>%
  
  group_by(SSBS) %>%
  mutate(TotalAbundance = sum(Effort_corrected_measurement)) %>%
  ungroup() %>%
  
  distinct(SSBS, .keep_all = TRUE) %>%
  
  group_by(SS) %>%
  mutate(MaxAbundance = max(TotalAbundance)) %>%
  ungroup() %>%
  
  mutate(RescaledAbundance = TotalAbundance/MaxAbundance)

# Compositional Similarity ----

# Get abundance for studies with 1 sampling effort, >1 species, and some Primary minimal data
cd_data_input <- diversity %>%
  
  # drop any rows with unknown LandUse and get only abundance data
  filter(!is.na(LandUse)) %>%
  filter(Diversity_metric_type == "Abundance") %>%
  
  # Group by Study and calculate: number of unique sampling efforts, number of unique species sampled, check if there are any Primary minimal sites
  group_by(SS) %>%
  mutate(n_sample_effort = n_distinct(Sampling_effort)) %>%
  mutate(n_species = n_distinct(Taxon_name_entered)) %>%
  mutate(n_primin_records = sum(LandUse == "Primary minimal")) %>%
  ungroup() %>%
  
  # Keep only the studies with one unique sampling effort, more than one species, and with at least some Primary minimal data
  filter(n_sample_effort == 1) %>%
  filter(n_species > 1) %>%
  filter(n_primin_records > 0) %>%
  
  # drop empty factor levels
  droplevels()

# Function to calculate compositional similarity between a single pair of sites in a study
get_bray <- function(s1, s2, data){
  
  require(betapart)
  
  # Filter, pivot so that each column is a species and each row is a site, and set the rownames to the SSBS
  sp_data <- data %>%
    filter(SSBS %in% c(s1, s2)) %>%
    dplyr::select(SSBS, Taxon_name_entered, Measurement) %>%
    pivot_wider(names_from = Taxon_name_entered, values_from = Measurement) %>%
    column_to_rownames("SSBS")
  
  # Calculate the balanced bray-curtis if both sites have individuals
  if(sum(rowSums(sp_data) == 0, na.rm = TRUE) == 1){
    # If one of the sites doesn't have any individuals (row sum is 0) then the similarity is 0
    bray <- 0
  
  }else if(sum(rowSums(sp_data) == 0, na.rm = TRUE) == 2){
    # if both sites have no individuals then the similarity is NA
    bray <- NA
    
  }else{
    
    # Calculate the balanced bray-curtis as similarity (1-bray)
    bray <- 1 - 
      bray.part(sp_data) %>%
      pluck("bray.bal") %>%
      pluck(1)
  }
  
}

# List study IDs
studies <- cd_data_input %>%
  distinct(SS) %>%
  pull()

# Get all the site comparisons for each study ----
site_comparisons <- map_dfr(.x = studies, .f = function(x){
  
  # Filter to the given study and get one row for each site
  site_data <- filter(cd_data_input, SS == x) %>%
    dplyr::select(SSBS, LandUse) %>%
    distinct(SSBS, .keep_all = TRUE)
  
  # Filter to sites that are Primary minimal (we only want to use comparisons with the baseline)
  baseline_sites <- site_data %>%
    filter(LandUse == "Primary minimal") %>%
    pull(SSBS)
  
  # Get the site IDs
  site_list <- site_data %>%
    pull(SSBS)
  
  # Get all site x site comparisons for this study
  # rename the columns for the compositional similarity function
  # remove the comparisons where the same site is being compared to itself
  site_comparisons <- expand.grid(baseline_sites, site_list) %>%
    
    rename(s1 = Var1, s2 = Var2) %>%
    filter(s1 != s2) %>%
    
    # make the values characters rather than factors
    # add the full name
    # add the study id
    mutate(s1 = as.character(s1),
           s2 = as.character(s2),
           contrast = paste(s1, "vs", s2, sep = "_"),
           SS = as.character(x))
  
  return(site_comparisons)
})

# Calculate the compositional similarity for each study and set of comparisons ----
# Run in parallel
future::plan(multisession, workers = 4)
bray <- future_map2_dbl(.x = site_comparisons$s1,
                        .y = site_comparisons$s2,
                        ~get_bray(s1 = .x, s2 = .y, data = cd_data_input))

# stop running things in parallel for now
future::plan(sequential)

# Get site coordinates and land uses
latlongs <- cd_data_input %>%
  group_by(SSBS) %>%
  summarise(Lat = unique(Latitude),
            Long = unique(Longitude))

lus <- cd_data_input %>%
  group_by(SSBS) %>%
  summarise(lu = unique(LandUse))

# Combine the data for modeling ----
cd_data <- site_comparisons %>%
  
  # add the bray-curtis data, already in the same order as site_comparisons
  mutate(bray = bray) %>%
  
  # calculate the geographic distances between s1 and s2 sites
  left_join(latlongs, by = c("s1" = "SSBS")) %>% rename(s1_lat = Lat, s1_long = Long) %>%
  left_join(latlongs, by = c("s2" = "SSBS")) %>% rename(s2_lat = Lat, s2_long = Long) %>%
  mutate(geog_dist = distHaversine(cbind(s1_long, s1_lat), cbind(s2_long, s2_lat))) %>%
  
  # create an lu_contrast column (for modelling) from s1 and s2 land use
  left_join(lus, by = c("s1" = "SSBS")) %>% rename(s1_lu = lu) %>%
  left_join(lus, by = c("s2" = "SSBS")) %>% rename(s2_lu = lu) %>%
  mutate(lu_contrast = paste(s1_lu, s2_lu, sep = "_vs_"))

# Run the statistical analysis ----

# Simple model of total abundance ----
# Note that the errors in models of ecological abundance are generally non-normal. 
# Usually, we would deal with that by modelling abundance with an error structure, such as poisson or quasipoisson. 
# In the PREDICTS database, ‘abundance’ measurements are not all whole numbers 
# (e.g., they might be expressed as average numbers per square metre). 
# The lme4 package doesn’t like using a discrete error structure with continuous data 
# so we must transform the data. 
# Generally, a log-transformation does well, but in some cases, a square-root transformation helps to normalise the errors.

# run a simple model
ab_m <- lmer(sqrt(RescaledAbundance) ~ LandUse + (1|SS) + (1|SSB), data = abundance_data)
summary(ab_m)

# This is all just to give you an idea of how to model BII, not how to do statistical analysis… 
# Please check your residual plots etc before using models to make inferences and spatial projections!

# Compositional similarity ----

# Data manipulation before modelling:
# logit transform the compositional similarity
# log10 transform the geographic distance between sites
# make primary minimal-primary minimal the baseline again
cd_data <- cd_data %>%
  mutate(logitCS = logit(bray, adjust = 0.001, percents = FALSE)) %>%
  mutate(log10geo = log10(geog_dist + 1)) %>%
  mutate(lu_contrast = factor(lu_contrast), 
         lu_contrast = relevel(lu_contrast, ref = "Primary minimal_vs_Primary minimal"))

# Model compositional similarity as a function of the land-use contrast and the geographic distance between sites
cd_m <- lmer(logitCS ~ lu_contrast + log10geo + (1|SS) + (1|s2), data = cd_data)
summary(cd_m)

# Projecting the model ----
# Project the abundance and compositional similarity models onto rasters of pressure data
# Then multiply the two rasters to produce BII.

# Model predictions: abundance ----
# Set up a df with all the levels to predict diversity for (all the land-use classes in your model must be in here)
# Calculate the predicted diversity for each of these land-use levels
newdata_ab <- data.frame(LandUse = levels(abundance_data$LandUse)) %>%
  # setting re.form = NA means random effect variance is ignored
  # square the predictions to back-transform the values
  mutate(ab_m_preds = predict(ab_m, ., re.form = NA) ^ 2)

# Model predictions: compositional similarity ----
# Function to calculate the inverse logit of the adjusted logit function
# where f is the value to be back-transformed and a is the adjustment value used for the transformation
inv_logit <- function(f, a){
  a <- (1-2*a)
  (a*(1+exp(f))+(exp(f)-1))/(2*a*(1+exp(f)))
}

# Set up the df with all the levels to predict diversity for
# We had an extra fixed effect in this model (log10geo), so we also have to set a baseline level for this
# We're interested in the compositional similarity when we discount natural turnover, 
# so we want to set this to a static value. 
# We'll use 0 here, but we can also set it to the median geographic distance in the original data or any other meaningful level.
newdata_cd <- data.frame(lu_contrast = levels(cd_data$lu_contrast),
                         log10geo = 0) %>%
  mutate(cd_m_preds = predict(cd_m, ., re.form = NA) %>%
           inv_logit(a = 0.001))

# Land use rasters ----
# generate a dataframe with random numbers for the cell values for each land-use class
lus <- data.frame(pri_min = rnorm(25, mean = 50, sd = 25),
                  pri = rnorm(25, mean = 100, sd = 25),
                  plant = rnorm(25, mean = 100, sd = 25),
                  sec = rnorm(25, mean = 300, sd = 25),
                  crop = rnorm(25, mean = 1000, sd = 25),
                  pas = rnorm(25, mean = 400, sd = 25),
                  urb = rnorm(25, mean = 50, sd = 25)
)

# let's artificially make the first cell dominated by urban land and the last cell dominated by minimally-used primary vegetation
lus$urb[1] <- 2000
lus$pri_min[25] <- 2000

lus <- lus %>%
  # calculate the row totals
  mutate(tot = rowSums(.)) %>%
  
  # now, for each land use, divide the value by the rowsum
  # this will give us the proportion of each land use in each cell
  transmute_at(1:7, list(~ ./tot))

# double check that the proportions of each land use sum to 1 (accounting for rounding errors)
all(zapsmall(rowSums(lus)) == 1)
