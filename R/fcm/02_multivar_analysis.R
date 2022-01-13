#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * Perform exploratory multivariate analysis
# Requires:
#     * GEE account
# Author:
#     * esturdivant@woodwellclimate.org, 2022-01-11
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialize ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source(here::here('R/fcm/01_standardize_layers.R'))

# Create test extent ----
test_bb <- ee$Geometry$Rectangle(
  coords = c(-62, -15, -45, 2),
  proj = "EPSG:4326",
  geodesic = FALSE
)

# Create global extent ----
world_bb <- ee$Geometry$Rectangle(
  coords = c(-180, -90, 180, 90),
  proj = "EPSG:4326",
  geodesic = FALSE
)

west_bb <- ee$Geometry$Rectangle(
  coords = c(-179, -89, 0, 89),
  proj = "EPSG:4326",
  geodesic = FALSE
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract sample for tests ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plot histogram of sample exported from GEE ----
img <- wcd_norm

# Take sample at random points within the region
sample <-  img$sampleRegions(tropics_bb, NULL, scale = 2000)

# Take sample at X random points
sample <-  img$sample(numPixels = 5e3) # couldn't export

# Export
task_name <- 'sample_wcd_norm_5e3'
task_vector <- sample %>%
  ee_table_to_drive(description = task_name,
                    folder = basename(export_path),
                    fileFormat = 'CSV',
                    timePrefix = FALSE)
task_vector$start()

# Import CSV for plot
samp_csv <- here::here(export_path, str_c(task_name, '.csv'))
samp_df <- read_csv(samp_csv)
n <- nrow(samp_df)

# Plot histogram
samp_df %>%
  ggplot() +
  geom_histogram() +
  labs(y = str_glue('Number of pixels (N = {n})'),
       x = 'WCD (tC/ha)') +
  theme_minimal()

# Save
ggsave(here::here('outputs/fcm/histograms', str_c(task_name, '.png')),
       width = 4, height = 3.5)


# Export test region ----
img <- wcd_norm

# Paint all the polygon edges with the same number and width, display.
outline <- ee$Image()$byte()$paint(featureCollection = test_bb, color = 1, width = 2)
Map$centerObject(test_bb)
(test_lyr <- Map$addLayer(outline, name = 'Test area', shown = TRUE))

task_name <- 'WCD_norm_testbrazil'

# Export test region
task_img_to_drive <- img %>%
  ee_image_to_drive(description = task_name,
                    folder = basename(export_path),
                    region = test_bb,
                    scale = 500,
                    maxPixels = 312352344)
task_img_to_drive$start()



# Import TIF for plot
library(terra)

test_img <- list.files(export_path, pattern = str_c(task_name, '.*tif'), full.names = TRUE)
r <- terra::rast(test_img)
terra::hist(r)

# Plot histogram
df <- r %>%
  as.data.frame() 
n <- nrow(df)

df %>% 
  ggplot() +
  geom_histogram() +
  labs(y = str_glue('Number of pixels (N = {n})'),
       x = 'WCD (tC/ha)') +
  theme_minimal()

# Save
ggsave(here::here('outputs/fcm/histograms', str_c(task_name, '.png')),
       width = 4, height = 3.5)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Distribution of each layer ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cross-correlation ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stack <- wcd_norm$addBands(soc_norm)$rename(c('WCD', 'SOC'))

# sample the stack:
sample <- stack$sample(region = west_bb, 
                       scale = 500, 
                       projection = 'EPSG:4326', 
                       numPixels = 1e5,
                       dropNulls = TRUE)

sample$first()$getInfo()

# // Then choose two properties to see the correlation:
pearsons <- sample$reduceColumns(ee$Reducer$pearsonsCorrelation(), 
                                c('WCD', 'SOC'))
pearsons$getInfo()

spearmans <- sample$reduceColumns(ee$Reducer$spearmansCorrelation(), 
                                 c('WCD', 'SOC'))
s <- spearmans$getInfo()
s$correlation


# use Spearman correlation matrix


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cronbach's alpha ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PCA ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cluster analysis ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
