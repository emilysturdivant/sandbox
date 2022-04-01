#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * Create a forest cover mask for the most recent year (2020)
# Requires:
#     * GEE account with access to global_AGB_2000_30m_Mgha_V4
#     * site polygon
# Author:
#     * esturdivant@woodwellclimate.org, 2022-03-21
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialize ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source(here::here('R/carbon_reports', '000_initialize.R'))

# Load libraries 
library(rgee)
ee_Initialize()

tmap_mode('view')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Upload shapefile ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create extent rectangle ----
site_sf <- st_read(polys_fp)
site_buff <- site_sf %>% st_buffer(2) # 0.6 for little sites, 1-2 for TdM
bb <- site_buff %>% st_bbox()

# qtm(site_buff) + qtm(site_sf)

site_bb <- ee$Geometry$Rectangle(
  coords = c(bb$xmin, bb$ymin, bb$xmax, bb$ymax),
  proj = "EPSG:4326",
  geodesic = FALSE
)

Map$centerObject(site_bb, zoom = 10)
agb_pal <- c('75322B','ffe215','B2B659','9EA850','909E49','819443','71893C',
             '627E37','4F712C','476B29','386122','28561B','215118','184A14',
             '114610') %>% 
  str_c("#", .)
viridis <- c('#440154', '#433982', '#30678D', '#218F8B', '#36B677', '#8ED542', '#FDE725')

# Paint all the polygon edges with the same number and width, display.
fc <- site_sf %>% st_zm() %>% sf_as_ee()
fc_dissolved <- fc$union()
outline <- ee$Image()$byte()$
  paint(featureCollection = fc_dissolved, color = 1, width = 2)
polys_lyr <- Map$addLayer(outline)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Use Hansens loss to mask Hansen 2000 TC ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Hansen forest cover and loss
hansen_30m <- ee$Image("UMD/hansen/global_forest_change_2020_v1_8")
tc_2000 <- hansen_30m$select(c('treecover2000'))
loss_year <- hansen_30m$select(c('lossyear'))
loss <- hansen_30m$select(c('loss'))

# Get forested pixels based on Hansen
forest_mask <- tc_2000$gt(25)
forest_mask <- forest_mask$unmask()$updateMask(forest_mask$eq(1))

# Mask 2000 tree cover with loss band ----
loss <- hansen_30m$select(c('loss'))
fc2020_fromloss <- forest_mask$updateMask(loss$neq(1))

# View
Map$addLayer(eeObject = forest_mask, 
             visParams = list(min = 0, max = 1, palette = viridis),
             name = 'Forest 2000') +
Map$addLayer(eeObject = fc2020_fromloss, 
               visParams = list(min = 0, max = 1, palette = agb_pal),
               name = 'Forest 2020') +
  polys_lyr

# Look at forest loss in 2015 and 2016 ----
loss_year$filter(ee$Filter$eq())
  
# Reclass values outside of range of interest
loss_year_focus <- loss_year$
  updateMask(loss_year$lte(17))$
  updateMask(loss_year$gt(14))

Map$addLayer(eeObject = loss_year_focus, 
             visParams = list(min = 14, max = 17, palette = viridis),
             name = 'Loss year') +
  polys_lyr +
  Map$addLegend(list(min = 14, max = 17, palette = viridis), 
                color_mapping = 'discrete')

# Export Hansen ----
task_name <- str_c('FC_2020_', site)

task_img_to_drive <- fc2020_fromloss %>% 
  ee_image_to_drive(description = task_name,
                    folder = basename(export_path),
                    region = site_bb,
                    scale = 90, 
                    maxPixels = 105713753)

task_img_to_drive$start()

# Loss year ----
task_name <- str_c('lossyear_', site)
task_img_to_drive <- loss_year %>% 
  ee_image_to_drive(description = task_name,
                    folder = basename(export_path),
                    region = site_bb,
                    scale = 30)

task_img_to_drive$start()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Use Hansen loss to mask 500m AGB ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wbd_mgha <- ee$Image("users/sgorelik/WHRC_Global_Biomass_500m_V6/Current_AGB_Mgha")

# Mask 
agb16_fromloss <- wbd_mgha$updateMask(loss$neq(1))

# View
Map$addLayer(eeObject = agb16_fromloss, 
             visParams = list(min = 0, max = 200, palette = viridis),
             name = 'AGB 2016')

# Export
task_name <- str_c('AGB_2016_', site)

task_img_to_drive <- agb16_fromloss %>% 
  ee_image_to_drive(description = task_name,
                    folder = basename(export_path),
                    region = site_bb,
                    scale = 500)

task_img_to_drive$start()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Use Hansens's 2000-2020 loss to mask our 30m AGB ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
agb_mgha <- ee$Image("users/sgorelik/global_AGB_2000_30m_Mgha_V4")

# Mask
agb00_fromloss <- agb_mgha$updateMask(loss$neq(1))

# View
Map$addLayer(eeObject = agb00_fromloss, 
             visParams = list(min = 0, max = 200, palette = viridis),
             name = 'AGB 2000')

# Export 
task_name <- str_c('AGB_2000_', site)

task_img_to_drive <- agb00_fromloss %>% 
  ee_image_to_drive(description = task_name,
                    folder = basename(export_path),
                    region = site_bb,
                    scale = 30)

task_img_to_drive$start()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export SRTM DEM ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
img <- ee$Image("USGS/SRTMGL1_003")
dem <- img$select('elevation')

# View
Map$addLayer(eeObject = dem, 
             visParams = list(min = 0, max = 2000, palette = viridis),
             name = 'DEM')

# Export 
task_name <- str_c('SRTM_DEM_', site)

task_img_to_drive <- dem %>% 
  ee_image_to_drive(description = task_name,
                    folder = basename(export_path),
                    region = site_bb,
                    scale = 30)

task_img_to_drive$start()
