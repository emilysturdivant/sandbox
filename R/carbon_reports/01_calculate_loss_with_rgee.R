#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * Calculate annual biomass loss for input polygons
# Requires:
#     * GEE account with access to global_AGB_2000_30m_Mgha_V4
#     * site polygon
#     * 02_report_from_Hansen_data.R for functions following the GEE section
# Author:
#     * esturdivant@woodwellclimate.org, 2021-10-10
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialize ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# source(here::here('R/carbon_reports', '000_initialize.R'))

# Load libraries 
library(rgee)
ee_Initialize()

# # Initialize
# final_polys_dir <- '/Volumes/GoogleDrive/My Drive/3_Biomass_projects/HIH/data/hih_sites'
# export_path <- '/Volumes/GoogleDrive/My Drive/Earth Engine Exports'
# final_polys_dir <- '~/Downloads/hih_sites'
# export_path <- here::here('data/gee_exports')
# 
# shps <- list.files(final_polys_dir, 'shp$', full.names = TRUE)
# (polys_fp <- shps[[9]])
# 
# site_name_var <- 'HIH_site' # Estonia: 'name'
# site_div_var <- 'name' # Estonia: 'div1'

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Upload vector file ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load polygon/s
fc_sf <- st_read(polys_fp) %>% st_zm()

library(tmap)
tmap_mode('view')
qtm(fc_sf)

# Upload to GEE
fc <- fc_sf %>% sf_as_ee()

# Viewing parameters
sz <- fc$size()$getInfo()
fc_dissolved <- if(sz > 1) fc$union() else fc

Map$centerObject(fc_dissolved, zoom = 10)
agb_pal <- c('75322B','84512A','8E6232','da8c19','ef9e0b','ffc011','ffdb2d',
             'ffe215','f9eb46','d5e400','c9d800','becc00','b4c200','B7B95B',
             'B2B659','AFB457','ABB156','A6AE53','A3AB52','A1AA51','9FA950',
             '9EA850','9CA74F','9BA64E','9AA54E','99A44D','95A24C','92A04A',
             '909E49','8C9C48','8B9A47','869745','859745','839544','839543',
             '819443','7E9241','7A8F40','778D3E','758C3E','758B3D','728A3C',
             '71893C','70883B','6F873B','6D863A','6A8438','678237','648036',
             '627E37','607D34','5E7B33','5A7831','577630','53742E','50722D',
             '4F712C','4E702C','4C6F2B','4A6D2A','496D29','486C29','486C29',
             '476B29','466A28','426827','3E6525','3B6323','3A6223','396222',
             '386122','355F21','345E22','315C1F','305B1E','2C591D','2B581C',
             '28561B','27551A','255419','245319','235218','225218','225118',
             '215118','205017','1F4F17','1C4E16','1B4D15','1A4C15','194C14',
             '184A14','164913','154812','124711','114610','114610','114610',
             '114610')
agb_pal <- agb_pal %>% str_c("#", .)
viridis <- c('#440154', '#433982', '#30678D', '#218F8B', '#36B677', '#8ED542', '#FDE725')

# Paint all the polygon edges with the same number and width, display.
outline <- ee$Image()$byte()$
  paint(featureCollection = fc_dissolved, color = 1, width = 2)
polys_lyr <- Map$addLayer(outline, name = 'Site boundary')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data and mask to forest (TC>25%) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Hansen forest cover and loss
hansen_30m <- ee$Image("UMD/hansen/global_forest_change_2021_v1_9")
tc_2000 <- hansen_30m$select(c('treecover2000'))
loss_year <- hansen_30m$select(c('lossyear'))
loss <- hansen_30m$select(c('loss'))

# Biomass density
agb_mgha <- ee$Image("users/sgorelik/global_AGB_2000_30m_Mgha_V4")

# convert 30m AGBD (Mg/ha) ca. 2000 to AGC (MgC)
carbon_mgcha <- agb_mgha$divide(2)             # convert biomass to carbon
carbon_mgc <- carbon_mgcha$
  # multiply(agb_res_m)$multiply(agb_res_m)$divide(1e4)$
  multiply(agb_mgha$pixelArea()$divide(1e4))$  # convert density to stock
  rename('agb_2000_mgc')                       # rename band/image

# Get forested pixels based on Hansen
forest_mask <- tc_2000$gt(25)
forest_mask <- forest_mask$unmask()$updateMask(forest_mask$eq(1))

# Mask layers to forested areas
loss_year <- loss_year$updateMask(forest_mask)
loss <- loss$updateMask(loss$eq(1))$updateMask(forest_mask)
carbon_mgc_masked <- carbon_mgc$updateMask(forest_mask)

# Map$addLayer(forest_mask, list(palette = viridis)) +
#   Map$addLayer(loss_year, list(min = 0, max = 20, palette = viridis)) +
#   Map$addLayer(loss, list(palette = viridis))

# spatial resolution
hansen_res_m <- loss_year$projection()$nominalScale()
agb_res_m <- agb_mgha$projection()$nominalScale()
agb_res_km <- agb_res_m$divide(1e3)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get baseline (2000) values for forest area and carbon ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get pixel area 
forest_2000 <- tc_2000$gt(25)$
  multiply(tc_2000$pixelArea()$divide(1e4))$
  rename(str_c('forest_2000_ha'))

# summarize Forest area ca. 2000 by district
fc_agg <- forest_2000$reduceRegions(
  collection = fc, # add to feature class
  reducer = ee$Reducer$sum()$unweighted(),
  scale = hansen_res_m
)$map(function(f){ # Rename sum column
  ee$Feature(f$geometry(), list(
    name = f$get(site_name_var),
    div1 = f$get(site_div_var),
    forest_2000_ha = f$get('sum'))
  )
})

# Get total carbon stock ca. 2000 by district ----
fc_2000 <- carbon_mgc$ #updateMask(forest_mask)$
  reduceRegions(
    collection = fc_agg, # add to feature class containing all loss results
    reducer = ee$Reducer$sum()$unweighted(),
    scale = agb_res_m
  )$map(function(f){ # Rename sum column
    ee$Feature(f$geometry(), list(
      name = f$get(site_name_var), 
      div1 = f$get(site_div_var),
      forest_2000_ha = f$get('forest_2000_ha'),
      carbon_2000_mgc = f$get('sum'))
    )
  })

# fc_2000$first()$get(site_div_var)$getInfo() %>% format(big.mark = ',')
# fc_2000$first()$get('forest_2000_ha')$getInfo() %>% format(big.mark = ',')
# fc_2000$first()$get('carbon_2000_mgc')$getInfo() %>% format(big.mark = ',')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get total loss values (2000-2020) for forest area and carbon ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Forest area loss
# Get pixel area with GEE
loss_area <- loss$
  multiply(loss$pixelArea()$divide(1e4))

fc_2000_loss <- loss_area$reduceRegions(
  collection = fc_2000, # add to feature class containing all loss results
  reducer = ee$Reducer$sum()$unweighted(),
  scale = agb_res_m
)$map(function(f){ # Rename sum column
  ee$Feature(f$geometry(), list(
    name = f$get(site_name_var), 
    div1 = f$get(site_div_var),
    forest_2000_ha = f$get('forest_2000_ha'),
    carbon_2000_mgc = f$get('carbon_2000_mgc'),
    forest_loss_ha = f$get('sum'))
  )
})
# fc_2000$first()$get('forest_loss_ha')$getInfo()

# Carbon stock loss 
c_loss <- carbon_mgc$
  updateMask(loss$neq(0))$
  updateMask(carbon_mgc$neq(0))

fc_c_loss <- c_loss$reduceRegions(
  collection = fc_2000_loss, # add to feature class containing all loss results
  reducer = ee$Reducer$sum()$unweighted(),
  scale = agb_res_m
)$map(function(f){ # Rename sum column
  ee$Feature(f$geometry(), list(
    name = f$get(site_name_var), 
    div1 = f$get(site_div_var),
    forest_2000_ha = f$get('forest_2000_ha'),
    carbon_2000_mgc = f$get('carbon_2000_mgc'),
    forest_loss_ha = f$get('forest_loss_ha'),
    c_loss_mgc = f$get('sum'))
  )
})

# Get polygon areas ----
fc_c_loss_area <- fc_c_loss$map(function(f) f$set(list(area_ha = f$area(1)$divide(1e4))))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build raster stacks of loss ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialize multi-band images
loss_area_sqkm <-ee$Image()$select(c())
loss_carbon_mgc <-ee$Image()$select(c())

# Each band represents either forest area or carbon loss in a given year
for (year in seq(1, 21)) { # 1-20 = 2001-2020
  # year_str <-'20' + ee$Number(year)$format('%02d')$getInfo()
  year_str <- as.character(2000+year)
  
  # get deforested pixels that year
  tmp_loss_mask <- loss_year$eq(year)
  
  # area loss - convert to ha
  tmp_area <- tmp_loss_mask$
    multiply(tmp_loss_mask$pixelArea()$divide(1e4))$
    rename(str_c('forest_loss_', year_str, '_ha'))
  loss_area_sqkm <- loss_area_sqkm$addBands(tmp_area)
  
  # carbon loss
  tmp_carbon <-carbon_mgc$
    multiply(tmp_loss_mask)$ # only keep deforested pixels that year
    rename(str_c('carbon_loss_', year_str, '_mgc'))
  loss_carbon_mgc <-loss_carbon_mgc$addBands(tmp_carbon)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sum forest area and carbon each year by region ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# summarize annual forest area loss by district
loss_fc <- loss_area_sqkm$reduceRegions(
  collection = fc_c_loss_area, # adds columns forest_loss_[year]_ha
  reducer = ee$Reducer$sum()$unweighted(),
  scale = hansen_res_m
)

# summarize annual carbon loss by district
loss_fc_carbon <- loss_carbon_mgc$reduceRegions(
  collection = loss_fc, # add columns carbon_loss_[year]_mgc to FC with area loss results
  reducer = ee$Reducer$sum()$unweighted(),
  scale = agb_res_m
)

# loss_fc_carbon$first()$propertyNames()$getInfo()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Convert to GeoJSON ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
task_vector <- loss_fc_carbon %>% 
  ee_table_to_drive(description = task_name,
                    folder = basename(export_path),
                    fileFormat = 'GeoJSON', 
                    timePrefix = FALSE)
task_vector$start()

# EXTRAS moved to x02_extras_calculate_loss_rgee.R: ----
#   Use Hansens's 2000-2020 loss to mask 
#   Mask 2000 tree cover
#   Create extent rectangle
#   Mask AGC with loss band
#   Compare AGB>0 to TC>25% 
#   Get carbon stock
#   Get carbon density
