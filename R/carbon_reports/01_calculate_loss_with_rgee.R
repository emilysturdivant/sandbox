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
# Load libraries 
library(rgee)
ee_Initialize()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Upload shapefile ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
task_name <- tools::file_path_sans_ext(basename(polys_fp))
out_fp <- file.path(export_path, str_c(task_name, '.geojson'))

fc <- st_read(polys_fp) %>% sf_as_ee()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build raster stacks of loss ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hansen_30m <- ee$Image("UMD/hansen/global_forest_change_2020_v1_8")
agb_30m_mgha <- ee$Image("users/sgorelik/global_AGB_2000_30m_Mgha_V4")

# # 30m Hansen loss year (0 = no loss; 1 - 20 = forest loss in year 2001 - 2018, respectively)
loss_year <- hansen_30m$select(c('lossyear'))

# # spatial resolution
hansen_res_m <- loss_year$projection()$nominalScale()
agb_res_m <- agb_30m_mgha$projection()$nominalScale()
agb_res_km <- agb_res_m$divide(1e3)

# convert 30m AGBD (Mg/ha) ca. 2000 to AGC (MgC)
agb_30m_mgc <- agb_30m_mgha$
  multiply(agb_res_m)$
  multiply(agb_res_m)$
  divide(2e4)$
  rename('agb_2000_mgc')

# build multi-band images, where each band represents either forest area or carbon loss in a given year
loss_area_sqkm <-ee$Image()$select(c())
loss_carbon_mgc <-ee$Image()$select(c())

for (year in seq(1, 20)) { # 1-20 = 2001-2020
  # year_str <-'20' + ee$Number(year)$format('%02d')$getInfo()
  year_str <- as.character(2000+year)
  
  
  tmp_loss_mask <- loss_year$eq(year)
  
  # area loss
  tmp_area <- tmp_loss_mask$
    multiply(hansen_res_m)$
    multiply(hansen_res_m)$
    divide(1e4)$
    rename(str_c('forest_loss_', year_str, '_ha'))
  loss_area_sqkm <- loss_area_sqkm$addBands(tmp_area)
  
  # carbon loss
  tmp_carbon <-agb_30m_mgc$
    multiply(tmp_loss_mask)$ # only keep deforestated pixels that year
    rename(str_c('carbon_loss_', year_str, '_mgc'))
  loss_carbon_mgc <-loss_carbon_mgc$addBands(tmp_carbon)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sum forest area and carbon each year by region ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# summarize annual forest area loss by district
fc_area_loss_sqkm <- loss_area_sqkm$reduceRegions(
  collection = fc, # adds columns forest_loss_[year]_ha
  reducer = ee$Reducer$sum(),
  scale = hansen_res_m
)

# summarize annual forest carbon loss by district
fc_carbon_loss_mgc <- loss_carbon_mgc$reduceRegions(
  collection = fc_area_loss_sqkm, # add columns carbon_loss_[year]_mgc to FC with area loss results
  reducer = ee$Reducer$sum(),
  scale = agb_res_m
)

# summarize AGB ca. 2000 by district
fc_carbon_2000_mgc <- agb_30m_mgc$reduceRegions(
  collection = fc_carbon_loss_mgc, # add to feature class containing all loss results
  reducer = ee$Reducer$sum(),
  scale = agb_res_m
)

# fc_carbon_2000_mgc$first()$getInfo()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Convert to GeoJSON ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
task_vector <- fc_carbon_2000_mgc %>% 
  ee_table_to_drive(description = task_name,
                    folder = basename(export_path),
                    fileFormat = 'GeoJSON', 
                    timePrefix = FALSE)
task_vector$start()
ee_monitoring(task_vector, quiet = TRUE) # optional
# local_fp <- ee_drive_to_local(task_vector, 
#                   dsn = file.path('outputs', str_c(task_name, '.geojson')), 
#                   overwrite = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Use Hansens's 2000-2020 loss to mask our 30m AGC ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Paint all the polygon edges with the same number and width, display.
outline <- ee$Image()$byte()$
  paint(featureCollection = fc, color = 1, width = 2)
polys_lyr <- Map$addLayer(outline, name = 'HIH sites')

# Mask AGC with loss band
loss <- hansen_30m$select(c('loss'))
loss <- loss$updateMask(loss$neq(1))
agb2020_fromloss <- agb_30m_mgc$
  updateMask(loss$mask())$
  updateMask(agb_30m_mgc$neq(0))

# View
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
viridis <- c('#440154', '#433982', '#30678D', '#218F8B', '#36B677', '#8ED542', '#FDE725')
Map$addLayer(eeObject = loss_year, 
             visParams = list(min = 0, max = 20, palette = viridis), 
             name = 'Hansen loss year') +
  Map$addLayer(eeObject = agb_30m_mgc$updateMask(agb_30m_mgc$neq(0)), 
               visParams = list(min = 0, max = 20, palette = agb_pal), 
               name = '2000 AGC', 
               shown = FALSE) +
  Map$addLayer(eeObject = agb2020_fromloss, 
               visParams = list(min = 0, max = 20, palette = agb_pal), 
               name = '2020 AGC from loss') +
  Map$addLayer(eeObject = loss, 
               visParams = list(min = 0, max = 1, palette = viridis), 
               name = 'Hansen loss mask', 
               shown = FALSE) +
  polys_lyr
