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

# Initialize
final_polys_dir <- '/Volumes/GoogleDrive/My Drive/3_Biomass_projects/HIH/data/hih_sites'
export_path <- '/Volumes/GoogleDrive/My Drive/Earth Engine Exports'

shps <- list.files(final_polys_dir, 'shp$', full.names = TRUE)
(polys_fp <- shps[[6]])

site_name_var <- 'HIH_site' # Estonia: 'name'
site_div_var <- 'name' # Estonia: 'div1'

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Upload shapefile ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
task_name <- tools::file_path_sans_ext(basename(polys_fp))
out_fp <- file.path(export_path, str_c(task_name, '.geojson'))

fc_sf <- st_read(polys_fp) %>% st_zm()
fc <- fc_sf %>% sf_as_ee()
fc_dissolved <- fc$union()

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
hansen_30m <- ee$Image("UMD/hansen/global_forest_change_2020_v1_8")
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# QC: Look at carbon stock ca. 2000 for dissolved FC ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Unmasked
total_carbon_mgc <- carbon_mgc$unmask()$ 
  reduceRegions(
    collection = fc_dissolved,
    reducer = ee$Reducer$sum()$unweighted(),
    scale = agb_res_m
  )
# total_carbon_mgc$first()$get('sum')$getInfo() %>% format(big.mark = ',')

# Masked to forest area
total_carbon_mgc <- carbon_mgc$updateMask(forest_mask)$
  reduceRegions(
    collection = fc_dissolved,
    reducer = ee$Reducer$sum()$unweighted(),
    scale = agb_res_m
  )
# total_carbon_mgc$first()$get('sum')$getInfo() %>% format(big.mark = ',')

# Total forest area
total_forest <- forest_2000$reduceRegions(
  collection = fc_dissolved, # add to feature class
  reducer = ee$Reducer$sum()$unweighted(),
  scale = hansen_res_m
)
# total_forest$first()$get('sum')$getInfo() %>% format(big.mark = ',')

# ~~~~~ QC 2000 vs. 2003 biomass ~~~~~~~ ----
# Compare 2000 and 2003 carbon density ----
# 2003 Biomass density and convert to AGC
agb_mgha_2003 <- ee$Image("users/sgorelik/Woodwell/Global_AGB_500m_2003_2016_V6")$
  select('mgha_2003')
carbon_mgcha_2003 <- agb_mgha_2003$divide(2)
carbon_mgc_2003 <- carbon_mgcha_2003$
  multiply(agb_mgha_2003$pixelArea()$divide(1e4))$
  rename('agb_2003_mgc')

# View settings
acd_viz <- list(min = 0, max = 60, palette = viridis)
acd_viz <- list(min = 0, max = 200, palette = viridis)
diff_viz <- list(min = -20, max = 20, palette = c('blue', 'white', 'red'))
legend <- Map$addLegend(visParams = acd_viz, name = NA, opacity = 1)
diff_lgnd <- Map$addLegend(visParams = diff_viz, name = NA, opacity = 1)

Map$centerObject(fc_dissolved, zoom = 7) # Whole country
# Map$setCenter(26.6, 57.8, zoom = 9) # SE area with high biomass in 2003
# Map$setCenter(26.6, 57.73, zoom = 12) # Very zoomed in on SE area
# Map$setCenter(24.3, 58.5, zoom = 10) # Parnu bogs

# Slider
Map$addLayer(carbon_mgcha$updateMask(carbon_mgcha$neq(0)), 
             acd_viz, 'Carbon density, 2000, 500-m') + polys_lyr |
  Map$addLayer(carbon_mgcha_2003$updateMask(carbon_mgcha_2003$neq(0)), 
               acd_viz, 'Carbon density, 2003, 500-m') +
  polys_lyr + legend

# For screenshot
Map$addLayer(carbon_mgcha$updateMask(carbon_mgcha$neq(0)), 
             acd_viz, 'Carbon density, 2000, 500-m') + 
  Map$addLayer(carbon_mgcha_2003$updateMask(carbon_mgcha_2003$neq(0)), 
               acd_viz, 'Carbon density, 2003, 500-m') + polys_lyr + legend

# Resample 2000 30m to 500m resolution ----
c_500m_reproj <- carbon_mgcha$reproject(crs = agb_mgha_2003$projection())

# Difference
diff_500m <- carbon_mgcha_2003$updateMask(carbon_mgcha_2003$gt(0))$
  subtract(c_500m_reproj)

# Slider
Map$addLayer(c_500m_reproj$updateMask(c_500m_reproj$neq(0)), 
             acd_viz, 'Carbon density, 2000, 500-m') + polys_lyr + legend |
  Map$addLayer(carbon_mgcha_2003$updateMask(carbon_mgcha_2003$gt(5)), 
               acd_viz, 'Carbon density, 2003, 500-m') + polys_lyr + legend

# For screenshot
Map$addLayer(diff_500m, diff_viz, 'Difference') + diff_lgnd + 
  Map$addLayer(c_500m_reproj$updateMask(c_500m_reproj$gt(0)), 
               acd_viz, 'Carbon density, 2000, 500-m') +
  Map$addLayer(carbon_mgcha_2003$updateMask(carbon_mgcha_2003$gt(0)), 
               acd_viz, 'Carbon density, 2003, 500-m') + polys_lyr #+ legend 

# Export sample for scatterplot -----
# make an image for the two variables
pairedImage =  ee$ImageCollection$fromImages(
  c(c_500m_reproj$updateMask(c_500m_reproj$neq(0)), 
    carbon_mgcha_2003$updateMask(carbon_mgcha_2003$gt(0))
  ))$
  toBands()$
  rename(c("c_2000","c_2003"))

# Take sample at random points within the region
sample <-  pairedImage$sampleRegions(fc_dissolved, NULL, 2000)

# Export
task_vector <- sample %>% 
  ee_table_to_drive(description = 'sample_500m_2000_2003_gt0',
                    folder = basename(export_path),
                    fileFormat = 'CSV', 
                    timePrefix = FALSE)
task_vector$start()

# Import CSV for plot ----
samp_df <- read_csv(here::here(export_path, 'sample_500m_2000_2003_gt0.csv'))

samp_df %>% 
  filter(c_2003>0 & c_2000>0) %>%
  ggplot(aes(x = c_2000, y = c_2003)) +
  # geom_point(alpha=0.1, size=0.5, fill="royalblue", color="royalblue") +
  geom_bin2d(bins = 40) +
  scale_fill_continuous(type = "viridis") +
  labs(y = 'ACD in 2003 (tC/ha)', 
       x = 'ACD in 2000, resampled to 500-m (tC/ha)') +
  geom_abline(slope = 1, intercept = 0, linetype = 'solid', color = 'darkgray', size=.25) +
  geom_smooth(method="lm", se = FALSE, fullrange=TRUE, col='black', 
              size=.25, linetype = 'dashed') +
  coord_fixed(ratio = 1, xlim=c(0, 65), ylim=c(0, 65)) +
  theme_minimal()

# Save
ggsave(here::here('~/Desktop/Estonia/comparison_reprojected', 
                  'est_scatter_2000v2003_dens.png'),
       width = 4, height = 3.5)
# ~~~~~ end QC ~~~~~ ----

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

fc_2000 <- loss_area$reduceRegions(
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

fc_2000 <- c_loss$reduceRegions(
  collection = fc_2000, # add to feature class containing all loss results
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
fc_2000 <- fc_2000$map(function(f) f$set(list(area_ha = f$area()$divide(1e4))))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build raster stacks of loss ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialize multi-band images
loss_area_sqkm <-ee$Image()$select(c())
loss_carbon_mgc <-ee$Image()$select(c())

# Each band represents either forest area or carbon loss in a given year
for (year in seq(1, 20)) { # 1-20 = 2001-2020
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
  collection = fc_2000, # adds columns forest_loss_[year]_ha
  reducer = ee$Reducer$sum()$unweighted(),
  scale = hansen_res_m
)

# summarize annual forest carbon loss by district
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
# ee_monitoring(task_vector, quiet = TRUE) # optional
# local_fp <- ee_drive_to_local(task_vector, 
#                   dsn = file.path('outputs', str_c(task_name, '.geojson')), 
#                   overwrite = TRUE)

# ~~~~~~ EXTRAS ~~~~~~~~ ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Uxe Hansens's 2000-2020 loss to mask ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Mask 2000 tree cover ----
loss <- hansen_30m$select(c('loss'))
fc2020_fromloss <- forest_mask$updateMask(loss$neq(1))

# View
Map$addLayer(eeObject = forest_mask, 
             visParams = list(min = 0, max = 1, palette = viridis),
             name = 'Forest 2000') +
Map$addLayer(eeObject = fc2020_fromloss, 
             visParams = list(min = 0, max = 1, palette = agb_pal),
             name = 'Forest 2020')

(site <- str_split(basename(polys_fp), '[_\\W]', simplify = TRUE)[,1])
task_name <- str_c('FC_2020_', site)

# Create extent rectangle ----
library(tmap)
tmap_mode('view')

site_sf <- st_read(polys_fp)
site_buff <- site_sf %>% st_buffer(0.4)
bb <- site_buff %>% st_bbox()

qtm(site_buff) + qtm(site_sf)

site_bb <- ee$Geometry$Rectangle(
  coords = c(bb$xmin, bb$ymin, bb$xmax, bb$ymax),
  proj = "EPSG:4326",
  geodesic = FALSE
)

# Export
task_img_to_drive <- fc2020_fromloss %>% 
  ee_image_to_drive(description = task_name,
                    folder = basename(export_path),
                    region = site_bb,
                    scale = 30)

task_img_to_drive$start()

# 
# # Mask AGC with loss band ----
# loss <- hansen_30m$select(c('loss'))
# loss <- loss$updateMask(loss$neq(1))
# agb2020_fromloss <- carbon_mgc$
#   updateMask(loss$mask())$
#   updateMask(carbon_mgc$neq(0))
# 
# # View
# Map$addLayer(eeObject = loss_year, 
#              visParams = list(min = 0, max = 20, palette = viridis), 
#              name = 'Hansen loss year') +
#   Map$addLayer(eeObject = fc_2000, 
#                visParams = list(min = 0, max = 1, palette = viridis), 
#                name = 'Forest') +
#   Map$addLayer(eeObject = carbon_mgc$updateMask(carbon_mgc$neq(0)), 
#                visParams = list(min = 0, max = 20, palette = agb_pal), 
#                name = '2000 AGC', 
#                shown = FALSE) +
#   Map$addLayer(eeObject = agb2020_fromloss, 
#                visParams = list(min = 0, max = 20, palette = agb_pal), 
#                name = '2020 AGC from loss') +
#   Map$addLayer(eeObject = loss, 
#                visParams = list(min = 0, max = 1, palette = viridis), 
#                name = 'Hansen loss mask', 
#                shown = FALSE) +
#   polys_lyr


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compare AGB>0 to TC>25% ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # AGB ----
# # Get forested pixels based on biomass
# agb_2000 <- agb_mgha$gt(0)
# agb_2000 <- agb_2000$unmask()$updateMask(agb_2000$eq(1))
# 
# # Count forested pixels by district
# ct_agb <- agb_2000$reduceRegions(
#   collection = estonia, # add to feature class
#   reducer = ee$Reducer$count(),
#   scale = agb_res_m
# )
# ct_agb_num <- ct_agb$first()$get('count')$getInfo()
# 
# # Tree cover ----
# # Count forested pixels by district
# ct_hansen <- forest_mask$reduceRegions(
#   collection = estonia, # add to feature class
#   reducer = ee$Reducer$count(),
#   scale = hansen_res_m
# )
# ct_hansen_num <- ct_hansen$first()$get('count')$getInfo()
# 
# # Where is AGB > 0 but Hansen shows non-forest? ----
# agb_nonforest <- agb_2000$unmask()$multiply(forest_mask$unmask()$eq(0))
# agb_nonforest <- agb_nonforest$updateMask(agb_nonforest$neq(0))
# 
# # Map$addLayer(agb_2000$unmask(), list(palette = viridis)) +
# #   Map$addLayer(forest_mask, list(palette = c('red', 'green'))) +
# Map$addLayer(agb_nonforest, list(min = 0, max = 1, palette = c('blue', 'orange'))) +
#   Map$addLayer(agb_forest, list(min = 0, max = 1, palette = c('blue', 'orange')))
# 
# # Count forested pixels by district
# ct_nonforest <- agb_nonforest$reduceRegions(
#   collection = estonia, # add to feature class
#   reducer = ee$Reducer$count(),
#   scale = hansen_res_m
# )
# ct_nonforest_num <- ct_nonforest$first()$get('count')$getInfo()
# 
# # Compare
# ct_nonforest_num / ct_agb_num
# 
# # Where is AGB==0 but Hansen shows forest? ----
# forest_nonagb <- forest_mask$unmask()$multiply(agb_2000$unmask()$eq(0))
# forest_nonagb <- forest_nonagb$updateMask(forest_nonagb$neq(0))
# 
# # Count forested pixels by district
# ct_nonagb <- forest_nonagb$reduceRegions(
#   collection = estonia, # add to feature class
#   reducer = ee$Reducer$count(),
#   scale = hansen_res_m
# )
# (ct_nonagb_num <- ct_nonagb$first()$get('count')$getInfo())
# 
# # Get carbon stock ----
# carbon_mgc <- carbon_mgc$updateMask(agb_nonforest)
# 
# # Map$addLayer(agb_nonforest, list(palette = c('blue', 'orange'))) |
# #   Map$addLayer(carbon_mgc, list(palette = agb_pal))
# 
# # Count forested pixels by district
# mgc_nonforest <- carbon_mgc$reduceRegions(
#   collection = estonia, # add to feature class
#   reducer = ee$Reducer$sum()$unweighted(),
#   scale = agb_res_m
# )
# mgc_nonforest_num <- mgc_nonforest$first()$get('sum')$getInfo()
# 
# # Get carbon density ----
# carbon_mgcha <- carbon_mgcha$updateMask(agb_nonforest)
# 
# # Map$addLayer(agb_nonforest, list(palette = c('blue', 'orange'))) |
# #   Map$addLayer(carbon_mgc, list(palette = agb_pal))
# 
# # Count forested pixels by district
# mgcha_nonforest <- carbon_mgcha$reduceRegions(
#   collection = estonia, # add to feature class
#   reducer = ee$Reducer$mean(),
#   scale = agb_res_m
# )
# mgcha_nonforest_num <- mgcha_nonforest$first()$get('mean')$getInfo()

