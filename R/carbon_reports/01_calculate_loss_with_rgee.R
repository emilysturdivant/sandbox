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
estonia <- fc$union()

Map$centerObject(estonia, zoom = 7)
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build raster stacks of loss ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hansen_30m <- ee$Image("UMD/hansen/global_forest_change_2020_v1_8")
agb_30m_mgha <- ee$Image("users/sgorelik/global_AGB_2000_30m_Mgha_V4")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Compare AGB>0 to TC>25% ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# AGB ----
# Get forested pixels based on biomass
agb_2000 <- agb_30m_mgha$gt(0)
agb_2000 <- agb_2000$unmask()$updateMask(agb_2000$eq(1))

# Count forested pixels by district
ct_agb <- agb_2000$reduceRegions(
  collection = estonia, # add to feature class
  reducer = ee$Reducer$count(),
  scale = agb_res_m
)
ct_agb_num <- ct_agb$first()$get('count')$getInfo()

# HANSEN TREE COVER ----
# Get forested pixels based on Hansen
tc_2000 <- hansen_30m$select(c('treecover2000'))
forest_2000 <- tc_2000$gt(25)
forest_2000 <- forest_2000$unmask()$updateMask(forest_2000$eq(1))

# Count forested pixels by district
ct_hansen <- forest_2000$reduceRegions(
  collection = estonia, # add to feature class
  reducer = ee$Reducer$count(),
  scale = hansen_res_m
)
ct_hansen_num <- ct_hansen$first()$get('count')$getInfo()

# MASK AGB forest with HANSEN forest ----
agb_nonforest <- agb_2000$unmask()$multiply(forest_2000$unmask()$eq(0))
agb_nonforest <- agb_nonforest$updateMask(agb_nonforest$neq(0))

# Map$addLayer(agb_2000$unmask(), list(palette = viridis)) +
#   Map$addLayer(forest_2000$unmask(), list(palette = c('red', 'green'))) +
#   Map$addLayer(agb_nonforest, list(palette = c('blue', 'orange')))

# Count forested pixels by district
ct_nonforest <- agb_nonforest$reduceRegions(
  collection = estonia, # add to feature class
  reducer = ee$Reducer$count(),
  scale = hansen_res_m
)
ct_nonforest_num <- ct_nonforest$first()$get('count')$getInfo()

# Compare
ct_nonforest_num / ct_agb_num

# Where does AGB==0 and Hansen show forest? ----
forest_nonagb <- forest_2000$unmask()$multiply(agb_2000$unmask()$eq(0))
forest_nonagb <- forest_nonagb$updateMask(forest_nonagb$neq(0))

# Count forested pixels by district
ct_nonagb <- forest_nonagb$reduceRegions(
  collection = estonia, # add to feature class
  reducer = ee$Reducer$count(),
  scale = hansen_res_m
)
(ct_nonagb_num <- ct_nonagb$first()$get('count')$getInfo())

# Get carbon stock ----
agb_30m_mgc <- agb_30m_mgc$updateMask(agb_nonforest)

# Map$addLayer(agb_nonforest, list(palette = c('blue', 'orange'))) |
#   Map$addLayer(agb_30m_mgc, list(palette = agb_pal))

# Count forested pixels by district
mgc_nonforest <- agb_30m_mgc$reduceRegions(
  collection = estonia, # add to feature class
  reducer = ee$Reducer$sum(),
  scale = agb_res_m
)
mgc_nonforest_num <- mgc_nonforest$first()$get('sum')$getInfo()

# Get carbon density ----
agb_30m_mgcha <- agb_30m_mgcha$updateMask(agb_nonforest)

# Map$addLayer(agb_nonforest, list(palette = c('blue', 'orange'))) |
#   Map$addLayer(agb_30m_mgc, list(palette = agb_pal))

# Count forested pixels by district
mgcha_nonforest <- agb_30m_mgcha$reduceRegions(
  collection = estonia, # add to feature class
  reducer = ee$Reducer$mean(),
  scale = agb_res_m
)
mgcha_nonforest_num <- mgcha_nonforest$first()$get('mean')$getInfo()

# Back to original programming -----

# 30m Hansen loss year (0 = no loss; 1 - 20 = forest loss in year 2001 - 2018, respectively)
loss_year <- hansen_30m$select(c('lossyear'))

# spatial resolution
hansen_res_m <- loss_year$projection()$nominalScale()
agb_res_m <- agb_30m_mgha$projection()$nominalScale()
agb_res_km <- agb_res_m$divide(1e3)

# convert 30m AGBD (Mg/ha) ca. 2000 to AGC (MgC)
agb_30m_mgcha <- agb_30m_mgha$divide(2)
agb_30m_mgc <- agb_30m_mgcha$
  multiply(agb_30m_mgha$pixelArea()$divide(1e4))$
  rename('agb_2000_mgc')

# build multi-band images, where each band represents either forest area or carbon loss in a given year
loss_area_sqkm <-ee$Image()$select(c())
loss_carbon_mgc <-ee$Image()$select(c())

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
  tmp_carbon <-agb_30m_mgc$
    multiply(tmp_loss_mask)$ # only keep deforested pixels that year
    rename(str_c('carbon_loss_', year_str, '_mgc'))
  loss_carbon_mgc <-loss_carbon_mgc$addBands(tmp_carbon)
}

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
  reducer = ee$Reducer$sum(),
  scale = hansen_res_m
)$map(function(f){ # Rename sum column
  ee$Feature(f$geometry(), list(
    name = f$get('name'),
    div1 = f$get('div1'),
    carbon_2000_mgc = f$get('carbon_2000_mgc'),
    forest_2000_ha = f$get('sum'))
  )
})
# fc_agg$first()$get('forest_2000_ha')$getInfo()
# 
# # Get counts of forested pixels
# fc_2000 <- tc_2000$gt(25)
# fc_2000 <- fc_2000$unmask()$updateMask(fc_2000$eq(1))
# 
# # Count forested pixels by district
# fc_counts <- fc_2000$reduceRegions(
#   collection = fc, # add to feature class 
#   reducer = ee$Reducer$count(),
#   scale = hansen_res_m
# )$map(function(f){ # Rename sum column
#   ee$Feature(f$geometry(), 
#              list(name = f$get('name'), div1 = f$get('div1'),
#                   ct_forest = f$get('count'))
#   )
# })
# 
# # Count non-forested pixels by district
# nonfc_2000 <- tc_2000$lte(25)
# nonfc_2000 <- nonfc_2000$unmask()$updateMask(nonfc_2000$eq(1))
# 
# fc_counts <- nonfc_2000$reduceRegions(
#   collection = fc_counts, # add to feature class 
#   reducer = ee$Reducer$count(),
#   scale = hansen_res_m
# )$map(function(f){ # Rename sum column
#   ee$Feature(f$geometry(), list(name = f$get('name'), div1 = f$get('div1'),
#     ct_forest = f$get('ct_forest'),
#     ct_nonforest = f$get('count'))
#   )
# })
# 
# # Count all pixels by district
# fc_counts <- ee$Image(1)$reduceRegions(
#   collection = fc_counts, # add to feature class 
#   reducer = ee$Reducer$count(),
#   scale = hansen_res_m
# )$map(function(f){ # Rename sum column
#   ee$Feature(f$geometry(), 
#              list(name = f$get('name'), div1 = f$get('div1'),
#                                 ct_forest = f$get('ct_forest'),
#                                 ct_nonforest = f$get('ct_nonforest'),
#                                 ct_all = f$get('count'),
#                   forest_pct = ee$Number(f$get('ct_forest'))$
#                     divide(ee$Number(f$get('count'))))
#   )
# })
# 
# # Get values
# fc_counts$first()$get('ct_forest')$getInfo()
# fc_counts$first()$get('ct_nonforest')$getInfo()
# fc_counts$first()$get('ct_all')$getInfo()
# fc_counts$first()$get('forest_pct')$getInfo()
# fc_counts$first()$get('div1')$getInfo()

# Get total carbon stock ca. 2000 by district ----
agb2000_mgc_masked <- agb_30m_mgc$unmask()$updateMask(agb_30m_mgc$gt(0))
# Map$addLayer(agb_30m_mgc, list(min = 0, max = 50, palette = agb_pal)) |
#   Map$addLayer(agb2000_mgc_masked, list(min = 0, max = 50, palette = agb_pal))

fc_agg <- agb2000_mgc_masked$
  reduceRegions(
  collection = fc_agg, # add to feature class containing all loss results
  reducer = ee$Reducer$sum(),
  scale = agb_res_m
)$map(function(f){ # Rename sum column
  ee$Feature(f$geometry(), list(
    name = f$get('name'), 
    div1 = f$get('div1'),
    forest_2000_ha = f$get('forest_2000_ha'),
    carbon_2000_mgc = f$get('sum'))
  )
})

# Get carbon density ca. 2000 by district ----
agb2000_mgcha_masked <- agb_30m_mgcha$unmask()$updateMask(agb_30m_mgcha$gt(0))
Map$addLayer(agb2000_mgcha_masked, list(min = 0, max = 50, palette = agb_pal))

fc_2000 <- agb2000_mgcha_masked$
  reduceRegions(
    collection = fc_agg, # add to feature class containing all loss results
    reducer = ee$Reducer$mean(),
    scale = agb_res_m
  )$map(function(f){ # Rename sum column
    ee$Feature(f$geometry(), list(
      name = f$get('name'), 
      div1 = f$get('div1'),
      forest_2000_ha = f$get('forest_2000_ha'),
      carbon_2000_mgc = f$get('carbon_2000_mgc'),
      c_dens_2000_mgcha = f$get('mean'))
    )
  })
fc_2000$first()$get('div1')$getInfo()
fc_2000$first()$get('forest_2000_ha')$getInfo()
fc_2000$first()$get('carbon_2000_mgc')$getInfo()
fc_2000$first()$get('c_dens_2000_mgcha')$getInfo()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get total loss values (2000-2020) for forest area and carbon ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Mask AGC with loss band
loss <- hansen_30m$select(c('loss'))$updateMask(loss$eq(1))

# Loss in forest area ----
# Get pixel area with GEE
loss_area <- loss$
  multiply(loss$pixelArea()$divide(1e4))

fc_2000 <- loss_area$reduceRegions(
  collection = fc_2000, # add to feature class containing all loss results
  reducer = ee$Reducer$sum(),
  scale = agb_res_m
)$map(function(f){ # Rename sum column
  ee$Feature(f$geometry(), list(
    name = f$get('name'), 
    div1 = f$get('div1'),
    forest_2000_ha = f$get('forest_2000_ha'),
    carbon_2000_mgc = f$get('carbon_2000_mgc'),
    c_dens_2000_mgcha = f$get('c_dens_2000_mgcha'),
    forest_loss_ha = f$get('sum'))
  )
})

# Loss in carbon stock ----
c_loss <- agb_30m_mgc$
  updateMask(loss$mask())$
  updateMask(agb_30m_mgc$neq(0))
# Map$addLayer(c_loss, list(min = 0, max = 2, palette = viridis)) |
# Map$addLayer(loss, list(min = 0, max = 1, palette = viridis))
# Map$addLayer(agb_30m_mgc, list(min = 0, max = 2, palette = viridis))
# 
# agb_30m_mgc$reduceRegion(
#   reducer = ee$Reducer$percentile(c(0, 50, 100)),
#   geometry = fc$first()$geometry(),
#   bestEffort = TRUE
# )$getInfo()

fc_2000 <- c_loss$reduceRegions(
  collection = fc_2000, # add to feature class containing all loss results
  reducer = ee$Reducer$sum(),
  scale = agb_res_m
)$map(function(f){ # Rename sum column
  ee$Feature(f$geometry(), list(
    name = f$get('name'), 
    div1 = f$get('div1'),
    forest_2000_ha = f$get('forest_2000_ha'),
    carbon_2000_mgc = f$get('carbon_2000_mgc'),
    c_dens_2000_mgcha = f$get('c_dens_2000_mgcha'),
    forest_loss_ha = f$get('forest_loss_ha'),
    c_loss_mgc = f$get('sum'))
  )
})

# Loss in density ----
c_dens_loss <- agb_30m_mgcha$
  updateMask(loss$mask())$
  updateMask(agb_30m_mgcha$neq(0))
# Map$addLayer(c_dens_loss, list(min = 0, max = 65, palette = viridis)) |
# Map$addLayer(loss, list(min = 0, max = 1, palette = viridis))
# Map$addLayer(agb_30m_mgcha, list(min = 0, max = 65, palette = viridis))
# 
# c_dens_loss$reduceRegion(
#   reducer = ee$Reducer$percentile(c(0, 50, 100)),
#   geometry = fc$first()$geometry(),
#   bestEffort = TRUE
# )$getInfo()

fc_2000 <- c_dens_loss$reduceRegions(
  collection = fc_2000, # add to feature class containing all loss results
  reducer = ee$Reducer$mean(),
  scale = agb_res_m
)$map(function(f){ # Rename sum column
  ee$Feature(f$geometry(), list(
    name = f$get('name'), 
    div1 = f$get('div1'),
    forest_2000_ha = f$get('forest_2000_ha'),
    carbon_2000_mgc = f$get('carbon_2000_mgc'),
    c_dens_2000_mgcha = f$get('c_dens_2000_mgcha'),
    forest_loss_ha = f$get('forest_loss_ha'),
    c_loss_mgc = f$get('c_loss_mgc'),
    c_loss_dens = f$get('mean'))
  )
})

fc_2000$first()$propertyNames()$getInfo()

# Get polygon areas ----
fc_2000 <- fc_2000$map(function(f) f$set(list(area_ha = f$area()$divide(1e4))))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sum forest area and carbon each year by region ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# summarize annual forest area loss by district
loss_fc <- loss_area_sqkm$reduceRegions(
  collection = fc_2000, # adds columns forest_loss_[year]_ha
  reducer = ee$Reducer$sum(),
  scale = hansen_res_m
)

# summarize annual forest carbon loss by district
loss_fc_carbon <- loss_carbon_mgc$reduceRegions(
  collection = loss_fc, # add columns carbon_loss_[year]_mgc to FC with area loss results
  reducer = ee$Reducer$sum(),
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
ee_monitoring(task_vector, quiet = TRUE) # optional
# local_fp <- ee_drive_to_local(task_vector, 
#                   dsn = file.path('outputs', str_c(task_name, '.geojson')), 
#                   overwrite = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Use Hansens's 2000-2020 loss to mask our 30m AGC ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# # Paint all the polygon edges with the same number and width, display.
# outline <- ee$Image()$byte()$
#   paint(featureCollection = fc, color = 1, width = 2)
# polys_lyr <- Map$addLayer(outline, name = 'HIH sites')
# 
# # Mask AGC with loss band
# loss <- hansen_30m$select(c('loss'))
# loss <- loss$updateMask(loss$neq(1))
# agb2020_fromloss <- agb_30m_mgc$
#   updateMask(loss$mask())$
#   updateMask(agb_30m_mgc$neq(0))
# 
# # View
# Map$addLayer(eeObject = loss_year, 
#              visParams = list(min = 0, max = 20, palette = viridis), 
#              name = 'Hansen loss year') +
#   Map$addLayer(eeObject = fc_2000, 
#                visParams = list(min = 0, max = 1, palette = viridis), 
#                name = 'Forest') +
#   Map$addLayer(eeObject = agb_30m_mgc$updateMask(agb_30m_mgc$neq(0)), 
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
