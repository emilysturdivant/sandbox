#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Script to:
#     * Prep layers for FCI
# Requires:
#     * 
# Author:
#     * esturdivant@woodwellclimate.org, 2021-09-30
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(sf)
library(terra)
library(tmap)
tmap_mode('view')
library(tidyverse)
library(countrycode)
library(rgee)
ee_Initialize()

# Initialize ----
data_dir <- '~/data'

# Prep to get paths to assets
user <- ee_get_assethome()
addm <- function(x) sprintf("%s/%s", user, x)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EPI ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Downloaded from https://sedac.ciesin.columbia.edu/data/set/epi-environmental-performance-index-2020/data-download
epi_xls <- here::here(data_dir, 'raw_data', 'sociopolitical', 'epi', 
                      '2020-epi-xlsx', '2020-epi.xlsx')

# Load all data
epi_all <- readxl::read_excel(epi_xls, sheet = '3_EPI_Results', 
                              col_names = TRUE, na = 'NA',
                              .name_repair = ~ str_replace_all(.x, '\\.', '_')) %>% 
  mutate(FIPS = countrycode(iso, origin = 'iso3c', destination = 'fips'),
         GAUL = countrycode(iso, origin = 'iso3c', destination = 'gaul')) 

# Look
epi %>% filter(!is.na(EPI.new)) %>% count(iso, EPI.new) %>% arrange(desc(n))

# Large Scale International Boundaries ----
# 'country_co' FIPS: wikipedia.org/wiki/List_of_FIPS_country_codes
countries <- ee$FeatureCollection("USDOS/LSIB_SIMPLE/2017")
epi_fc <- ee$FeatureCollection(list()) # Empty table
for (row in 1:nrow(epi)) {
  
  # Get values for the given country: FIPS code and values
  code <- epi_all[[row, "FIPS"]]
  score  <- epi_all %>% 
    slice(row) %>% 
    select(EPI_new:HLT_new) %>% # Doesn't work with all the columns, maybe because of NAs?
    as.list()
  
  # Filter FC to the country
  fc <- countries$filter(ee$Filter$eq('country_co', code))
  
  # Set the properties for every feature from the values in the input table
  fc <- fc$map( function(f){ f$set(score) })$
    copyProperties(countries)
  
  # Append country FC to output FC
  epi_fc <- epi_fc$merge(fc)
}
epi_fc$first()$get('HLT_new')$getInfo()
Map$addLayer(epi_fc, list(), name='GCI')

# Save FC
task_vector <- ee_table_to_asset(epi_fc, assetId = addm('EPI_2020_LSIB'))
task_vector$start()

# Convert to raster
epi_ic <- epi_fc$
  reduceToImage(
    properties = list('EPI_new'),
    reducer = ee$Reducer$first()
  )
Map$addLayer(epi_ic, list(), name='EPI')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# World Bank ESG data ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Downloaded from https://datacatalog.worldbank.org/search/dataset/0037651/Environment,%20Social%20and%20Governance%20Data
esg_dir <- here::here(data_dir, 'raw_data', 'sociopolitical', 'worldbank', 
                      'WB_EnvSocGov_data')

# Load and look
countries_csv <- here::here(esg_dir, 'ESGCountry.csv')
esg_countries <- read_csv(countries_csv)
esg_countries %>% tbl_vars()
esg_countries %>% distinct(`Latest agricultural census`)

# Load and look
series_csv <- here::here(esg_dir, 'ESGSeries.csv')
esg_series <- read_csv(series_csv)
esg_series %>% tbl_vars()
esg_series %>% distinct(Topic)
esg_series %>% group_by(Topic) %>% distinct(`Indicator Name`) %>% 
  filter(str_detect(Topic, regex('governance: gender', ignore_case = TRUE)))
t <- esg_series %>% 
  filter(str_detect(`Indicator Name`, regex('school enrollment', ignore_case = TRUE))) 
t <- esg_series %>% distinct(Source)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Subnational HDI (rasterize) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gdl_dir <- here::here(data_dir, 'raw_data', 'sociopolitical', 'GDL_2019')

# Downloaded from https://globaldatalab.org/shdi/shapefiles/
shp <- here::here(gdl_dir, 'GDL Shapefiles V4', 'GDL_Shapefiles_V4.shp')

# Rasterize to 4x LBII resolution ----
shdi_v <- terra::vect(shp)
lbii_tif <- file.path(data_dir, 'biodiversity/lbii_from_ascii.tif')
lbii_r <- terra::rast(lbii_tif)
temp_r <- aggregate(lbii_r, 4)
shdi_r <- terra::rasterize(shdi_v, temp_r, field = 'shdi', 
                 fun = function(x) min(x, na.rm = TRUE))

shdi_tif <- here::here(data_dir, 'sociopolitical', 'shdi_2arcmin.tif')
shdi_r %>% writeRaster(shdi_tif, datatype = 'FLT4S', overwrite = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Worldwide Governance Indicators ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wgi_xls <- here::here(data_dir, 'raw_data', 'sociopolitical', 'governance_wgi', 
                      'wgidataset.xlsx')

# Load all data
wgi_all <- readxl::read_excel(wgi_xls, sheet = 2, col_names = FALSE, skip = 13,
                              na = '#N/A')
colnames(wgi_all) <- str_c(wgi_all[2, 3:ncol(wgi_all)], 
                           wgi_all[1, 3:ncol(wgi_all)], collapse = '_')

wgi_all %>% 
  pivot_longer(cols = 3:134)

# Select 2020 estimate
wgi <- wgi_all %>% 
  select(Attribute:ZWE) %>%
  filter(`Series Global ID` == 'GCI4',
         Edition == '2019') %>% 
  pivot_longer(cols = AGO:ZWE,
               names_to = 'COUNTRY_CODE') %>% 
  pivot_wider(names_from = Attribute, 
              names_repair = 'universal') %>% 
  mutate(across(VALUE:SCORE, ~ as.numeric(.x)), 
         SOURCE.DATE = as.numeric(SOURCE.DATE),
         FIPS = countrycode(COUNTRY_CODE, origin = 'wb', 
                            destination = 'fips'),
         GAUL = countrycode(COUNTRY_CODE, origin = 'wb', 
                            destination = 'gaul')) 

gci_vals <- gci %>% select(COUNTRY_CODE, COUNTRY_NAME, FIPS, GAUL, RANK, SCORE)

# Upload to asset
gci_csv <- here::here(data_dir, 'sociopolitical', 'wef_gci', 'GCI4_2019.csv')
gci_vals %>% write_csv(gci_csv)

# Large Scale International Boundaries ----
# 'country_co' FIPS: wikipedia.org/wiki/List_of_FIPS_country_codes
countries <- ee$FeatureCollection("USDOS/LSIB_SIMPLE/2017")
data <- ee$FeatureCollection(list()) # Empty table
for (row in 1:nrow(gci_vals)) {
  
  code <- gci_vals[[row, "FIPS"]]
  score  <- gci_vals[[row, "SCORE"]]
  
  fc <- countries$filter(ee$Filter$eq('country_co', code))
  fc <- fc$map(function(f) {
    f$set(list(gci = score))
  })$
    copyProperties(countries)
  
  data <- data$merge(fc)
}
gci_ee2 <- data
gci_ee2$first()$get('gci')$getInfo()
Map$addLayer(gci_ee2, list(), name='GCI')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Global Competitiveness Index ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gci_xls <- here::here(data_dir, 'raw_data', 'sociopolitical', 'wef_gci', 
                      'WEF_GCI_4.0_2019_Dataset.xlsx')

# Load all data
gci_all <- readxl::read_excel(gci_xls, sheet = 'Data', col_names = FALSE, skip = 2)
names <- c(unname(as_vector(gci_all[2, 1:10])), 
           unname(as_vector(gci_all[1, 11:ncol(gci_all)])))
colnames(gci_all) <- names
gci_all[2, 1:10] <- NA
gci_all$`Series Global ID`[2] <- 'GCI4'
gci_all$Attribute[2] <- 'COUNTRY_NAME'
gci_all$Edition[2] <- '2019'

# Filter to GCI 
gci <- gci_all %>% 
  filter(`Series Global ID` == 'GCI4',
         Edition == '2019') %>% 
  select(Attribute:ZWE) %>%
  pivot_longer(cols = AGO:ZWE,
               names_to = 'COUNTRY_CODE') %>% 
  pivot_wider(names_from = Attribute, 
              names_repair = 'universal') %>% 
  mutate(across(VALUE:SCORE, ~ as.numeric(.x)), 
         SOURCE.DATE = as.numeric(SOURCE.DATE),
         FIPS = countrycode(COUNTRY_CODE, origin = 'wb', 
                                 destination = 'fips'),
         GAUL = countrycode(COUNTRY_CODE, origin = 'wb', 
                                 destination = 'gaul')) 

gci_vals <- gci %>% select(COUNTRY_CODE, COUNTRY_NAME, FIPS, GAUL, RANK, SCORE)

# Upload to asset
gci_csv <- here::here(data_dir, 'sociopolitical', 'wef_gci', 'GCI4_2019.csv')
gci_vals %>% write_csv(gci_csv)

# Large Scale International Boundaries ----
# 'country_co' FIPS: wikipedia.org/wiki/List_of_FIPS_country_codes
countries <- ee$FeatureCollection("USDOS/LSIB_SIMPLE/2017")
data <- ee$FeatureCollection(list()) # Empty table
for (row in 1:nrow(gci_vals)) {
  
  code <- gci_vals[[row, "FIPS"]]
  score  <- gci_vals[[row, "SCORE"]]
  
  fc <- countries$filter(ee$Filter$eq('country_co', code))
  fc <- fc$map(function(f) {
    f$set(list(gci = score))
  })$
    copyProperties(countries)
  
  data <- data$merge(fc)
}
gci_ee2 <- data
gci_ee2$first()$get('gci')$getInfo()
Map$addLayer(gci_ee2, list(), name='GCI')

# This doesn't work: 
countries <- countries$map(function(f) {
  
  lsib_code <- f$get('country_co')$getInfo()
  gci_row <- gci_vals %>% filter(FIPS == lsib_code)
  
  if(nrow(gci_row) == 1){
    f$set('gci', gci_row$SCORE)
  } else {
    f$set('gci', NULL)
  }
  
})

# Option 3: for-loop (WATCH OUT!)
size <- countries$size()
print(size$getInfo()) # 312
countriesList <- countries$toList(1) # Adjust size.

for (j in (seq_len(countriesList$length()$getInfo()) - 1)) {
  feature <- ee$Feature(countriesList$get(j))
  # Convert ImageCollection > FeatureCollection
  fc <- ee$FeatureCollection(
    imagery$map(
      function(image) {
        ee$Feature(
          feature$geometry()$centroid(100),
          image$reduceRegion(
            reducer = ee$Reducer$mean(),
            geometry = feature$geometry(),
            scale = 500
          )
        )$set(
          list(
            time = image$date()$millis(),
            date = image$date()$format()
          )
        )$copyProperties(feature)
      }
    )
  )
  data <- data$merge(fc)
}
print(data$first()$getInfo())
# n <- countries$size()$getInfo()
# for (c in 1:n) {
dict <- countries$first()$toDictionary(list('country_co'))
dict$getInfo()
ee_print(dict)



# It worked!
f_list <- list()
for (row in 1:nrow(gci_vals)) {
  
  code <- gci_vals[[row, "FIPS"]]
  score  <- gci_vals[[row, "SCORE"]]
  rank  <- gci_vals[[row, "RANK"]]
  
  f <- countries$filter(ee$Filter$eq('country_co', code))$geometry()
  f <- ee$Feature(f,
    list(gci = score,
         gci_rank = rank,
         fips = code))
  
  f_list[row] <- c(f)
}
gci_ee <- ee$FeatureCollection(f_list)
gci_ee$first()$get('fips')$getInfo()

# Save FC
task_vector <- ee_table_to_asset(gci_ee, assetId = addm('WEF_GCI4_2019'))
task_vector$start()

# Convert to raster
gci_ic <- gci_ee$
  reduceToImage(
    properties = list('gci'),
    reducer = ee$Reducer$first()
  )
Map$addLayer(gci_ic, list(), name='GCI')



# Option 3: for-loop (WATCH OUT!)
size <- countriesTable$size()
print(size$getInfo()) # 312
countriesList <- countriesTable$toList(1) # Adjust size.
data <- ee$FeatureCollection(list()) # Empty table.
for (j in (seq_len(countriesList$length()$getInfo()) - 1)) {
  feature <- ee$Feature(countriesList$get(j))
  # Convert ImageCollection > FeatureCollection
  fc <- ee$FeatureCollection(
    imagery$map(
      function(image) {
        ee$Feature(
          feature$geometry()$centroid(100),
          image$reduceRegion(
            reducer = ee$Reducer$mean(),
            geometry = feature$geometry(),
            scale = 500
          )
        )$set(
          list(
            time = image$date()$millis(),
            date = image$date()$format()
          )
        )$copyProperties(feature)
      }
    )
  )
  data <- data$merge(fc)
}
print(data$first()$getInfo())



# GAUL ----
countries <- ee$FeatureCollection("FAO/GAUL_SIMPLIFIED_500m/2015/level0")
f <- countries$first()
f$get('ADM0_NAME')$getInfo()
f$get('gci')$getInfo()
countries$get('ADM0_NAME')
# Codes at: https://www.fao.org/in-action/countrystat/news-and-events/events/training-material/gaul-codes2014/en/

gci_country_list <- gci_vals %>% distinct(ISO) %>% deframe()
countrycode::countrycode("Congo, Democratic Rep.", origin = 'country.name', 
                         destination = 'gaul')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Subnational HDI (simplify) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gdl_dir <- here::here(data_dir, 'raw_data', 'sociopolitical', 'GDL_2019')

# Downloaded from https://globaldatalab.org/shdi/shapefiles/
shp <- here::here(gdl_dir, 'GDL Shapefiles V4', 'GDL_Shapefiles_V4.shp')

# Simplify and subset to those that intersect tropics
sf <- st_read(shp)
sfs <- sf %>% st_simplify(dTolerance = 0.01)
sf.st <- sfs[unlist(st_intersects(tropics_26, sfs)),]

tm_shape(sfs) + tm_polygons(col = 'shdi')


cts <- sf %>% st_drop_geometry() %>% 
  drop_na(shdi) %>% 
  distinct(country)

# # Load other indicators
# csv <- file.path(gdl_dir, 'GDL-Life-expectancy-data.csv')
# le <- read_csv(csv) %>% 
#   select(GDLcode = GDLCODE,
#          LE = `2019`)
# 
# csv <- file.path(gdl_dir, 'GDL-Health-index-data.csv')
# hi <- read_csv(csv) %>% 
#   select(GDLcode = GDLCODE,
#          HI = `2019`)

# Join to GDL subnational units ----
sf.le <- sfs %>% left_join(le, by = 'GDLcode')
sf.ind <- sf.le %>% left_join(hi, by = 'GDLcode')

tm_shape(sf.ind) + tm_polygons(col = 'shdi', lwd = NA)
tm_shape(sf.ind) + tm_polygons(col = 'LE', lwd = NA)
tm_shape(sf.ind) + tm_polygons(col = 'HI', lwd = NA)

sf.ind %>% st_write(file.path(gdl_dir, 'GDL_subnational_hdi_le_hi.shp'), 
                    append = FALSE)

# sf.ind %>% sf_as_ee('getInfo_to_asset', addm('GDL_subnational_hdi_le_hi'))
# ee_monitoring()







# Create tropics extent ----
e <- ext(c(xmin = -180, xmax = 180, ymin = -23.3, ymax = 23.3))
bb <- c(e$xmin, e$ymin, e$xmax, e$ymax)
tropics_rect <- st_as_sfc(st_bbox(bb, crs = st_crs(4326)))

tropics_rect_shp <- file.path(data_dir, 'context', 'tropics_rect.shp')
if(!file.exists(tropics_rect_shp)) {
  tropics_rect %>% st_write(tropics_rect_shp)
}

# Create tropics extent ----
e <- ext(c(xmin = -180, xmax = 180, ymin = -26, ymax = 26))
bb <- c(e$xmin, e$ymin, e$xmax, e$ymax)
tropics_26 <- st_as_sfc(st_bbox(bb, crs = st_crs(4326)))

# Extract countries in tropics and add MSF flag ----
standardize_text <- function(x){
  x %>% as.character() %>% 
    str_trim() %>% 
    str_to_upper() %>% 
    stringi::stri_trans_general(str=., id='Latin-ASCII') %>% 
    str_replace_all(' +', ' ') %>% 
    str_replace_all('(, )+', ', ') %>% 
    str_replace_all('( ,)+', ',') %>% 
    str_remove_all('^,') %>% 
    str_trim()
}

# Load GADM country boundaries as singlepart
gadm_shp <- file.path(data_dir, 'gadm', 'gadm36_0.shp')
countries <- st_read(gadm_shp, promote_to_multi = TRUE) %>% 
  st_cast("POLYGON")

# Simplify and subset to those that intersect tropics
countries <- countries %>% st_simplify(dTolerance = 0.01)
countries <- countries[unlist(st_intersects(tropics_rect, countries)),]

# Remove parts of countries < 2km2
countries['area'] <- countries %>% st_area()
countries <- countries %>% 
  filter(area > units::set_units(9, 'km^2')) %>% 
  group_by(NAME_0) %>% 
  summarize()

# Extract Protected Areas in the tropics ----
# Protected area polygons ----
pa_zips <- list.files(
  file.path(data_dir, 'protected_areas', 'WDPA_Oct2021_Public_shp'),
  pattern = ".zip$", full.names=TRUE, recursive = TRUE)
# 
# # Unzip
# unzip_and_filter_pa_polygons <- function(z) {
#   # z <- pa_zips[[1]]
#   
#   # Unzip to temp dir
#   miao <- tempfile()
#   unzip(z, exdir = miao)
#   
#   # Load shapefile (polygons)
#   (shp_fp <- list.files(miao, pattern = ".shp$", full.names=TRUE, recursive = TRUE))
#   pa <- st_read(shp_fp[[2]])
#   
#   # Filter
#   pa <- pa %>% 
#     filter(MARINE != 2) %>% 
#     st_simplify(dTolerance = 0.001) 
#   
#   # subset polygons to those that intersect tropics rectangle
#   pa_tropics <- pa[unlist(st_intersects(tropics_rect, pa)),]
#   
#   return(pa_tropics)
# }
# 
# pa_tropics <- pa_zips %>% purrr::map_dfr(unzip_and_filter_pa_polygons)
# pa_tropics %>% 
#   st_write(file.path(data_dir, 'protected_areas', 'WDPA_Oct2021_polygons_tropics_simp001.gpkg'))
# 
# # Protected area points ----
# # pa_zip <- file.path(data_dir, 'protected_areas', 'WDPA_Oct2021_Public_shp', 'WDPA_Oct2021_Public_shp.zip')
# pa_zips <- list.files(
#   file.path(data_dir, 'protected_areas', 'WDPA_Oct2021_Public_shp'),
#   pattern = ".zip$", full.names=TRUE, recursive = TRUE)
# 
# # Unzip
# unzip_and_filter_pa_points <- function(z) {
#   # z <- pa_zips[[1]]
#   
#   # Unzip to temp dir
#   miao <- tempfile()
#   unzip(z, exdir = miao)
#   
#   # Load shapefile (polygons)
#   (shp_fp <- list.files(miao, pattern = "points\\.shp$", full.names=TRUE, recursive = TRUE))
#   pa <- st_read(shp_fp[[1]])
#   
#   pa <- pa[unlist(st_intersects(tropics_rect, pa)),]
#   pa <- pa %>% filter(REP_AREA > 0, MARINE != 2)
#   pa_buff <- pa %>% st_buffer(8.5*0.001)
#   
#   return(pa_buff)
# }
# 
# pa_tropics <- pa_zips %>% purrr::map_dfr(unzip_and_filter_pa_points)
# pa_tropics %>% 
#   st_write(file.path(data_dir, 'protected_areas', 'WDPA_Oct2021_points_tropics_buff0085.gpkg'))
# 
# # Merge
# pa_polys <- st_read(file.path(data_dir, 'protected_areas', 'WDPA_Oct2021_polygons_tropics_simp001.gpkg'))
# pa_tropics2 <- bind_rows(pa_polys, pa_tropics)
# 
# pa_tropics2 %>% object.size() %>% print(units = "MB")
# pa_tropics2 %>% 
#   st_write(file.path(data_dir, 'protected_areas', 'WDPA_Oct2021_tropics_simp001_buff0085.gpkg'))

# OECM polygons ----
# pa_zip <- file.path(data_dir, 'protected_areas', 'WDPA_Oct2021_Public_shp', 'WDPA_Oct2021_Public_shp.zip')
pa_zips <- list.files(
  file.path(data_dir, 'protected_areas', 'WDOECM_Oct2021_Public_shp'),
  pattern = ".zip$", full.names=TRUE, recursive = TRUE)

# Unzip
unzip_and_filter_pas <- function(z) {
  # z <- pa_zips[[1]]
  
  # Unzip to temp dir
  miao <- tempfile()
  unzip(z, exdir = miao)
  
  # Load shapefile (polygons)
  (shp_fp <- list.files(miao, pattern = "polygons\\.shp$", full.names=TRUE, recursive = TRUE))
  pa <- st_read(shp_fp[[1]])
  
  # Filter
  pa <- pa %>% 
    filter(MARINE != 2) %>% 
    st_simplify(dTolerance = 0.001) 
  
  # subset polygons to those that intersect tropics rectangle
  pa_polys <- pa[unlist(st_intersects(tropics_rect, pa)),]
  
  # Load points
  (shp_fp <- list.files(miao, pattern = "points\\.shp$", full.names=TRUE, recursive = TRUE))
  pa <- st_read(shp_fp[[1]])
  pa <- pa[unlist(st_intersects(tropics_rect, pa)),]
  pa <- pa %>% filter(REP_AREA > 0, MARINE != 2)
  pa_points <- pa %>% st_buffer(8.5*0.001)
  
  # Combine
  pa_tropics <- bind_rows(pa_polys, pa_points)
  
  # Return
  return(pa_tropics)
}

pa_tropics <- pa_zips %>% purrr::map_dfr(unzip_and_filter_pas)
tm_shape(pa_tropics) + tm_polygons()
pa_tropics %>% 
  st_write(file.path(data_dir, 'protected_areas', 'WDOECM_Oct2021_tropics_simp001_buff0085.gpkg'))


# Intact Forest Landscape ----
in_dir <- file.path(data_dir, 'forests', 'IFL_2016')
out_dir <- file.path(data_dir, 'forests', 'ifl_2016_tropics')
ifl_shp <- list.files(in_dir, pattern = ".shp$", full.names=TRUE, recursive = TRUE)

# Unzip
subset_to_tropics <- function(shp_fp) {
  # shp_fp <- gsn_shps[[1]]
  
  # Load data
  pa <- st_read(shp_fp)
  
  # Filter
  pa <- pa %>% 
    st_simplify(dTolerance = 0.001) 
  
  # subset polygons to those that intersect tropics rectangle
  pa_polys <- pa[unlist(st_intersects(tropics_rect, pa)),]
  
  (fn <- str_split(shp_fp, '/') %>% last() %>% last())
  pa_polys %>% 
    st_write(out_dir, str_c(tools::file_path_sans_ext(fn), '_simp001.gpkg'))
}

ifl_shp[[1]] %>% purrr::walk(subset_to_tropics)

# FLII ----
out_dir <- file.path(data_dir, 'forests', 'flii_tropics2')
(flii_tifs <- list.files(file.path(data_dir, 'forests', 'flii'), 
                         pattern = 'flii_[^earth].*\\.tif$', 
                         full.names = TRUE))

crop_and_agg <- function(fp) {
  # Get output name
  fn_ac <- file.path(out_dir, str_c(basename(tools::file_path_sans_ext(fp)), '_agg3.tif'))
  
  # Load raster
  r <- terra::rast(fp)
  
  # Get intersection of the bounding boxes of the two rasters 
  # (didn't work with terra::intersect)
  r_bbox <- terra::ext(r) %>% as.vector() %>% sf::st_bbox() %>% sf::st_as_sfc()
  e_bbox <- e %>% as.vector() %>% sf::st_bbox() %>% sf::st_as_sfc()
  bb <- sf::st_intersection(r_bbox, e_bbox)
  if(length(bb) == 0) {
    print('Input raster does not include tropics.')
    return()
  }
  
  bb <- bb %>% sf::st_bbox() %>% as.vector()
  
  # Convert to terra extent object
  bbex <- terra::ext(bb[c(1, 3, 2, 4)])
  
  # Crop
  NAflag(r) <- -9999
  r <- terra::crop(r, bbex)
  
  # Aggregate and save
  terra::aggregate(r, fact = 3, na.rm = TRUE, filename = fn_ac, overwrite = TRUE, 
                   datatype = 'INT2U')
}

flii_tifs %>% purrr::walk(crop_and_agg)

# gdalUtils::gdalwarp(srcfile = fp, 
#                     dstfile = fn_ac, 
#                     te = bb, 
#                     tr = c(xres(flii), yres(flii)), 
#                     tap = TRUE, 
#                     overwrite = TRUE)

# Mosaic resulting rasters
in_dir <- file.path(data_dir, 'forests', 'flii_tropics2')
out_dir <- file.path(data_dir, 'forests', 'flii_tropics3')
(flii_tifs <- list.files(in_dir, 
                         pattern = 'flii_[^earth].*\\.tif$', 
                         full.names = TRUE))

gdalUtils::mosaic_rasters(flii_tifs, file.path(out_dir, 'flii_tropics_agg3.tif'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Human modification ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
in_dir <- file.path(data_dir, 'human_influence', 'gHM')
out_dir <- in_dir
(tifs <- list.files(in_dir, pattern = 'gHM\\.tif$', full.names = TRUE))
fp <- tifs[[1]]

# Get output name
fn_ac <- file.path(out_dir, str_c(basename(tools::file_path_sans_ext(fp)), '_wgs_tropics.tif'))

# Load raster
r <- terra::rast(fp)

# Reproject from Mollweide to WGS84
crs(r)
r_p <- terra::project(r, "epsg:4326")

# Get intersection of the bounding boxes of the two rasters
r_bbox <- terra::ext(r_p) %>% as.vector() %>% sf::st_bbox() %>% sf::st_as_sfc()
e_bbox <- e %>% as.vector() %>% sf::st_bbox() %>% sf::st_as_sfc()
bb <- sf::st_intersection(r_bbox, e_bbox)
if(length(bb) == 0) {
  print('Input raster does not include tropics.')
  return()
}

# Convert to terra extent object
bb <- bb %>% sf::st_bbox() %>% as.vector()
bbex <- terra::ext(bb[c(1, 3, 2, 4)])

# Crop and save
terra::crop(r_p, bbex, filename = fn_ac, overwrite = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Development threat ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dpi_dir <- here::here(data_dir, 'Dev_Threat_Index', 'dev_potential_indices_2016')
fps <- list.files(dpi_dir, 'lulc.*dpi_g.*\\.tif$', full.names = TRUE, recursive = TRUE)

# Reduce file
(fp <- fps[[1]])
fp_out <- here::here(dpi_dir, 
                     fp %>% 
                       str_remove_all('.*lulc-development-potential-indices_') %>% 
                       str_replace_all('_geographic', '_geo_int'))

r <- terra::rast(fp)
{r * 100} %>% terra::writeRaster(fp_out, datatype = 'INT1U', overwrite = TRUE)



# fps <- list.files(dpi_dir, '*dpi_g.*\\.tif$', full.names = TRUE, recursive = TRUE)
# (fp <- fps[[2]])
# r <- terra::rast(fp)
# r %>% terra::writeRaster(
#   "/Users/emilysturdivant/data/Dev_Threat_Index/dev_potential_indices_2016/convgas_dpi_geo_int.tif",
#   datatype = 'INT1U', overwrite = TRUE)
# 
# 
# # Unzip to temp dir
# miao <- tempfile()
# unzip(z, exdir = miao)
# 
# # Load shapefile (polygons)
# (shp_fp <- list.files(miao, pattern = "polygons\\.shp$", full.names=TRUE, recursive = TRUE))
# pa <- st_read(shp_fp[[1]])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Spatial database of planted trees ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sdpt_zip <- file.path(data_dir, 'raw_data/biodiversity/plantations_v1_3_dl.gdb.zip')
sdpt <- download.file('http://gfw-files.s3.amazonaws.com/plantations/final/global/plantations_v1_3_dl.gdb.zip', 
                      destfile = sdpt_zip)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Local biodiversity intactness index ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lbii_zip <- file.path(data_dir, 'raw_data/biodiversity/lbii_2005.zip')
lbii <- download.file('http://data.nhm.ac.uk/resources/ab6817846d8d4ff8214ba8a7d6a707e0-16689-1636052089.zip', 
                      destfile = lbii_zip)

# Unzip to temp dir
miao <- tempfile()
unzip(lbii_zip, exdir = miao)

# List extracted files
(fp1 <- list.files(miao, full.names=TRUE, recursive = TRUE))

# Unzip to temp dir
miao2 <- tempfile()
unzip(fp1, exdir = miao2)

# List extracted files
(lbii_asc <- list.files(miao2, full.names=TRUE, recursive = TRUE))

# Load
lbii <- rast(lbii_asc)

lbii_tif <- file.path(data_dir, 'biodiversity/lbii_2005.tif')
lbii %>% writeRaster(lbii_tif)

# BII from https://data.nhm.ac.uk/dataset/bii-bte/resource/94be0af6-ec90-4b83-8f02-64a4983e1ca1 ----
bii_rds <- list.files(file.path(data_dir, 'biodiversity'), 'rds$', full.names = TRUE)
bii_dat <- readRDS(bii_rds)

# Download area_codes.json (from link sent after I requested the data)
dl_url <- "http://data.nhm.ac.uk/resources/1a2475f13eb08757aaf434016b425cb8-16689-1636566334.zip"
local_zip_fp <- file.path(data_dir, 'raw_data/biodiversity/bii/area_code.zip')
download.file(dl_url, destfile = local_zip_fp)

# Unzip to temp dir
miao <- tempfile()
unzip(local_zip_fp, exdir = miao)

# List extracted files
(fp1 <- list.files(miao, full.names=TRUE, recursive = TRUE))

# Load
area_codes <- rjson::fromJSON(fp1)
area_codes_json <- file.path(data_dir, 'raw_data/biodiversity/bii/area_code.json')
area_codes <- rjson::fromJSON(area_codes_json)

lbii_tif <- file.path(data_dir, 'biodiversity/lbii_2005.tif')
lbii %>% writeRaster(lbii_tif)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Biomes ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load biomes
biomes_shp <- here::here(data_dir, 'raw_data', 'Ecoregions2017', 'Ecoregions2017.shp')
biomes_dhf <- st_read(biomes_shp) %>% filter(BIOME_NUM == 1)
biomes_dhf <- biomes %>% st_make_valid()

biomes_dhf_shp <- here::here(data_dir, 'raw_data', 'Ecoregions2017', 'Ecoregions2017_DHF.shp')
biomes_dhf %>% st_write(biomes_dhf_shp, append = FALSE)

bdhf_sub2 <- biomes_dhf %>% filter(OBJECTID %in% c(14, 519)) %>% st_make_valid()
bdhf_sub2 <- bdhf_sub2 %>% st_buffer(0.0001) %>% st_buffer(-0.0001)
bdhf_sub2_shp <- here::here(data_dir, 'ecoregions', 'Ecoregions2017_DHF_sub2.shp')
bdhf_sub2 %>% st_write(bdhf_sub2_shp, append = FALSE)

bdhf_sub2 <- st_read(bdhf_sub2_shp)
bdhf_sub1 <- st_read(here::here(data_dir, 'ecoregions', 
                                'Ecoregions2017_DHF_dissolved1.shp'))

bdhf <- bind_rows(bdhf_sub1, bdhf_sub2)
bdhf_combo3_shp <- here::here(data_dir, 'ecoregions', 'Ecoregions2017_DHF_combo3.shp')
bdhf %>% st_write(bdhf_combo3_shp, append = FALSE)

bdhf_simp <- st_read(here::here(data_dir, 'ecoregions', 
                                'Ecoregions2017_DHF_combo3_simp.1.shp'))
bdhf_union <- bdhf_simp %>% st_union()
bdhf_noholes <- bdhf_union %>% nngeo::st_remove_holes(max_area = 0.5)
bdhf_noholes_shp <- here::here(data_dir, 'ecoregions', 'Ecoregions2017_DHF_combo3_simp.1_noholes.shp')
bdhf_noholes %>% st_write(bdhf_noholes_shp, append = FALSE)
qtm(bdhf_noholes)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BasinATLAS (from HydroSHEDS) ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lev12_shp <- list.files(here::here('/Volumes/STORAGE/work_Woodwell/raw_data',
                               '/HydroSHEDS/BasinATLAS_Data_v10_shp/BasinATLAS_v10_shp'), 
                    'lev12\\.shp', full.names = TRUE)

lev12 <- st_read(lev12_shp)






