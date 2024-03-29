
library(tidyverse)
library(sf)
library(terra)
library(units)
library(stars)

# Copy data from storage bucket ---- 
# "sudo gsutil -m cp -r gs://ejs-data/raw_inputs/Walker_etal_2022/ \
# ~/repos/sandbox/hih/data/Walker_etal_2022/"

# sudo gsutil -m cp -r gs://ejs-data/processed_hansen/MBF \
# ~/repos/sandbox/hih/data/from_gee_500m/

# sudo gsutil -m cp -r gs://ejs-data/processed_hansen/MBF/forest2022_buff2km* \
# ~/repos/sandbox/hih/data/from_gee_500m/

# sudo gsutil -m cp -r gs://ejs-data/processed_hansen/MBF/forest2022_buff2km_500m_ITs_inMBF* \
# ~/repos/sandbox/hih/data/from_gee_500m/

# sudo gsutil -m cp -r gs://ejs-data/processed_hansen/MBF/Pop_2020_cons_unadj_500m* \
# ~/repos/sandbox/hih/data/from_gee_500m/

# Make VRTs (run commands in terminal) ----
# gdalbuildvrt ~/repos/sandbox/hih/data/from_gee_500m/MBF/alltiles.vrt ~/repos/sandbox/hih/data/from_gee_500m/MBF/*.tif
# gdalbuildvrt tropics.vrt *.tif
# gdalbuildvrt forest2022_tropics.vrt forest2022*.tif
# gdalbuildvrt ~/repos/sandbox/hih/data/from_gee_500m/MBF/forest2022_tropics.vrt \
# ~/repos/sandbox/hih/data/from_gee_500m/MBF/forest2022*.tif


out_dir <- here::here('hih/data') 

# Walker AGB
agb_tif <- here::here(out_dir, 'Walker_etal_2022/Base_Cur_AGB_MgCha_500m.tif')
agbm_fp <- here::here(out_dir, 'Base_Cur_AGB_MgCha_500m_forest_MBF.tif')
agbm_fpiplc <- here::here(out_dir, 'Base_Cur_AGB_MgCha_500m_forest_MBF_IPLC.tif')
agbm_fpit <- here::here(out_dir, 'Base_Cur_AGB_MgCha_500m_forest_MBF_ITs.tif')
agb_biome_fp <- here::here(out_dir, 'Base_Cur_AGB_MgCha_500m_MBF.tif')

# Biome
dhf_diss_fp <- here::here(out_dir, 'Ecoregions2017_MBF_realms.geojson')

iplcs_fp <- here::here('hih/data/IPLCs_RRI_tropics.shp')
its_fp <- here::here('hih/data/ITs_inMBF.shp')

# Forest
hansen_dir <- here::here(out_dir, 'from_gee_500m/MBF')
forest_tif <- here::here(hansen_dir, 'forest2022_tropics.tif')
fmask_fp <- here::here(out_dir, 'forestmask_500m.tif')
fmask_biome_fp <- here::here(out_dir, 'forestmask_500m_MBF.tif')

# Forest with ITs
forestbuff_tif <- here::here(out_dir, 'forest2022_buff2km_500m.tif')
fmask_biplc_fp <- here::here(out_dir, 'forestmask_500m_MBF_IPLCs.tif')
# fbuffiplc_fp <- here::here(out_dir, 'forestmask_500m_buff2km.tif')

# Forest with ITs
forestbuffIT_tif <- here::here(out_dir, 'forestbuff_500m_ITs.tif')
fmask_bip_fp <- here::here(out_dir, 'forestmask_500m_MBF_ITs.tif')
# fbuff_fp <- here::here(out_dir, 'forestmask_500m_buff2km.tif')

# Prep forest ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(!file.exists(forest_tif)){
  # VRT for Hansen forest cover - doesn't work on VM
  interp_tiles <- list.files(hansen_dir, 'forest2022.*\\.tif', full.names = TRUE)
  forest_vrt <- here::here(hansen_dir, 'forest2022_tropics.vrt')
  args <- c('gdalbuildvrt', forest_vrt, interp_tiles) 
  out <- system2('sudo', args = args)
  
  # Convert forest to correct MODIS crs
  args <- c('gdal_translate', '-a_srs', "'+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs'", 
            forest_vrt, forest_tif) 
  out <- system2('sudo', args = args)
}

# Load biome polygons and transform
mbf_pols <- st_read(dhf_diss_fp) %>% 
  st_transform(st_crs(terra::rast(forest_tif))) 

# Crop and mask to biome
for_rb <- terra::rast(forest_tif) %>% 
  crop(terra::vect(mbf_pols), mask=TRUE, 
       filename = fmask_biome_fp,
       datatype = 'INT1U',
       gdal=c("COMPRESS=LZW"),
       overwrite = TRUE)

## IPLCs ----
# Load IPLC polygons and transform
ip_pols <- st_read(iplcs_fp) %>% 
  st_transform(st_crs(terra::rast(forest_tif))) 

# Crop and mask to biome
for_bip <- terra::rast(fmask_biome_fp) %>% 
  crop(terra::vect(ip_pols), mask=TRUE, 
       filename = fmask_biplc_fp,
       datatype = 'INT1U',
       gdal=c("COMPRESS=LZW"),
       overwrite = TRUE)

# Load forest buffer 
if(!file.exists(forestbuff_tif)){
  # VRT for Hansen forest cover - doesn't work on VM
  interp_tiles <- list.files(dirname(hansen_dir), 'forest2022_buff2km.*\\.tif', full.names = TRUE)
  forestbuff_vrt <- here::here(dirname(hansen_dir), 'forest2022_buff2km_tropics.vrt')
  args <- c('gdalbuildvrt', forestbuff_vrt, interp_tiles) 
  out <- system2('sudo', args = args)
  
  # Convert forest to correct MODIS crs
  args <- c('gdal_translate', '-a_srs', "'+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs'", 
            forestbuff_vrt, forestbuff_tif) 
  out <- system2('sudo', args = args)
}

## ITs ----
# Load IPLC polygons and transform
ip_pols <- st_read(its_fp) %>% 
  st_transform(st_crs(terra::rast(forest_tif))) 

# Crop and mask to ITs
for_bip <- terra::rast(fmask_biome_fp) %>% 
  crop(terra::vect(ip_pols), mask=TRUE, 
       filename = fmask_bip_fp,
       datatype = 'INT1U',
       gdal=c("COMPRESS=LZW"),
       overwrite = TRUE)

# Load forest buffer
if(!file.exists(forestbuffIT_tif)){
  # VRT for Hansen forest cover - doesn't work on VM
  interp_tiles <- list.files(dirname(hansen_dir), 'forest2022_buff2km_500m_ITs_inMBF.*\\.tif', full.names = TRUE)
  forestbuff_vrt <- here::here(dirname(hansen_dir), 'forest2022_buff2km_500m_ITs_inMBF.vrt')
  args <- c('gdalbuildvrt', forestbuff_vrt, interp_tiles) 
  out <- system2('sudo', args = args)
  
  # Convert forest to correct MODIS crs
  args <- c('gdal_translate', '-a_srs', "'+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs'", 
            forestbuff_vrt, forestbuffIT_tif) 
  out <- system2('sudo', args = args)
}

# Mask AGB to biome ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(!file.exists(agb_biome_fp)){
  agb_mgcha <- terra::rast(agb_tif)
  
  # Load biome polygons and transform
  pols <- st_read(dhf_diss_fp) %>% 
    st_transform(st_crs(agb_mgcha)) 
  
  # Get area of biome
  pols %>% st_area() %>% units::set_units('ha') %>% sum()
  
  # Crop and mask to biome
  agb_mgcha_biome <- agb_mgcha %>% 
    crop(terra::vect(pols), mask=TRUE, 
         filename = agb_biome_fp,
         # datatype = 'INT1U',
         gdal=c("COMPRESS=LZW"),
         overwrite = FALSE)
  # plot(agb_mgcha_biome)
  
} else {
  agb_mgcha_biome <- terra::rast(agb_biome_fp)
}

pix_m <- res(agb_mgcha_biome)[1]
pix_ha <- pix_m * pix_m * 1e-4

# Get sum for biome
c_biome <- agb_mgcha_biome %>% global(fun='sum', na.rm=TRUE)
cstock_biome <- c_biome * pix_ha
cstock_biome

# # Convert C density to stock
# agb_mgc_biome <- agb_mgcha_biome * pix_ha
# plot(agb_mgc_biome)
# 
# # Get sum for biome
# cstock_biome <- agb_mgc_biome %>% global(fun='sum', na.rm=TRUE)
# cstock_biome

# Mask AGB to forest ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Mask to forest
if(!file.exists(agbm_fp)){
  # Load biome polygons and transform
  pols <- st_read(dhf_diss_fp) %>% 
    st_transform(st_crs(agb_mgcha_biome)) 
  
  for_r <- terra::rast(forest_tif) %>% 
    crop(terra::vect(pols))
  
  # Reclassify treecover % to forest mask
  rclmat <- rbind(c(-Inf, 0.25, NA), 
                  c(0.25, Inf, 1))
  fmask <- for_r %>% 
    classify(rclmat, 
             include.lowest=TRUE, 
             filename = fmask_fp,
             datatype = 'INT1U',
             overwrite = TRUE)
  
  fmask <- fmask %>% crop(agb_mgcha_biome)
  agb_mgcha_biome <- agb_mgcha_biome %>% crop(fmask)
         
  # Mask to forest
  agb_mgcha_bf <- agb_mgcha_biome %>%
    mask(fmask, maskvalues=NA, filename=agbm_fp, 
         gdal=c("COMPRESS=LZW"),
         overwrite = TRUE)
}

# plot(fmask)

agb_mgcha_bf <- terra::rast(agbm_fp)
plot(agb_mgcha_bf)

# Get sum
c_bf <- agb_mgcha_bf %>% global(fun='sum', na.rm=TRUE)
cstock_bf <- c_bf * pix_ha
cstock_bf

# Mask AGB to IPLC ----

# Load biome polygons and transform
pols <- st_read(iplcs_fp) %>% 
  st_transform(st_crs(agb_mgcha_biome)) 

# Get area of biome
pols %>% st_area() %>% units::set_units('ha') %>% sum()

# Crop and mask to IPLC
agb_mgcha_bip <- agb_mgcha_biome %>% 
  crop(terra::vect(pols), mask=TRUE,
       filename=agbm_fpiplc,
       datatype = 'INT2U',
       gdal=c("COMPRESS=LZW"),
       overwrite = TRUE)
plot(agb_mgcha_bip)

# Get sum for biome
c_bip <- agb_mgcha_bip %>% global(fun='sum', na.rm=TRUE)
cstock_bip <- c_bip * pix_ha
cstock_bip

# Mask AGB to IPLC and forest ----

# Crop and mask to IPLC
agb_mgcha_bipf <- agb_mgcha_bf %>% 
  crop(terra::vect(pols), mask=TRUE)
plot(agb_mgcha_bipf)

# Get sum for biome
c_bipf <- agb_mgcha_bipf %>% global(fun='sum', na.rm=TRUE)
cstock_bipf <- c_bipf * pix_ha
cstock_bipf



# Mask AGB to ITs ----

# Load biome polygons and transform
pols <- st_read(its_fp) %>% 
  st_transform(st_crs(agb_mgcha_biome)) 

# Get area of biome
pols %>% st_area() %>% units::set_units('ha') %>% sum()

# Crop and mask to IPLC
agb_mgcha_bip <- agb_mgcha_biome %>% 
  crop(terra::vect(pols), mask=TRUE,
       filename=agbm_fpit,
       datatype = 'INT2U',
       gdal=c("COMPRESS=LZW"),
       overwrite = TRUE)
plot(agb_mgcha_bip)

# Get sum for biome
c_bip <- agb_mgcha_bip %>% global(fun='sum', na.rm=TRUE)
cstock_bip <- c_bip * pix_ha
cstock_bip

# Mask AGB to ITs and forest ----
# Crop and mask to IPLC
agb_mgcha_bipf <- agb_mgcha_bf %>% 
  crop(terra::vect(pols), mask=TRUE)
plot(agb_mgcha_bipf)

# Get sum for biome
c_bipf <- agb_mgcha_bipf %>% global(fun='sum', na.rm=TRUE)
cstock_bipf <- c_bipf * pix_ha
cstock_bipf


# Buffer ----
buff_dist <- 2000 # 2km
fmask <- terra::rast(fmask_fp)
fbuff <- fmask %>% buffer(buff_dist)
fbuff <- fbuff %>% mask()

fbuff <- fbuff %>% 
  crop(vect(pols), mask=TRUE, 
       filename = fbuffiplc_fp,
       datatype = 'INT1U',
       gdal=c("COMPRESS=LZW"),
       overwrite = FALSE)

# View for a subregion
pols <- st_read(dhf_diss_fp) %>% 
  st_transform(st_crs(agb_mgcha_biome)) 
subpol <- pols %>% slice(1)
plot(fbuff %>% crop(vect(subpol)))


fbuff <- fbuff %>% crop(agb_mgcha_biome)
agb_mgcha_biome <- agb_mgcha_biome %>% crop(fbuff)


# Calculate Euclidean distance. Set image parameters
# var euclideanDist = forest.gt(forest_tc)
# .distance(ee.Kernel.euclidean(buff_dist, 'meters'));

# var buffer = euclideanDist.gt(0)
# .reduceResolution(ee.Reducer.mode(), false, 65535);



# LCI ----
# sudo gsutil -m cp -r gs://ejs-data/lci_projects/biome_mbf/ \
# ~/repos/sandbox/hih/data/LCI/
lci_dir <- here::here('hih/data/LCI/biome_mbf')

lci_r <- terra::rast(here::here(lci_dir, 'comp_add.tif'))
lci_mask1_fp <- here::here(dirname(lci_dir), 'comp_add_MBF.tif')

# Load biome polygons and transform
mbf_pols <- st_read(dhf_diss_fp) %>% 
  st_transform(st_crs(lci_r)) 

# Crop and mask to biome
lci_rb <- lci_r %>% 
  crop(terra::vect(mbf_pols), mask=TRUE, 
       filename = lci_mask1_fp,
       datatype = 'INT1U',
       gdal=c("COMPRESS=LZW"),
       overwrite = TRUE)

## IPLCs ----
lci_mask2_fp <- here::here(dirname(lci_dir), 'comp_add_MBF_IPLC.tif')
lci_forest_fp <- here::here(dirname(lci_dir), 'comp_add_forest_MBF.tif')
lci_forestbuff_fp <- here::here(dirname(lci_dir), 'comp_add_forestbuff_MBF_IPLC.tif')

# Load IPLC polygons and transform
ip_pols <- st_read(iplcs_fp) %>% 
  st_transform(st_crs(lci_r)) 

# Crop and mask to IPLCs
lci_bip <- lci_rb %>% 
  crop(terra::vect(ip_pols), mask=TRUE, 
       filename = lci_mask2_fp,
       datatype = 'INT1U',
       gdal=c("COMPRESS=LZW"),
       overwrite = TRUE)

# Mask to forest
fmask <- terra::rast(fmask_fp)
fmask <- fmask %>% crop(lci_r)
lci_r <- lci_r %>% crop(fmask)
lci_f <- lci_r %>%
  mask(fmask, maskvalues=NA, filename=lci_forest_fp, 
       gdal=c("COMPRESS=LZW"),
       overwrite = TRUE)

# Mask to forest+buffer
fbmask <- terra::rast(forestbuff_tif)
fbmask <- fbmask %>% crop(lci_r)
lci_r <- lci_r %>% crop(fbmask)
lci_fb <- lci_r %>%
  mask(fbmask, maskvalues=0, filename=lci_forestbuff_fp, 
       gdal=c("COMPRESS=LZW"),
       overwrite = TRUE)


## ITs ----
lci_mask3_fp <- here::here(dirname(lci_dir), 'comp_add_MBF_IT.tif')
lci_forestbuffIT_fp <- here::here(dirname(lci_dir), 'comp_add_forestbuff_MBF_IT.tif')

# Load IPLC polygons and transform
ip_pols <- st_read(its_fp) %>% 
  st_transform(st_crs(lci_r)) 

# Crop and mask to ITs
lci_bip <- lci_rb %>% 
  crop(terra::vect(ip_pols), mask=TRUE, 
       filename = lci_mask3_fp,
       datatype = 'INT1U',
       gdal=c("COMPRESS=LZW"),
       overwrite = TRUE)

# Mask to forest+buffer from ITs
fbmask <- terra::rast(forestbuffIT_tif)
fbmask <- fbmask %>% crop(lci_r)
lci_r <- lci_r %>% crop(fbmask)
lci_fb <- lci_r %>%
  mask(fbmask, maskvalues=0, filename=lci_forestbuffIT_fp, 
       gdal=c("COMPRESS=LZW"),
       overwrite = TRUE)

# Mask Population to forest and forest+buffer ----
gee_dir <- here::here(out_dir, 'from_gee_500m')
pop_tif <- here::here(out_dir, 'pop_500m')

if(!file.exists(pop_tif)){
  # VRT for Hansen forest cover - doesn't work on VM
  interp_tiles <- list.files(gee_dir, 'Pop_2020_cons_unadj_500m.*\\.tif', full.names = TRUE)
  pop_vrt <- here::here(gee_dir, 'Pop_2020_cons_unadj_500m.vrt')
  args <- c('gdalbuildvrt', pop_vrt, interp_tiles) 
  out <- system2('sudo', args = args)
  
  # Convert forest to correct MODIS crs
  args <- c('gdal_translate', '-a_srs', "'+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs'", 
            pop_vrt, pop_tif) 
  out <- system2('sudo', args = args)
}



pop_biome_fp <- here::here(out_dir, 'pop_500m_MBF.tif')

pop_r <- terra::rast(pop_tif)

# Load biome polygons and transform
mbf_pols <- st_read(dhf_diss_fp) %>% 
  st_transform(st_crs(pop_r)) 

# Crop and mask to biome
pop_biom <- pop_r %>% 
  crop(terra::vect(mbf_pols), mask=TRUE,
       filename=pop_biome_fp, 
       datatype = 'INT4U',
       gdal=c("COMPRESS=LZW"),
       overwrite = TRUE)
plot(pop_biom)

## IPLCs ----
pop_bip_fp <- here::here(out_dir, 'pop_500m_MBF_IPLC.tif')
pop_forest_fp <- here::here(out_dir, 'pop_500m_forIPLC.tif')
pop_forestbuff_fp <- here::here(out_dir, 'pop_500m_forIPLC_buff.tif')

# Load IPLC polygons and transform
ip_pols <- st_read(iplcs_fp) %>% 
  st_transform(st_crs(pop_r)) 

# Crop and mask to IPLCs
pop_biom_ip <- pop_biom %>% 
  crop(terra::vect(ip_pols), mask=TRUE,
       filename=pop_bip_fp, 
       datatype = 'INT4U',
       gdal=c("COMPRESS=LZW"),
       overwrite = TRUE)

# Mask to forest in IPLC
fmask <- terra::rast(fmask_fp)
fmask <- fmask %>% crop(pop_biom_ip)
pop_biom_ipc <- pop_biom_ip %>% crop(fmask)
pop_f <- pop_biom_ipc %>%
  mask(fmask, 
       maskvalues=NA, 
       filename=pop_forest_fp, 
       datatype = 'INT4U',
       gdal=c("COMPRESS=LZW"),
       overwrite = TRUE)

# Mask to forest+buffer
fbmask <- terra::rast(forestbuff_tif)
fbmask <- fbmask %>% crop(pop_r)
pop_biomc <- pop_biom %>% crop(fbmask)
pop_fb <- pop_biomc %>%
  mask(fbmask, 
       maskvalues=0, 
       filename=pop_forestbuff_fp, 
       datatype = 'INT4U',
       gdal=c("COMPRESS=LZW"),
       overwrite = TRUE)
plot(pop_fb)

## ITs ----
pop_bip_fp <- here::here(out_dir, 'pop_500m_MBF_IT.tif')
pop_forest_fp <- here::here(out_dir, 'pop_500m_forIT.tif')
pop_forestbuffIT_fp <- here::here(out_dir, 'pop_500m_forIT_buff.tif')

# Load IPLC polygons and transform
ip_pols <- st_read(its_fp) %>% 
  st_transform(st_crs(pop_r)) 

# Crop and mask to IPLCs
pop_biom_ip <- pop_biom %>% 
  crop(terra::vect(ip_pols), mask=TRUE,
       filename=pop_bip_fp, 
       datatype = 'INT4U',
       gdal=c("COMPRESS=LZW"),
       overwrite = TRUE)

# Mask to forest in IPLC
fmask <- terra::rast(fmask_fp)
fmask <- fmask %>% crop(pop_biom_ip)
pop_biom_ipc <- pop_biom_ip %>% crop(fmask)
pop_f <- pop_biom_ipc %>%
  mask(fmask, 
       maskvalues=NA, 
       filename=pop_forest_fp, 
       datatype = 'INT4U',
       gdal=c("COMPRESS=LZW"),
       overwrite = TRUE)

# Mask to forest+buffer
fbmask <- terra::rast(forestbuffIT_tif)
fbmask <- fbmask %>% crop(pop_biom)
pop_biomc <- pop_biom %>% crop(fbmask)
fbmask <- fbmask %>% crop(pop_biomc)
pop_fb <- pop_biomc %>%
  mask(fbmask, 
       maskvalues=0, 
       filename=pop_forestbuffIT_fp, 
       datatype = 'INT4U',
       gdal=c("COMPRESS=LZW"),
       overwrite = TRUE)
plot(pop_fb)
