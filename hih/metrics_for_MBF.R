
library(tidyverse)
library(sf)
library(terra)
library(units)
library(stars)

out_dir <- '/Users/esturdivant/data/hih/requests/scaling'
out_dir <- here::here('hih/data') 

# Walker AGB
# agb_tif <- '/Volumes/ejs_storage/data/raw_data/biomass/Woodwell_AGB_2018_Global_500m_Mgha_wm.tif'
agb_tif <- '~/data/Walker_etal_2022/Base_Cur_AGB_MgCha_500m.tif'

# Biome
dhf_diss_fp <- here::here('hih/data/Ecoregions2017_MBF_realms.geojson')

# Hansen forest cover
hansen_dir <- '/Users/esturdivant/data/Hansen_etal_2013/from_gee_500m'
interp_tiles <- list.files(hansen_dir, '*\\.tif', full.names = TRUE)
forest_vrt <- here::here(hansen_dir, 'alltiles.vrt')
gdalUtilities::gdalbuildvrt(gdalfile = interp_tiles, forest_vrt, overwrite=T)

# Convert to correct MODIS crs
forest_tif <- here::here(hansen_dir, 'alltiles_mod.tif')
args <- c('-a_srs', "'+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs'", 
          forest_vrt, forest_tif) 
out <- system2('gdal_translate', args = args)
str_c('gdal_translate ', str_c(args, collapse=' '))

# Mask AGB to forest ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
agb_r <- stars::read_stars(agb_tif)
for_r <- stars::read_stars(forest_tif)

for_r <- terra::rast(forest_tif)
agb_r <- terra::rast(agb_tif, win=terra::ext(for_r))

agbm_fp <- here::here(out_dir, 'Base_Cur_AGB_MgCha_500m_forest_MBF.tif')
agb_rm <- agb_r %>%
  mask(for_r, maskvalues=NA, filename=agbm_fp, 
       gdal=c("COMPRESS=LZW"))
plot(agb_r)
plot(for_r)
plot(agb_rm)

# Get values for one raster layer ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fp_poly <- dhf_diss_fp
site_var <- params$site_var

params <- list(rast_fp = agbm_fp, level='AGB_MgCha')

extract_values <- function(params) {
  # Read stars (proxy)
  rr <- stars::read_stars(params$rast_fp)
  
  # Pixel area in hectares to convert density to stock
  dims <- st_dimensions(rr)
  pix_ha <- dims$x$delta * dims$y$delta * 1e-4
  
  # Load and reproject polygons
  pols <- st_read(fp_poly) |> st_transform(st_crs(rr))
  
  # Get pixel values for each polygon ----
  df <- seq_len(nrow(pols)) |> 
    purrr::map_dfr(
      function(i) {
        pol <- pols[i,]
        
        # Extract values
        ar <- rr[pol] |> 
          stars::st_as_stars() |> 
          pull() |> 
          as.vector() |> 
          na.omit() |> 
          as_tibble() |> 
          mutate(value = value * pix_ha)
      }
    )
  
  # Add variable with LCI level name
  df |> mutate(level = params$level)
}

# Extract values ----
df_p <- extract_values(params)

df_p <- tifs_params |> purrr::map_dfr(extract_values) |> 
  mutate(comp_name = factor(level, levels = purrr::map_chr(tifs_params, ~.x$level))) 

# Summary stats ----
pix.ha <- 463.3127 * 463.3127 * 1e-4
stats <- df_p |> 
  # mutate(name = forcats::fct_rev(name)) |> 
  group_by(comp_name) |> 
  summarize(Min = min(value, na.rm=TRUE),
            Q1 = quantile(value, 0.25, na.rm=TRUE),
            Median = median(value, na.rm=TRUE),
            Mean = mean(value, na.rm=TRUE),
            Q3 = quantile(value, 0.75, na.rm=TRUE),
            Max = max(value, na.rm=TRUE),
            N = n(),
            Stock_tC = sum(value, na.rm=TRUE), 
            Area_ha = N * pix.ha, 
            Dens_tCha = Stock_tC / Area_ha, 
            Stock_MtC = Stock_tC * 1e-6, 
            Area_Mha = Area_ha * 1e-6)
