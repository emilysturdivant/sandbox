# 8/30/2022 
# Emily Sturdivant
# Wayne: "please convert this RData file to a Shapefile using lat_lowestmode and lon_lowestmode as the coordinates"

library(sf)
library(tidyr)
library(dplyr)
library(tmap)
tmap_mode('view')

# Filepath
fp <- "/Users/esturdivant/data/misc/gedi_l2a_l4a_milkcreek.RData"

# Load RData file (variable "data")
load(fp)

# Look
head(data)
class(data)

# Convert to simple features
df <- st_as_sf(x = data,                         
               coords = c("lon_lowestmode", "lat_lowestmode"),
               crs = 'EPSG:4326')

# Map
qtm(df, dots.col = 'elev_lowestmode')

# Save as shp (field names are abbreviated by default)
st_write(df, "/Users/esturdivant/data/misc/gedi_l2a_l4a_milkcreek.shp")
