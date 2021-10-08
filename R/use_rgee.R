
# Load libraries ----
library(rgee)
ee_Initialize()
library(tidyverse)

# Initialize variables ----
viridis <- c('#440154', '#433982', '#30678D', '#218F8B', '#36B677', '#8ED542', '#FDE725')

# Create ImageCollection ----
biomass_mgha <- ee$ImageCollection(c(
  "users/sgorelik/WHRC_Global_Biomass_500m_V6/Current_AGB_Mgha", 
  "users/sgorelik/WHRC_Global_Biomass_500m_V6/Current_BGB_Mgha", 
  "users/sgorelik/WHRC_Global_Biomass_500m_V6/Current_SOC_Mgha"
))

# pixel resolution and area
res <- biomass_mgha$
  first()$
  projection()$
  nominalScale()$getInfo()
pxl_ha <- (res*res)/(1e4)
print(str_c('Pixel area (ha): ', pxl_ha))

# convert from biomass (Mg/ha) to carbon stock (MgC)
convert_mgha_to_mgc <-  function(img_mgha) {
  img_mgha$
    divide(2)$       # biomass (Mg/ha) to carbon (Mg/ha)
    multiply(pxl_ha) # density (MgC/ha) to stock (MgC)
}

# Process biomass images - clip, convert to carbon stock, and sum
carbon_mgc_tropics <- biomass_mgha$
  #map( function(image) { return image.clip(bb) })
  map(convert_mgha_to_mgc)$
  sum()

# Aggregate biomass (MgC) from 500m to 1km ----
carbon_mgc_tropics_1km <- carbon_mgc_tropics$
  
  # Explicitly specify the layer in MODIS projection and resolution
  # reproject(crs = 'SR-ORG:6974', scale = 463.3127165275)$
  setDefaultProjection(crs = 'SR-ORG:6974', scale = 463.3127165275)$

  # Specify that you want to sum 500m carbon stock
  # values within each of the 1km output pixels
  reduceResolution(reducer = ee$Reducer$sum(), maxPixels = 5)$
  
  # Finally, specify the output spatial resolution (~500m)
  reproject(crs = 'SR-ORG:6974', scale = 1000)
  # setDefaultProjection(crs = 'SR-ORG:6974', scale = 1000)

# View ----
# Set a region of interest and center map display
region <- ee$Geometry$BBox(43, -18, 50, -16)
Map$centerObject(region, 6)

# View 500 m and 1 km total C stock
Map$addLayer(
  eeObject = carbon_mgc_tropics,
  visParams = list(min = 1, max = 20000, palette = viridis),
  name = "Total carbon (MgC)"
) + 
Map$addLayer(
  eeObject = carbon_mgc_tropics_1km,
  visParams = list(min = 1, max = 50000, palette = viridis),
  name = "Total carbon (MgC) 1km"
)
