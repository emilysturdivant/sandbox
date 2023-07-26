
library(terra)

fp_tif <- '/Users/esturdivant/Downloads/LCI_current_AGB.tif'

# Convert to INT1U with values 0-100 ----
# Load
rr <- terra::rast(fp_tif)

# Are any pixels > 10000?
sum(rr[rr['band_data'] > 10000])

# Convert to 0-100 range and round to integer
r100 <- rr / 100
r100int <- round(r100)

# Save as unsigned 1 byte integer
terra::writeRaster(r100int,
            '/Users/esturdivant/Downloads/LCI_current_AGB_0to100.tif', 
            datatype='INT1U', 
            gdal=c('COMPRESS=LZW'))








def zarr2gtiff(fp_in, fp_out='auto', dtype = 'float32', crs='modis',
               scale_to_100=False):
  ''' Converts a zarr archive to geotiff file '''

if fp_out == 'auto':
  fp_out = modify_extension(fp_in, '.tif')

ds_in = xr.open_zarr(fp_in)
# ds_in = read_dataset(fp_in, array=True)

if scale_to_100:
  # Convert to 0-100 range and round to integer
  ds_in <- round(ds_in / 100)

# Write to COG
ds_in = ds_in.rio.write_crs(crs)
write_geotiff(ds_in, fp_out, dtype = dtype, crs=crs)

return fp_out