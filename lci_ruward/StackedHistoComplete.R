# Libraries
library(terra)
library(tidyverse)
library(here)
library(stringr)

# Load as multi-layer SpatRast ----
multilyr <- terra::rast(c(here::here("lci_ruward/FLI_2002_2012.tif"), 
                          here::here("lci_ruward/AMZ_loss_pers_13_2022.tif"), 
                          here::here("lci_ruward/Nonforest_mask_2012.tif")))

# Set layer names
names(multilyr) <- c('variable', 'loss', 'nonforest')

# Convert to DF ----
# Convert to DF with loss categories
df <- multilyr %>% 
  as.data.frame() %>% 
  mutate(loss = case_when(loss == 1 ~ 'persistence',
                          loss == 2 ~ 'loss',
                          TRUE ~ NA_character_))

# Filter out NAs in mask layer (apply mask)
df_filt <- df %>% 
  filter(!is.na(nonforest))

# Bar chart ----
# Count pixels per bin 
bins_df <- df_filt %>%
  mutate(bins = cut_interval(variable, length=300)) %>% # Get intervals
  count(loss, bins) %>%                                 # Count pixels
  mutate(bins = bins %>%                                # Get start of interval
           str_remove( ",.*") %>% 
           str_remove('[:punct:]') %>% 
           as.numeric())
  
# Plot - stacked percent bar chart
bins_df %>% 
  ggplot(aes(fill=loss, y=n, x=bins)) +
  geom_bar(position='fill', stat = 'identity') +
  xlab("FLI") +
  ylab("Proportion of Total Pixels in Bin")

# Plot
bins_df %>% 
  ggplot(aes(fill=loss, y=n, x=bins)) +
  geom_bar(position='dodge', stat = 'identity') +
  xlab("FLI") +
  ylab("Proportion of Total Pixels in Bin")

# Plot
bins_df %>% 
  ggplot(aes(fill=loss, y=n, x=bins)) +
  geom_bar(position='stack', stat = 'identity') +
  xlab("FLI") +
  ylab("Proportion of Total Pixels in Bin")

# Density plot ----
df_filt %>% 
  select(variable, loss) %>% 
  ggplot(aes(x=variable, group=loss, color=loss, fill=loss)) +
  geom_density(alpha=0.5)







#set mask to distance (masked out nonforest at 2012)
mask <- AMZ_nonforest_2012

#Mask Rasters to Nonforest 2012
loss_mask <- mask(DRC_loss_pers_13_22, mask)

distance_mask <- mask(DRC_Dist_2012, mask)


#Reclass_FLI_mask <- mask(rc1, mask_rast)
#mask_rast <- rast(mask)
#Reclass_FLI_mask_DF <- as.data.frame(Reclass_FLI_mask)


#FLI_loss_12_19 <- raster("C:/Users/rutha/Downloads/FLI_loss_12_19.tif")

#FLI_DF <- as.data.frame(FLI_loss_12_19)

# Set variables for each region / year
distance <- distance_mask
loss_persistence <- loss_mask

# Check extent and min/max values of each raster
#cellStats(distance, range)
#distance@extent
#loss_12_19_test@extent
#summary(DRC_loss_pers_12_19)
#crs(distance)
#crs(loss_persistence)

#plot distance raster images
#image.plot(amazon_distance_2011,
#           xlim = c(-6710500, -5267000), #extent
#           ylim = c(-1646000, -231000), #extent
#           zlim = c(463, 56350), #min and max values
#           title = "Distance from Nonforest 2018 Xingu Basin (meters)",
#           col = hcl.colors(12, "viridis"), useRaster = TRUE)
#title(main = "Distance from Nonforest 2018 Xingu Basin (meters)")


# Mask Nonforest at start of time interval rom both rasters
#distance[distance == 0] <- NA
#loss_persistence<- mask(loss_persistence, distance)
#
#cellStats(distance, range)
#loss_persistence@extent

#print(DF1)

# Convert rasters to data frames
distance_DF <- as.data.frame(distance)
loss_persistence_DF <- as.data.frame(loss_persistence)

# Combine into one data frame
DF1 <- data.frame(distance_DF, loss_persistence_DF)
print(DF1)

#select just the Forest Loss, creating a new dataframe
updated_df <- subset(DF1, lossyear != "Forest Persistence 2022")


# change loss / persistance labels
DF1$lossyear <- ifelse(DF1$lossyear == 1, 'Forest Persistence 2022', DF1$lossyear)
DF1$lossyear <- ifelse(DF1$lossyear == 2, 'Forest Loss 2013 - 2022', DF1$lossyear)

#FLI_rast = rast(AMZ_FLI_2002_2012)
#print(FLI_rast)
#print(distance)

#create new column of Dataframe That reclasses FLI values into bins of 500
# <- c(0, 0, 0,
#       1, 500, 500,
#       501, 1000, 1000,
#       1001, 1500, 1500,
#       1501, 2000, 2000,
#       2001, 2500, 2500,
#       2501, 3000, 3000,
#       3001, 3500, 3500,
#       3501, 4000, 4000,
#       4001, 4500, 4500,
#       5001, 5500, 5500,
#       5501, 6000, 6000,
#       6001, 6500, 6500,
#       6501, 7000, 7000,
#       7001, 7500, 7500,
#       7501, 8000, 8000,
#       8001, 8500, 8500,
#       8501, 9000, 9000,
#       9001, 9500, 9500,
#       9501, 10000, 10000)

#rclmat <- matrix(m, ncol = 3, byrow = TRUE)
#rc1 <- classify(FLI_rast, rclmat, include.lowest=TRUE)
#rc1_DF <- as.data.frame(rc1)
#print(rc1_DF)
print(DF1)
#plot stacked histogram Distance
ggplot(data=DF1, aes(x = fcf_2012, fill = lossyear)) + geom_histogram() +
  xlab("Distance from Nonforest (meters)") +
  geom_histogram(bins = 20) +
  #xlim(c(-20, 0)) +
  ylab("Frequency") +
  labs(title = "Stacked Histogram of Loss per Distance from Nonforest DRC 2013 - 2022")

#fcloss_02to12_l2_tif
#trendsum_73_unweighted

#plot stacked histogram FLI
ggplot(data=subset(DF1, !is.na(lossyear)), aes(x=fcf_2012, fill = lossyear)) +
  geom_histogram(bins = 20) +
  xlab("Distance From Nonforest (meters)") +
  ylab("Frequency") +
  labs(title = "Stacked Histogram of Loss per Distance from Nonforest 2013 - 2022")


# Proportional Plot
ggplot() +
  geom_bar(data = DF1, aes(x = fcloss_02to12_l2_tif, fill = lossyear),
           position = "fill") +
  scale_x +
         #  breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000,
         #             4500, 5000, 5500, 6000, 6500, 7000, 7500, 8000,
          #            8500, 9000, 9500, 10000)) +
  xlab("FLI") +
  ylab("Proportion of Total Pixels in Bin") +
  labs(title = "100% Stacked Histogram of FLI in the Xingu Basin")

#plot of just forest loss
ggplot(data = updated_df, aes(x = fcf_2012)) + geom_histogram(bins = 20) +
  xlab('Distance from Nonforest (meters)') +
  ylab ('Frequency of Forest Loss 2013 - 2022') +
labs(title = 'Forest Loss per Distance from Nonforest DRC 2013 - 2022')


#view image
#image(loss_persistence,
#      xlim = c(-6710500, -5267000), #extent
#      ylim = c(-1646000, -231000), #extent
#      zlim = c(1, 2), #min and max values
#      col = hcl.colors(2, "Grays"),
#      useRaster = TRUE)
#title(main = "Forest / Nonforest Xingu Basin 2018")
#legend(x = "topright", legend = c("Nonforest", "Forest"), fill = c("black", "white"))
