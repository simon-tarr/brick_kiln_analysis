
library(tidyverse)
library(raster)
library(lubridate)


###############################
### CREATE LAND COORDINATES ###
###############################

# Load in India shapefile, create mask
shp <-raster::shapefile("input/study_area/study_area_adm3_noPAK.shp")
ext <- floor(extent(shp))
rr <- raster(ext, res = 0.5)
india_mask <- rasterize(shp, rr, field = 1)

# Load in a climate raster, crop to extent of india_mask
# Doesn't matter which original ISIMIP raster you use as all have the same resolution and extent
# Creates a raster of India's land and removes all ocean cells which we don't process
r <-raster::brick("original_climate_data/gdfl-esm4/ssp126/hursAdjust/gfdl-esm4_r1i1p1f1_w5e5_ssp126_hursAdjust_global_daily_2021_2030.nc")
cropped_ext <- raster::crop(x = r[[1]], y = india_mask)
cropped_mask <- raster::mask(cropped_ext, india_mask)

# Create tibble of coordinates and values for Indian land area only
# We drop rows where values=NA as these are ocean cells whic we don't process
india_pts_all <- as_tibble(sp::coordinates(cropped_mask))
india_values <-as_tibble(raster::extract(cropped_mask, india_pts_all))
pts <-as_tibble(cbind(india_pts_all, india_values)) %>% drop_na() %>% dplyr::select(-value)

# Extract regional information from regional areas shapefile
pts<-raster::extract(shp, pts) %>% 
  select(NAME_0, NAME_1, NAME_2, NAME_3) %>% 
  cbind(pts, .) %>% 
  as_tibble()

names(pts)<-c("x", "y", "country", "adm1", "adm2", "adm3")

regional_data<-pts

# Take a peek to make sure everything looks okay
# You should see a circle on each land cell across the landscape
# Save the points to disk to be used later if required
plot(cropped_mask)
points(pts)
write_csv(pts, file="output/india_pts.csv")

# Compute number of kilns within each cell
kilns<-read_csv("input/India_All_Brick_kilns_UNDP.csv") %>% 
  janitor::clean_names() %>% 
  rename(x = longitude) %>% 
  rename(y = latitude) %>% 
  dplyr::select(x, y)

cell_counts<-as_tibble(raster::cellFromXY(india_mask, xy=as.data.frame(kilns))) %>% 
  group_by(value) %>% 
  count()

kilns<-as_tibble(raster::xyFromCell(india_mask, cell_counts$value)) %>% 
  cbind(cell_counts) %>% 
  as_tibble() %>% 
  left_join(pts, ., by = c("x", "y")) %>% 
  rename(cell = value) %>% 
  rename(kiln_count = n) %>% 
  mutate(kiln_count = ifelse(is.na(kiln_count), 0, kiln_count))

# Plot kiln heatmap to see where most kilns are
plot(rasterize(x=kilns[, 1:2], # lon-lat data
               y=india_mask, # raster object
               field=kilns$kiln_count))

write_csv(kilns, file = "input/kiln_locations_processed.csv")
