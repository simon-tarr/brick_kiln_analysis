
library(tidyverse)
library(doParallel)
library(foreach)
library(raster)
library(lubridate)

##############
### README ###
##############

# This script takes processed ISIMIP3b climate data 
# and creates dataframes which can be ingested by the 
# WBGT modelling package called `HeatStress`

# The dataframes take the same form as the example data in `HeatStress`
# You can view the expected data by loading it with `data("data_obs")`

# You must run 01_wbgt_process_climate.R first to process 
# the raw ISIMIP3b climate data or download the processed data and 
# save it in an appropriate location

####################
### LOAD IN DATA ###
####################

# Load cell centroids (coordinates for land ONLY)
pts<-read_csv("output/india_pts.csv")

# Function to create output dataframes for WBGT analyses
# Explore function code to see the expected directory structure for input climate data
create_wbgt_df<-function(gcm, scenario, pts, dewpoint_mode){
  
  # gcm = Character. Shortname of the GCM e.g. GFDL-ESM2 would become gfdl. Must be one of: gfdl, ipsl, mpi, mri or ukes
  # scenario = Character. The emissions scenario of interest. Can be one of: ssp126, ssp370, ssp585
  # pts = Tibble. Dataframe containing coordinates of cell centroids for the study area
  # dewpoint_mode = Character. see ?frost::calcDewPoint() for information on the different modes. Must be one of: A, B or C
  
  pts<-as_tibble(pts[, 1:2])
  
  # Create vector of dates to sensibly name each layer in the rasterstack
  # Dates span the length of the study 2021 - 2050
  days<-seq(ymd("2021-01-01"), ymd("2050-12-31"), by = "days")
  
  print("Loading raster data...")

  hursAdjust<-raster::brick(paste0("output/climate_data/", gcm, "/", scenario, "/hursAdjust_", trimws(gcm, whitespace = '-.*'),"_2021_2050_", scenario, ".nc"))
  tasAdjust<-raster::brick(paste0("output/climate_data/", gcm,"/", scenario, "/tasAdjust_", trimws(gcm, whitespace = '-.*'),"_2021_2050_", scenario, ".nc"))
  tasmaxAdjust<-raster::brick(paste0("output/climate_data/", gcm,"/", scenario, "/tasmaxAdjust_", trimws(gcm, whitespace = '-.*'),"_2021_2050_", scenario, ".nc"))
  windAdjust<-raster::brick(paste0("output/climate_data/", gcm,"/", scenario, "/windAdjust_", trimws(gcm, whitespace = '-.*'),"_2021_2050_", scenario, ".nc"))
  rsdsAdjust<-raster::brick(paste0("output/climate_data/", gcm,"/", scenario, "/rsdsAdjust_", trimws(gcm, whitespace = '-.*'),"_2021_2050_", scenario, ".nc"))

  names(hursAdjust)<-days
  names(tasAdjust)<-days
  names(tasmaxAdjust)<-days
  names(windAdjust)<-days
  names(rsdsAdjust)<-days
  
  print("Extracting values from rasters...")
  
  hurs_dat<-as_tibble(raster::extract(hursAdjust, pts))
  tas_dat<-as_tibble(raster::extract(tasAdjust, pts) - 273.15) # Convert from Kelvin to Celsius
  tasmax_dat<-as_tibble(raster::extract(tasmaxAdjust, pts) - 273.15) # Convert from Kelvin to Celsius
  wind_dat<-as_tibble(raster::extract(windAdjust, pts))
  sol_dat<-as_tibble(raster::extract(rsdsAdjust, pts))
  
  print("Building climate dataframe for WBGT analysis...")
  
  dat<-list()
  
  for(i in 1:nrow(pts)){
    
    # Grab climate vars from tibbles
    h<-hurs_dat[i, ]
    t<-tas_dat[i, ]
    m<-tasmax_dat[i, ]
    w<-wind_dat[i, ]
    s<-sol_dat[i,] 
    
    # Create vector of column names
    col_names<-c("hurs", "tasmean", "tasmax", "wind", "solar")
    
    # Bind these rows together
    # Transpose rows/columns to make 4 x 10,957 dataframe
    # Rename and reorder columns
    x<-bind_rows(h, t, m, w, s)
    x<-as_tibble(t(x)) %>% 
      rename_at(., names(.), ~col_names) %>%
      add_column(date = days) %>% 
      mutate(dewp = frost::calcDewPoint(RH = hurs, temp = tasmean, mode = dewpoint_mode)) %>% 
      dplyr::select(date, tasmean, tasmax, dewp, hurs, wind, solar)
    
    dat[[i]]<-x
    
  }
  
  return(dat)
  
}

#################
### GFDL-ESM4 ###
#################

gfdl_ssp126<-create_wbgt_df(gcm = 'gfdl-esm4', scenario = 'ssp126', pts=pts, dewpoint_mode = "A")
gfdl_ssp370<-create_wbgt_df(gcm = 'gfdl-esm4', scenario = 'ssp370', pts=pts, dewpoint_mode = "A")
gfdl_ssp585<-create_wbgt_df(gcm = 'gfdl-esm4', scenario = 'ssp585', pts=pts, dewpoint_mode = "A")

####################
### IPSL-CM6A-LR ###
####################

ipsl_ssp126<-create_wbgt_df(gcm = 'ipsl-cm6a-lr', scenario = 'ssp126', pts=pts, dewpoint_mode = "A")
ipsl_ssp370<-create_wbgt_df(gcm = 'ipsl-cm6a-lr', scenario = 'ssp370', pts=pts, dewpoint_mode = "A")
ipsl_ssp585<-create_wbgt_df(gcm = 'ipsl-cm6a-lr', scenario = 'ssp585', pts=pts, dewpoint_mode = "A")

#####################
### MPI-ESM1-2-HR ###
#####################

mpi_ssp126<-create_wbgt_df(gcm = 'mpi-esm1-2-hr', scenario = 'ssp126', pts=pts, dewpoint_mode = "A")
mpi_ssp370<-create_wbgt_df(gcm = 'mpi-esm1-2-hr', scenario = 'ssp370', pts=pts, dewpoint_mode = "A")
mpi_ssp585<-create_wbgt_df(gcm = 'mpi-esm1-2-hr', scenario = 'ssp585', pts=pts, dewpoint_mode = "A")

##################
### MRI-ESM2-0 ###
##################

mri_ssp126<-create_wbgt_df(gcm = 'mri-esm2-0', scenario = 'ssp126', pts=pts, dewpoint_mode = "A")
mri_ssp370<-create_wbgt_df(gcm = 'mri-esm2-0', scenario = 'ssp370', pts=pts, dewpoint_mode = "A")
mri_ssp585<-create_wbgt_df(gcm = 'mri-esm2-0', scenario = 'ssp585', pts=pts, dewpoint_mode = "A")

#################
### UKESM1-LL ###
#################

ukes_ssp126<-create_wbgt_df(gcm = 'ukesm1-0-ll', scenario = 'ssp126', pts=pts, dewpoint_mode = "A")
ukes_ssp370<-create_wbgt_df(gcm = 'ukesm1-0-ll', scenario = 'ssp370', pts=pts, dewpoint_mode = "A")
ukes_ssp585<-create_wbgt_df(gcm = 'ukesm1-0-ll', scenario = 'ssp585', pts=pts, dewpoint_mode = "A")


