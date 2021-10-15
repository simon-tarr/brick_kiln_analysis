
library(sp)
library(raster)
library(HeatStress)
library(rgdal)
library(tidyverse)
library(lubridate)
library(foreach)
library(doParallel)
library(frost)

##############
### README ###
##############

# This script is used to process raw ISIMIP3b data (2021-01-01 to 2050-12-31 inclusive) 

# For the following GCMs:
# GFDL-ESM4, IPSL-CM6A-L, MPI-ESM1-2-HR, MRI-ESM2-0, UKESM1-0-L

# And the following emissions scenarios:
# SSP126, SSP370, SSP585

# And the following bias-corrected, near-surface, ISIMIP3b variables:
# Mean Air Temperature, Maximum Air Temperature, Relative Humidity, Wind Speed, Shortwave Downwelling Radition

# Output data are large RasterStacks for each unique combination of climate variable, GCM 
# and emissions scenario. Each Stack contains a total of 10,957 layers
# Each layer represents a single day over the time period specified above

# NOTE THAT THIS SCRIPT ONLY NEEDS TO BE RUN THE ONCE!
# IT WILL SAVE THE OUTPUT FILES TO THIS RPROJECT DIRECTORY
# USE THESE OUTPUTS IN DOWNSTREAM SCRIPTS (e.g. for running WBGT models)

#####################################
### PROCESS ORIGINAL CLIMATE DATA ###
#####################################

# Function to process climate data
# Files saved to root of Rproject's root directory
# Move created output files to a sensible location within the project
# e.g. output/climate_data/gfdl-esm4/spp126/hursAdjust_gfdl_2021_2050_ssp126.nc
create_climate_stack<-function(input_dir, mask, output_dir){
  
  # input_dir = (relative) file path to original ISIMIP3b climate data
  # mask = a `raster` object of area of interest. Used to crop and mask global-extent original climate data
  # output_dir = (relative) file path of where to save your output data
  
  files_full_path <- list.files(path = input_dir, pattern='.nc$', all.files=TRUE, full.names=TRUE)
  file_names <- list.files(path = input_dir, pattern='.nc$', all.files=TRUE, full.names=FALSE)
  stk<-raster::stack(files_full_path)
  cropped_ext <- raster::crop(x = stk, y = mask)
  cropped_ext <- raster::mask(cropped_ext, mask)
  writeRaster(cropped_ext, filename = output_dir)
  
}

#################
### GFDL-ESM4 ###
#################

# Humidity
hursAdjust_gfdl_2021_2050_ssp126<-create_climate_stack(input_dir = "original_climate_data/gdfl-esm4/ssp126/hursAdjust/", mask = india_mask, output_dir = "hursAdjust_gfdl_2021_2050_ssp126.nc")
hursAdjust_gfdl_2021_2050_ssp370<-create_climate_stack(input_dir = "original_climate_data/gdfl-esm4/ssp370/hursAdjust/", mask = india_mask, output_dir = "hursAdjust_gfdl_2021_2050_ssp370.nc")
hursAdjust_gfdl_2021_2050_ssp585<-create_climate_stack(input_dir = "original_climate_data/gdfl-esm4/ssp585/hursAdjust/", mask = india_mask, output_dir = "hursAdjust_gfdl_2021_2050_ssp585.nc")

# AirTemp Mean
tasAdjust_gfdl_2021_2050_ssp126<-create_climate_stack(input_dir = "original_climate_data/gdfl-esm4/ssp126/tasAdjust/", mask = india_mask, output_dir = "tasAdjust_gfdl_2021_2050_ssp126.nc")
tasAdjust_gfdl_2021_2050_ssp370<-create_climate_stack(input_dir = "original_climate_data/gdfl-esm4/ssp370/tasAdjust/", mask = india_mask, output_dir = "tasAdjust_gfdl_2021_2050_ssp370.nc")
tasAdjust_gfdl_2021_2050_ssp585<-create_climate_stack(input_dir = "original_climate_data/gdfl-esm4/ssp585/tasAdjust/", mask = india_mask, output_dir = "tasAdjust_gfdl_2021_2050_ssp585.nc")

# AirTemp Max
tasmaxAdjust_gfdl_2021_2050_ssp126<-create_climate_stack(input_dir = "original_climate_data/gdfl-esm4/ssp126/tasmaxAdjust/", mask = india_mask, output_dir = "tasmaxAdjust_gfdl_2021_2050_ssp126.nc")
tasmaxAdjust_gfdl_2021_2050_ssp370<-create_climate_stack(input_dir = "original_climate_data/gdfl-esm4/ssp370/tasmaxAdjust/", mask = india_mask, output_dir = "tasmaxAdjust_gfdl_2021_2050_ssp370.nc")
tasmaxAdjust_gfdl_2021_2050_ssp585<-create_climate_stack(input_dir = "original_climate_data/gdfl-esm4/ssp585/tasmaxAdjust/", mask = india_mask, output_dir = "tasmaxAdjust_gfdl_2021_2050_ssp585.nc")

# Wind Speed
windAdjust_gfdl_2021_2050_ssp126<-create_climate_stack(input_dir = "original_climate_data/gdfl-esm4/ssp126/windAdjust/", mask = india_mask, output_dir = "windAdjust_gfdl_2021_2050_ssp126.nc")
windAdjust_gfdl_2021_2050_ssp370<-create_climate_stack(input_dir = "original_climate_data/gdfl-esm4/ssp370/windAdjust/", mask = india_mask, output_dir = "windAdjust_gfdl_2021_2050_ssp370.nc")
windAdjust_gfdl_2021_2050_ssp585<-create_climate_stack(input_dir = "original_climate_data/gdfl-esm4/ssp585/windAdjust/", mask = india_mask, output_dir = "windAdjust_gfdl_2021_2050_ssp585.nc")

# Short-Wave Solar Radiation
rsdsAdjust_gfdl_2021_2050_ssp126<-create_climate_stack(input_dir = "original_climate_data/gdfl-esm4/ssp126/rsdsAdjust/", mask = india_mask, output_dir = "rsdsAdjust_gfdl_2021_2050_ssp126.nc")
rsdsAdjust_gfdl_2021_2050_ssp370<-create_climate_stack(input_dir = "original_climate_data/gdfl-esm4/ssp370/rsdsAdjust/", mask = india_mask, output_dir = "rsdsAdjust_gfdl_2021_2050_ssp370.nc")
rsdsAdjust_gfdl_2021_2050_ssp585<-create_climate_stack(input_dir = "original_climate_data/gdfl-esm4/ssp585/rsdsAdjust/", mask = india_mask, output_dir = "rsdsAdjust_gfdl_2021_2050_ssp585.nc")


####################
### IPSL-CM6A-LR ###
####################

# Humidity
hursAdjust_ipsl_2021_2050_ssp126<-create_climate_stack(input_dir = "original_climate_data/ipsl-cm6a-lr/ssp126/hursAdjust/", mask = india_mask, output_dir = "hursAdjust_ipsl_2021_2050_ssp126.nc")
hursAdjust_ipsl_2021_2050_ssp370<-create_climate_stack(input_dir = "original_climate_data/ipsl-cm6a-lr/ssp370/hursAdjust/", mask = india_mask, output_dir = "hursAdjust_ipsl_2021_2050_ssp370.nc")
hursAdjust_ipsl_2021_2050_ssp585<-create_climate_stack(input_dir = "original_climate_data/ipsl-cm6a-lr/ssp585/hursAdjust/", mask = india_mask, output_dir = "hursAdjust_ipsl_2021_2050_ssp585.nc")

# AirTemp Mean
tasAdjust_ipsl_2021_2050_ssp126<-create_climate_stack(input_dir = "original_climate_data/ipsl-cm6a-lr/ssp126/tasAdjust/", mask = india_mask, output_dir = "tasAdjust_ipsl_2021_2050_ssp126.nc")
tasAdjust_ipsl_2021_2050_ssp370<-create_climate_stack(input_dir = "original_climate_data/ipsl-cm6a-lr/ssp370/tasAdjust/", mask = india_mask, output_dir = "tasAdjust_ipsl_2021_2050_ssp370.nc")
tasAdjust_ipsl_2021_2050_ssp585<-create_climate_stack(input_dir = "original_climate_data/ipsl-cm6a-lr/ssp585/tasAdjust/", mask = india_mask, output_dir = "tasAdjust_ipsl_2021_2050_ssp585.nc")

# AirTemp Max
tasmaxAdjust_ipsl_2021_2050_ssp126<-create_climate_stack(input_dir = "original_climate_data/ipsl-cm6a-lr/ssp126/tasmaxAdjust/", mask = india_mask, output_dir = "tasmaxAdjust_ipsl_2021_2050_ssp126.nc")
tasmaxAdjust_ipsl_2021_2050_ssp370<-create_climate_stack(input_dir = "original_climate_data/ipsl-cm6a-lr/ssp370/tasmaxAdjust/", mask = india_mask, output_dir = "tasmaxAdjust_ipsl_2021_2050_ssp370.nc")
tasmaxAdjust_ipsl_2021_2050_ssp585<-create_climate_stack(input_dir = "original_climate_data/ipsl-cm6a-lr/ssp585/tasmaxAdjust/", mask = india_mask, output_dir = "tasmaxAdjust_ipsl_2021_2050_ssp585.nc")

# Wind Speed
windAdjust_ipsl_2021_2050_ssp126<-create_climate_stack(input_dir = "original_climate_data/ipsl-cm6a-lr/ssp126/windAdjust/", mask = india_mask, output_dir = "windAdjust_ipsl_2021_2050_ssp126.nc")
windAdjust_ipsl_2021_2050_ssp370<-create_climate_stack(input_dir = "original_climate_data/ipsl-cm6a-lr/ssp370/windAdjust/", mask = india_mask, output_dir = "windAdjust_ipsl_2021_2050_ssp370.nc")
windAdjust_ipsl_2021_2050_ssp585<-create_climate_stack(input_dir = "original_climate_data/ipsl-cm6a-lr/ssp585/windAdjust/", mask = india_mask, output_dir = "windAdjust_ipsl_2021_2050_ssp585.nc")

# Short-Wave Solar Radiation
rsdsAdjust_ipsl_2021_2050_ssp126<-create_climate_stack(input_dir = "original_climate_data/ipsl-cm6a-lr/ssp126/rsdsAdjust/", mask = india_mask, output_dir = "rsdsAdjust_ipsl_2021_2050_ssp126.nc")
rsdsAdjust_ipsl_2021_2050_ssp370<-create_climate_stack(input_dir = "original_climate_data/ipsl-cm6a-lr/ssp370/rsdsAdjust/", mask = india_mask, output_dir = "rsdsAdjust_ipsl_2021_2050_ssp370.nc")
rsdsAdjust_ipsl_2021_2050_ssp585<-create_climate_stack(input_dir = "original_climate_data/ipsl-cm6a-lr/ssp585/rsdsAdjust/", mask = india_mask, output_dir = "rsdsAdjust_ipsl_2021_2050_ssp585.nc")


#####################
### MPI-ESM1-2-HR ###
#####################

# Humidity
hursAdjust_mpi_2021_2050_ssp126<-create_climate_stack(input_dir = "original_climate_data/mpi-esm1-2-hr/ssp126/hursAdjust/", mask = india_mask, output_dir = "hursAdjust_mpi_2021_2050_ssp126.nc")
hursAdjust_mpi_2021_2050_ssp370<-create_climate_stack(input_dir = "original_climate_data/mpi-esm1-2-hr/ssp370/hursAdjust/", mask = india_mask, output_dir = "hursAdjust_mpi_2021_2050_ssp370.nc")
hursAdjust_mpi_2021_2050_ssp585<-create_climate_stack(input_dir = "original_climate_data/mpi-esm1-2-hr/ssp585/hursAdjust/", mask = india_mask, output_dir = "hursAdjust_mpi_2021_2050_ssp585.nc")

# AirTemp Mean
tasAdjust_mpi_2021_2050_ssp126<-create_climate_stack(input_dir = "original_climate_data/mpi-esm1-2-hr/ssp126/tasAdjust/", mask = india_mask, output_dir = "tasAdjust_mpi_2021_2050_ssp126.nc")
tasAdjust_mpi_2021_2050_ssp370<-create_climate_stack(input_dir = "original_climate_data/mpi-esm1-2-hr/ssp370/tasAdjust/", mask = india_mask, output_dir = "tasAdjust_mpi_2021_2050_ssp370.nc")
tasAdjust_mpi_2021_2050_ssp585<-create_climate_stack(input_dir = "original_climate_data/mpi-esm1-2-hr/ssp585/tasAdjust/", mask = india_mask, output_dir = "tasAdjust_mpi_2021_2050_ssp585.nc")

# AirTemp Max
tasmaxAdjust_mpi_2021_2050_ssp126<-create_climate_stack(input_dir = "original_climate_data/mpi-esm1-2-hr/ssp126/tasmaxAdjust/", mask = india_mask, output_dir = "tasmaxAdjust_mpi_2021_2050_ssp126.nc")
tasmaxAdjust_mpi_2021_2050_ssp370<-create_climate_stack(input_dir = "original_climate_data/mpi-esm1-2-hr/ssp370/tasmaxAdjust/", mask = india_mask, output_dir = "tasmaxAdjust_mpi_2021_2050_ssp370.nc")
tasmaxAdjust_mpi_2021_2050_ssp585<-create_climate_stack(input_dir = "original_climate_data/mpi-esm1-2-hr/ssp585/tasmaxAdjust/", mask = india_mask, output_dir = "tasmaxAdjust_mpi_2021_2050_ssp585.nc")

# Wind Speed
windAdjust_mpi_2021_2050_ssp126<-create_climate_stack(input_dir = "original_climate_data/mpi-esm1-2-hr/ssp126/windAdjust/", mask = india_mask, output_dir = "windAdjust_mpi_2021_2050_ssp126.nc")
windAdjust_mpi_2021_2050_ssp370<-create_climate_stack(input_dir = "original_climate_data/mpi-esm1-2-hr/ssp370/windAdjust/", mask = india_mask, output_dir = "windAdjust_mpi_2021_2050_ssp370.nc")
windAdjust_mpi_2021_2050_ssp585<-create_climate_stack(input_dir = "original_climate_data/mpi-esm1-2-hr/ssp585/windAdjust/", mask = india_mask, output_dir = "windAdjust_mpi_2021_2050_ssp585.nc")

# Short-Wave Solar Radiation
rsdsAdjust_mpi_2021_2050_ssp126<-create_climate_stack(input_dir = "original_climate_data/mpi-esm1-2-hr/ssp126/rsdsAdjust/", mask = india_mask, output_dir = "rsdsAdjust_mpi_2021_2050_ssp126.nc")
rsdsAdjust_mpi_2021_2050_ssp370<-create_climate_stack(input_dir = "original_climate_data/mpi-esm1-2-hr/ssp370/rsdsAdjust/", mask = india_mask, output_dir = "rsdsAdjust_mpi_2021_2050_ssp370.nc")
rsdsAdjust_mpi_2021_2050_ssp585<-create_climate_stack(input_dir = "original_climate_data/mpi-esm1-2-hr/ssp585/rsdsAdjust/", mask = india_mask, output_dir = "rsdsAdjust_mpi_2021_2050_ssp585.nc")


##################
### MRI-ESM2-0 ###
##################

# Humidity
hursAdjust_mri_2021_2050_ssp126<-create_climate_stack(input_dir = "original_climate_data/mri-esm2-0/ssp126/hursAdjust/", mask = india_mask, output_dir = "hursAdjust_mri_2021_2050_ssp126.nc")
hursAdjust_mri_2021_2050_ssp370<-create_climate_stack(input_dir = "original_climate_data/mri-esm2-0/ssp370/hursAdjust/", mask = india_mask, output_dir = "hursAdjust_mri_2021_2050_ssp370.nc")
hursAdjust_mri_2021_2050_ssp585<-create_climate_stack(input_dir = "original_climate_data/mri-esm2-0/ssp585/hursAdjust/", mask = india_mask, output_dir = "hursAdjust_mRI_2021_2050_ssp585.nc")

# AirTemp Mean
tasAdjust_mri_2021_2050_ssp126<-create_climate_stack(input_dir = "original_climate_data/mri-esm2-0/ssp126/tasAdjust/", mask = india_mask, output_dir = "tasAdjust_mri_2021_2050_ssp126.nc")
tasAdjust_mri_2021_2050_ssp370<-create_climate_stack(input_dir = "original_climate_data/mri-esm2-0/ssp370/tasAdjust/", mask = india_mask, output_dir = "tasAdjust_mri_2021_2050_ssp370.nc")
tasAdjust_mri_2021_2050_ssp585<-create_climate_stack(input_dir = "original_climate_data/mri-esm2-0/ssp585/tasAdjust/", mask = india_mask, output_dir = "tasAdjust_mri_2021_2050_ssp585.nc")

# AirTemp Max
tasmaxAdjust_mri_2021_2050_ssp126<-create_climate_stack(input_dir = "original_climate_data/mri-esm2-0/ssp126/tasmaxAdjust/", mask = india_mask, output_dir = "tasmaxAdjust_mri_2021_2050_ssp126.nc")
tasmaxAdjust_mri_2021_2050_ssp370<-create_climate_stack(input_dir = "original_climate_data/mri-esm2-0/ssp370/tasmaxAdjust/", mask = india_mask, output_dir = "tasmaxAdjust_mri_2021_2050_ssp370.nc")
tasmaxAdjust_mri_2021_2050_ssp585<-create_climate_stack(input_dir = "original_climate_data/mri-esm2-0/ssp585/tasmaxAdjust/", mask = india_mask, output_dir = "tasmaxAdjust_mri_2021_2050_ssp585.nc")

# Wind Speed
windAdjust_mri_2021_2050_ssp126<-create_climate_stack(input_dir = "original_climate_data/mri-esm2-0/ssp126/windAdjust/", mask = india_mask, output_dir = "windAdjust_mri_2021_2050_ssp126.nc")
windAdjust_mri_2021_2050_ssp370<-create_climate_stack(input_dir = "original_climate_data/mri-esm2-0/ssp370/windAdjust/", mask = india_mask, output_dir = "windAdjust_mri_2021_2050_ssp370.nc")
windAdjust_mri_2021_2050_ssp585<-create_climate_stack(input_dir = "original_climate_data/mri-esm2-0/ssp585/windAdjust/", mask = india_mask, output_dir = "windAdjust_mri_2021_2050_ssp585.nc")

# Short-Wave Solar Radiation
rsdsAdjust_mri_2021_2050_ssp126<-create_climate_stack(input_dir = "original_climate_data/mri-esm2-0/ssp126/rsdsAdjust/", mask = india_mask, output_dir = "rsdsAdjust_mri_2021_2050_ssp126.nc")
rsdsAdjust_mri_2021_2050_ssp370<-create_climate_stack(input_dir = "original_climate_data/mri-esm2-0/ssp370/rsdsAdjust/", mask = india_mask, output_dir = "rsdsAdjust_mri_2021_2050_ssp370.nc")
rsdsAdjust_mri_2021_2050_ssp585<-create_climate_stack(input_dir = "original_climate_data/mri-esm2-0/ssp585/rsdsAdjust/", mask = india_mask, output_dir = "rsdsAdjust_mri_2021_2050_ssp585.nc")

###################
### UKESM1-0-LL ###
###################

# Humidity
hursAdjust_ukes_2021_2050_ssp126<-create_climate_stack(input_dir = "original_climate_data/ukesm1-0-ll/ssp126/hursAdjust/", mask = india_mask, output_dir = "hursAdjust_ukes_2021_2050_ssp126.nc")
hursAdjust_ukes_2021_2050_ssp370<-create_climate_stack(input_dir = "original_climate_data/ukesm1-0-ll/ssp370/hursAdjust/", mask = india_mask, output_dir = "hursAdjust_ukes_2021_2050_ssp370.nc")
hursAdjust_ukes_2021_2050_ssp585<-create_climate_stack(input_dir = "original_climate_data/ukesm1-0-ll/ssp585/hursAdjust/", mask = india_mask, output_dir = "hursAdjust_ukes_2021_2050_ssp585.nc")

# AirTemp Mean
tasAdjust_ukes_2021_2050_ssp126<-create_climate_stack(input_dir = "original_climate_data/ukesm1-0-ll/ssp126/tasAdjust/", mask = india_mask, output_dir = "tasAdjust_ukes_2021_2050_ssp126.nc")
tasAdjust_ukes_2021_2050_ssp370<-create_climate_stack(input_dir = "original_climate_data/ukesm1-0-ll/ssp370/tasAdjust/", mask = india_mask, output_dir = "tasAdjust_ukes_2021_2050_ssp370.nc")
tasAdjust_ukes_2021_2050_ssp585<-create_climate_stack(input_dir = "original_climate_data/ukesm1-0-ll/ssp585/tasAdjust/", mask = india_mask, output_dir = "tasAdjust_ukes_2021_2050_ssp585.nc")

# AirTemp Max
tasmaxAdjust_ukes_2021_2050_ssp126<-create_climate_stack(input_dir = "original_climate_data/ukesm1-0-ll/ssp126/tasmaxAdjust/", mask = india_mask, output_dir = "tasmaxAdjust_ukes_2021_2050_ssp126.nc")
tasmaxAdjust_ukes_2021_2050_ssp370<-create_climate_stack(input_dir = "original_climate_data/ukesm1-0-ll/ssp370/tasmaxAdjust/", mask = india_mask, output_dir = "tasmaxAdjust_ukes_2021_2050_ssp370.nc")
tasmaxAdjust_ukes_2021_2050_ssp585<-create_climate_stack(input_dir = "original_climate_data/ukesm1-0-ll/ssp585/tasmaxAdjust/", mask = india_mask, output_dir = "tasmaxAdjust_ukes_2021_2050_ssp585.nc")

# Wind Speed
windAdjust_ukes_2021_2050_ssp126<-create_climate_stack(input_dir = "original_climate_data/ukesm1-0-ll/ssp126/windAdjust/", mask = india_mask, output_dir = "windAdjust_ukes_2021_2050_ssp126.nc")
windAdjust_ukes_2021_2050_ssp370<-create_climate_stack(input_dir = "original_climate_data/ukesm1-0-ll/ssp370/windAdjust/", mask = india_mask, output_dir = "windAdjust_ukes_2021_2050_ssp370.nc")
windAdjust_ukes_2021_2050_ssp585<-create_climate_stack(input_dir = "original_climate_data/ukesm1-0-ll/ssp585/windAdjust/", mask = india_mask, output_dir = "windAdjust_ukes_2021_2050_ssp585.nc")

# Short-Wave Solar Radiation
rsdsAdjust_ukes_2021_2050_ssp126<-create_climate_stack(input_dir = "original_climate_data/ukesm1-0-ll/ssp126/rsdsAdjust/", mask = india_mask, output_dir = "rsdsAdjust_ukesm1_2021_2050_ssp126.nc")
rsdsAdjust_ukes_2021_2050_ssp370<-create_climate_stack(input_dir = "original_climate_data/ukesm1-0-ll/ssp370/rsdsAdjust/", mask = india_mask, output_dir = "rsdsAdjust_ukesm1_2021_2050_ssp370.nc")
rsdsAdjust_ukes_2021_2050_ssp585<-create_climate_stack(input_dir = "original_climate_data/ukesm1-0-ll/ssp585/rsdsAdjust/", mask = india_mask, output_dir = "rsdsAdjust_ukesm1_2021_2050_ssp585.nc")






