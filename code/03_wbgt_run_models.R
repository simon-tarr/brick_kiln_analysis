library(tidyverse)
library(HeatStress)
library(doParallel)
library(foreach)

# Function to run WBGT models for various GCM and emissions scenarios
# Some models (e.g. Bernard) require a parallel backend - set number of cores appropriately
run_wbgt<-function(gcm, scenario, wbgt_model, pts, cores){
  
  if (cores > parallel::detectCores()) {
    
    stop(paste0("Execution halted! You have specifed more cores than your computer has! Please reduce the number
         of cores. parallel::detectCores() says you have ", parallel::detectCores(), " cores. It is recommended
         that you run this function with ", parallel::detectCores()-2, " cores so that there's some CPU overhead for other
         activities on your computer."))
  }
  
  # gcm = Character. Shortname of the GCM e.g. GFDL-ESM2 would become gfdl. Must be one of: gfdl, ipsl, mpi, mri or ukes
  # scenario = Character. The emissions scenario of interest. Can be one of: ssp126, ssp370, ssp585
  # wbgt_model = Character. The WBGT model you want to run. Can be one of: stull, bernard, liljegren
  # pts = Tibble. Dataframe containing coordinates of cell centroids
  # cores = The number of CPU cores to use for parallel model processing
  
  obj = get(paste0(gcm, "_", scenario))
  wbgt_output<-list()
  
  # Create vector of dates to sensibly name each layer in the rasterstack
  # Dates span the length of the study 2021 - 2050
  days<-seq(ymd("2021-01-01"), ymd("2050-12-31"), by = "days")
  
  # Grab XY coordinates only from pts dataframe
  pts<-as_tibble(pts[, 1:2])
  
  if (wbgt_model == "stull"){
    
    print("Running Stull (2011) Model...")
    print("Note: not running in parallel due to the fast execution time of the Stull model.")
    
    for (i in 1:length(obj)){
      wbgt_output[[i]]<-as_tibble(wbt.Stull(tas = obj[[i]]$tasmax, hurs = obj[[i]]$hurs)) %>% 
        mutate(x=pts[i,]$x) %>% 
        mutate(y=pts[i,]$y) %>% 
        rename(wbgt = value) %>% 
        add_column(date = days) %>% 
        add_column(src = paste0(gcm, "_", scenario)) %>% 
        left_join(., regional_data, by = c("x", "y")) %>% 
        dplyr::select(x, y, date, country, adm1, adm2, adm3, wbgt, src)

    }
    
    return(wbgt_output)
    
  } else
    
    if (wbgt_model == "bernard"){
      
      print("Running Bernard (1999) Model...")
      
      cl<-makeCluster(cores)
      registerDoParallel(cl)
      
      wbgt_output<-foreach(i = 1:length(obj), .packages = c("HeatStress", "dplyr", "tibble")) %dopar% {
        wbgt_output[[i]]<-as_tibble(wbgt.Bernard(tas = obj[[i]]$tasmax, dewp = obj[[i]]$dewp)) %>% 
          mutate(x=pts[i,]$x) %>% 
          mutate(y=pts[i,]$y) %>% 
          rename(wbgt = data) %>% 
          add_column(date = days) %>% 
          add_column(src = paste0(gcm, "_", scenario)) %>% 
          select(date, x, y, Tpwb, wbgt, src)
      }
      
      stopCluster(cl)
      
      return(wbgt_output)
      
    } else
      
      if (wbgt_model == "liljegren"){
        
        print("Running Liljegren (2008) Model...")
        
        cl<-makeCluster(cores)
        registerDoParallel(cl)
        
        wbgt_output<-foreach(i = 1:length(obj), .packages = c("HeatStress", "dplyr", "tibble")) %dopar% {
          wbgt_output[[i]]<-as_tibble(wbgt.Liljegren(tas = obj[[i]]$tasmax, 
                                                     dewp = obj[[i]]$dewp, 
                                                     wind = obj[[i]]$wind,
                                                     radiation = obj[[i]]$solar,
                                                     dates = obj[[i]]$date,
                                                     lon = pts[i, ]$x,
                                                     lat = pts[i, ]$y)) %>% 
            mutate(x=pts[i,]$x) %>% 
            mutate(y=pts[i,]$y) %>% 
            add_column(date = days) %>% 
            add_column(src = paste0(gcm, "_", scenario)) %>% 
            rename(wbgt = data)
        }
        
        stopCluster(cl)
        
        return(wbgt_output)
        
        
      }
  
}

# Function to convert WBGT model outputs into the format specified by Edgar
# User can specify whether to compute monthly or annual outputs
# as well as specifying the temperature threshold over which a day is 
# counted as 'a heatstress day'
create_master_results_df<-function(input, method, temperature_threshold){
  
  # input = Character vector of input WBGT models to concatenate into dataframe
  # method = How to summarise the data. Must be either: "year" or "month"
  # temperature_threshold = temperature (in Celsius) over which heatstress days are counted
  
  output<-list()
  
  for (r in 1:length(input)){
    
    annual_summary_df<-list()
    data<-get(input[r])
    
    for (i in 1:length(data)){
      
      point_data<-data[[i]][1,] %>% 
        dplyr::select(-date, -wbgt)
      
      x<-data[[i]] %>% 
        group_by(date = lubridate::floor_date(date, method)) %>% 
        summarise(days_over = sum(wbgt>temperature_threshold))
      
      annual_summary_df[[i]]<-as_tibble(cbind(point_data, x))
      
    }
    
    output[[r]]<-map_dfr(annual_summary_df, bind_rows) 
    
  }
  

#################
### GFDL-ESM4 ###
#################

# Create WBGT data, create a bound/transformed dataframe (trs)
gfdl_ssp126_stull<-run_wbgt(gcm="gfdl", scenario="ssp126", wbgt_model = "stull", pts=pts, cores = 6)
gfdl_ssp370_stull<-run_wbgt(gcm="gfdl", scenario="ssp370", wbgt_model = "stull", pts=pts, cores = 6)
gfdl_ssp585_stull<-run_wbgt(gcm="gfdl", scenario="ssp585", wbgt_model = "stull", pts=pts, cores = 6)

gfdl_ssp126_stull_trs<-map_dfr(gfdl_ssp126_stull, bind_rows) 
gfdl_ssp370_stull_trs<-map_dfr(gfdl_ssp370_stull, bind_rows) 
gfdl_ssp585_stull_trs<-map_dfr(gfdl_ssp585_stull, bind_rows)

gfdl_ssp126_bernard<-run_wbgt(gcm="gfdl", scenario="ssp126", wbgt_model = "bernard", pts=pts, cores = 6)
gfdl_ssp370_bernard<-run_wbgt(gcm="gfdl", scenario="ssp370", wbgt_model = "bernard", pts=pts, cores = 6)
gfdl_ssp585_bernard<-run_wbgt(gcm="gfdl", scenario="ssp585", wbgt_model = "bernard", pts=pts, cores = 6)

gfdl_ssp126_bernard_trs<-map_dfr(gfdl_ssp126_bernard, bind_rows) 
gfdl_ssp370_bernard_trs<-map_dfr(gfdl_ssp370_bernard, bind_rows) 
gfdl_ssp585_bernard_trs<-map_dfr(gfdl_ssp585_bernard, bind_rows)

ptm <- proc.time()
gfdl_ssp126_liljegren<-run_wbgt(gcm="gfdl", scenario="ssp126", wbgt_model = "liljegren", pts=pts, cores = 6)
proc.time() - ptm

####################
### IPSL-CM6A-LR ###
####################

ipsl_ssp126_stull<-run_wbgt(gcm="ipsl", scenario="ssp126", wbgt_model = "stull", pts=pts, cores = 6)
ipsl_ssp370_stull<-run_wbgt(gcm="ipsl", scenario="ssp370", wbgt_model = "stull", pts=pts, cores = 6)
ipsl_ssp585_stull<-run_wbgt(gcm="ipsl", scenario="ssp585", wbgt_model = "stull", pts=pts, cores = 6)

ipsl_ssp126_stull_trs<-map_dfr(ipsl_ssp126_stull, bind_rows) 
ipsl_ssp370_stull_trs<-map_dfr(ipsl_ssp370_stull, bind_rows) 
ipsl_ssp585_stull_trs<-map_dfr(ipsl_ssp585_stull, bind_rows)


#####################
### MPI-ESM1-2-HR ###
#####################

mpi_ssp126_stull<-run_wbgt(gcm="mpi", scenario="ssp126", wbgt_model = "stull", pts=pts, cores = 6)
mpi_ssp370_stull<-run_wbgt(gcm="mpi", scenario="ssp370", wbgt_model = "stull", pts=pts, cores = 6)
mpi_ssp585_stull<-run_wbgt(gcm="mpi", scenario="ssp585", wbgt_model = "stull", pts=pts, cores = 6)

mpi_ssp126_stull_trs<-map_dfr(mpi_ssp126_stull, bind_rows) 
mpi_ssp370_stull_trs<-map_dfr(mpi_ssp370_stull, bind_rows) 
mpi_ssp585_stull_trs<-map_dfr(mpi_ssp585_stull, bind_rows)

##################
### MRI-ESM2-0 ###
##################

mri_ssp126_stull<-run_wbgt(gcm="mri", scenario="ssp126", wbgt_model = "stull", pts=pts, cores = 6)
mri_ssp370_stull<-run_wbgt(gcm="mri", scenario="ssp370", wbgt_model = "stull", pts=pts, cores = 6)
mri_ssp585_stull<-run_wbgt(gcm="mri", scenario="ssp585", wbgt_model = "stull", pts=pts, cores = 6)

mri_ssp126_stull_trs<-map_dfr(mri_ssp126_stull, bind_rows) 
mri_ssp370_stull_trs<-map_dfr(mri_ssp370_stull, bind_rows) 
mri_ssp585_stull_trs<-map_dfr(mri_ssp585_stull, bind_rows)

#################
### UKESM1-LL ###
#################

ukes_ssp126_stull<-run_wbgt(gcm="ukes", scenario="ssp126", wbgt_model = "stull", pts=pts, cores = 6)
ukes_ssp370_stull<-run_wbgt(gcm="ukes", scenario="ssp370", wbgt_model = "stull", pts=pts, cores = 6)
ukes_ssp585_stull<-run_wbgt(gcm="ukes", scenario="ssp585", wbgt_model = "stull", pts=pts, cores = 6)

ukes_ssp126_stull_trs<-map_dfr(ukes_ssp126_stull, bind_rows) 
ukes_ssp370_stull_trs<-map_dfr(ukes_ssp370_stull, bind_rows) 
ukes_ssp585_stull_trs<-map_dfr(ukes_ssp585_stull, bind_rows)

###################################################
### CREATE CONCATENATED DATAFRAMES OF WBGT DATA ###
###################################################


  rtn<-map_dfr(output, bind_rows) %>% left_join(., kilns, by = c("x", "y")) %>% 
    dplyr::select(x, y, country.x, adm1.x, adm2.x, adm3.x, src, date, days_over, kiln_count)
  
  return(rtn)
  
}


gfdl_annual<-create_master_results_df(input=c("gfdl_ssp126_stull", 
                                    "gfdl_ssp370_stull", 
                                    "gfdl_ssp585_stull"), method = "year", temperature_threshold = 30)

gfdl_monthly<-create_master_results_df(input=c("gfdl_ssp126_stull", 
                                              "gfdl_ssp370_stull", 
                                              "gfdl_ssp585_stull"), method = "month", temperature_threshold = 30)


