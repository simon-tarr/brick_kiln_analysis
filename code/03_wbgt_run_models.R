library(tidyverse)
library(HeatStress)
library(doParallel)
library(foreach)

# Function to run WBGT models for various GCM and emissions scenarios
# Some models (e.g. Bernard) require a parallel backend - set number of cores appropriately
run_wbgt<-function(gcm, scenario, wbgt_model, pts, cores){
  
  # gcm = Character. Shortname of the GCM e.g. GFDL-ESM2 would become gfdl. Must be one of: gfdl, ipsl, mpi, mri or ukes
  # scenario = Character. The emissions scenario of interest. Can be one of: ssp126, ssp370, ssp585
  # wbgt_model = Character. The WBGT model you want to run. Can be one of: stull, bernard, liljegren
  # pts = Tibble. Dataframe containing coordinates of cell centroids
  # cores = The number of CPU cores to use for parallel model processing
  
  if (cores > parallel::detectCores()) {
    
    stop(paste0("Execution halted! You have specifed more cores than your computer has! Please reduce the number
         of cores. parallel::detectCores() says you have ", parallel::detectCores(), " cores. It is recommended
         that you run this function with ", parallel::detectCores()-2, " cores so that there's some CPU overhead for other
         activities on your computer."))
  }
  
  obj = get(paste(gcm, scenario, sep="_"))
  wbgt_output<-list()
  
  # Create vector of dates to sensibly name each layer in the rasterstack
  # Dates span the length of the study 2021 - 2050
  days<-seq(ymd("2021-01-01"), ymd("2050-12-31"), by = "days")
  
  # Grab XY coordinates only from pts dataframe
  pts<-as_tibble(pts[, 1:2])
  
  # Define parallel cluster
  cl<-makeCluster(cores)
  registerDoParallel(cl)
  
  if (wbgt_model == "stull"){
    
    print("Running Stull (2011) Model...")
    print("Note: not running in parallel due to the fast execution time of the Stull model.")
    
    for (i in 1:length(obj)){
      
      wbgt_output[[i]]<-as_tibble(wbt.Stull(tas = obj[[i]]$tasmax, hurs = obj[[i]]$hurs)) %>% 
        mutate(x=pts[i,]$x) %>% 
        mutate(y=pts[i,]$y) %>% 
        rename(wbgt = value) %>% 
        add_column(date = days) %>% 
        add_column(gcm = gcm) %>% 
        add_column(scenario = scenario) %>% 
        add_column(model = wbgt_model) %>% 
        left_join(., region_data, by = c("x", "y")) %>% 
        add_column(Tpwb = NA) %>% 
        add_column(Tnwb = NA) %>% 
        add_column(Tg = NA) %>% 
        dplyr::select(x, y, date, country, adm1, adm2, adm3, wbgt, Tpwb, Tnwb, Tg, gcm, scenario, model)
      
    }
    
    return(wbgt_output)
    
  } else
    
    if (wbgt_model == "bernard"){
      
      print("Running Bernard (1999) Model...")
      
      wbgt_output<-foreach(i = 1:length(obj), .packages = c("HeatStress", "dplyr", "tibble"), .export = ls(globalenv())) %dopar% {
        
        wbgt_output[[i]]<-as_tibble(wbgt.Bernard(tas = obj[[i]]$tasmax, dewp = obj[[i]]$dewp)) %>% 
          mutate(x=pts[i,]$x) %>% 
          mutate(y=pts[i,]$y) %>% 
          rename(wbgt = data) %>% 
          add_column(date = days) %>% 
          add_column(gcm = gcm) %>% 
          add_column(scenario = scenario) %>% 
          add_column(model = wbgt_model) %>% 
          add_column(Tnwb = NA) %>% 
          add_column(Tg = NA) %>% 
          left_join(., region_data, by = c("x", "y")) %>% 
          dplyr::select(x, y, date, country, adm1, adm2, adm3, wbgt, Tpwb, Tnwb, Tg, gcm, scenario, model)
        
      }
      
      stopCluster(cl)
      
      return(wbgt_output)
      
    } else
      
      if (wbgt_model == "liljegren"){
        
        print("Running Liljegren (2008) Model...")
        
        wbgt_output<-foreach(i = 1:length(obj), .packages = c("HeatStress", "dplyr", "tibble"), .export = ls(globalenv())) %dopar% {
          
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
            add_column(gcm = gcm) %>% 
            add_column(scenario = scenario) %>% 
            add_column(model = wbgt_model) %>% 
            rename(wbgt = data) %>% 
            add_column(Tpwb = NA) %>% 
            left_join(., region_data, by = c("x", "y")) %>% 
            dplyr::select(x, y, date, country, adm1, adm2, adm3, wbgt, Tpwb, Tnwb, Tg, gcm, scenario, model)
        }

        return(wbgt_output)
      }
  
  stopCluster(cl)
  
}



# Function to convert WBGT model outputs into the format specified by Edgar
# User can specify whether to compute monthly or annual outputs
# as well as specifying the temperature threshold over which a day is 
# counted as 'a heatstress day'

# Note this function only works on the wbgt column! This is because WBGT is an
# output from all implementations (Stull, Bernard and Liljegren).
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
        dplyr::select(-date, -wbgt, -Tpwb, -Tnwb, -Tg)
      
      x<-data[[i]] %>% 
        group_by(date = lubridate::floor_date(date, method)) %>% 
        dplyr::summarise(days_over = sum(wbgt > temperature_threshold))
      
      annual_summary_df[[i]]<-as_tibble(cbind(point_data, x))
      
    }
    
    output[[r]]<-map_dfr(annual_summary_df, bind_rows) 
    
  }
  
  rtn<-map_dfr(output, bind_rows) %>% left_join(., kilns, by = c("x", "y")) %>% 
    dplyr::select(x, y, country.x, adm1.x, adm2.x, adm3.x, gcm, scenario, model, date, days_over, kiln_count)
  
  return(rtn)
  
}

#################
### GFDL-ESM4 ###
#################

# Create WBGT data, create a bound/transformed dataframe (trs)
gfdl_ssp126_stull<-run_wbgt(gcm="gfdl", scenario="ssp126", wbgt_model = "stull", pts=pts, cores = 6)
gfdl_ssp370_stull<-run_wbgt(gcm="gfdl", scenario="ssp370", wbgt_model = "stull", pts=pts, cores = 6)
gfdl_ssp585_stull<-run_wbgt(gcm="gfdl", scenario="ssp585", wbgt_model = "stull", pts=pts, cores = 6)

# gfdl_ssp126_stull_trs<-map_dfr(gfdl_ssp126_stull, bind_rows) 
# gfdl_ssp370_stull_trs<-map_dfr(gfdl_ssp370_stull, bind_rows) 
# gfdl_ssp585_stull_trs<-map_dfr(gfdl_ssp585_stull, bind_rows)

gfdl_ssp126_bernard<-run_wbgt(gcm="gfdl", scenario="ssp126", wbgt_model = "bernard", pts=pts, cores = 6)
gfdl_ssp370_bernard<-run_wbgt(gcm="gfdl", scenario="ssp370", wbgt_model = "bernard", pts=pts, cores = 6)
gfdl_ssp585_bernard<-run_wbgt(gcm="gfdl", scenario="ssp585", wbgt_model = "bernard", pts=pts, cores = 6)

# gfdl_ssp126_bernard_trs<-map_dfr(gfdl_ssp126_bernard, bind_rows) 
# gfdl_ssp370_bernard_trs<-map_dfr(gfdl_ssp370_bernard, bind_rows) 
# gfdl_ssp585_bernard_trs<-map_dfr(gfdl_ssp585_bernard, bind_rows)

gfdl_ssp126_liljegren<-run_wbgt(gcm="gfdl", scenario="ssp126", wbgt_model = "liljegren", pts=pts, cores = 6)
gfdl_ssp370_liljegren<-run_wbgt(gcm="gfdl", scenario="ssp370", wbgt_model = "liljegren", pts=pts, cores = 6)
gfdl_ssp585_liljegren<-run_wbgt(gcm="gfdl", scenario="ssp585", wbgt_model = "liljegren", pts=pts, cores = 6)

####################
### IPSL-CM6A-LR ###
####################

ipsl_ssp126_stull<-run_wbgt(gcm="ipsl", scenario="ssp126", wbgt_model = "stull", pts=pts, cores = 6)
ipsl_ssp370_stull<-run_wbgt(gcm="ipsl", scenario="ssp370", wbgt_model = "stull", pts=pts, cores = 6)
ipsl_ssp585_stull<-run_wbgt(gcm="ipsl", scenario="ssp585", wbgt_model = "stull", pts=pts, cores = 6)

# ipsl_ssp126_stull_trs<-map_dfr(ipsl_ssp126_stull, bind_rows) 
# ipsl_ssp370_stull_trs<-map_dfr(ipsl_ssp370_stull, bind_rows) 
# ipsl_ssp585_stull_trs<-map_dfr(ipsl_ssp585_stull, bind_rows)

ipsl_ssp126_bernard<-run_wbgt(gcm="ipsl", scenario="ssp126", wbgt_model = "bernard", pts=pts, cores = 6)
ipsl_ssp370_bernard<-run_wbgt(gcm="ipsl", scenario="ssp370", wbgt_model = "bernard", pts=pts, cores = 6)
ipsl_ssp585_bernard<-run_wbgt(gcm="ipsl", scenario="ssp585", wbgt_model = "bernard", pts=pts, cores = 6)

ipsl_ssp126_liljegren<-run_wbgt(gcm="ipsl", scenario="ssp126", wbgt_model = "liljegren", pts=pts, cores = 6)
ipsl_ssp370_liljegren<-run_wbgt(gcm="ipsl", scenario="ssp370", wbgt_model = "liljegren", pts=pts, cores = 6)
ipsl_ssp585_liljegren<-run_wbgt(gcm="ipsl", scenario="ssp585", wbgt_model = "liljegren", pts=pts, cores = 6)


#####################
### MPI-ESM1-2-HR ###
#####################

mpi_ssp126_stull<-run_wbgt(gcm="mpi", scenario="ssp126", wbgt_model = "stull", pts=pts, cores = 6)
mpi_ssp370_stull<-run_wbgt(gcm="mpi", scenario="ssp370", wbgt_model = "stull", pts=pts, cores = 6)
mpi_ssp585_stull<-run_wbgt(gcm="mpi", scenario="ssp585", wbgt_model = "stull", pts=pts, cores = 6)

# mpi_ssp126_stull_trs<-map_dfr(mpi_ssp126_stull, bind_rows) 
# mpi_ssp370_stull_trs<-map_dfr(mpi_ssp370_stull, bind_rows) 
# mpi_ssp585_stull_trs<-map_dfr(mpi_ssp585_stull, bind_rows)

mpi_ssp126_bernard<-run_wbgt(gcm="mpi", scenario="ssp126", wbgt_model = "bernard", pts=pts, cores = 6)
mpi_ssp370_bernard<-run_wbgt(gcm="mpi", scenario="ssp370", wbgt_model = "bernard", pts=pts, cores = 6)
mpi_ssp585_bernard<-run_wbgt(gcm="mpi", scenario="ssp585", wbgt_model = "bernard", pts=pts, cores = 6)

mpi_ssp126_liljegren<-run_wbgt(gcm="mpi", scenario="ssp126", wbgt_model = "liljegren", pts=pts, cores = 6)
mpi_ssp370_liljegren<-run_wbgt(gcm="mpi", scenario="ssp370", wbgt_model = "liljegren", pts=pts, cores = 6)
mpi_ssp585_liljegren<-run_wbgt(gcm="mpi", scenario="ssp585", wbgt_model = "liljegren", pts=pts, cores = 6)

##################
### MRI-ESM2-0 ###
##################

mri_ssp126_stull<-run_wbgt(gcm="mri", scenario="ssp126", wbgt_model = "stull", pts=pts, cores = 6)
mri_ssp370_stull<-run_wbgt(gcm="mri", scenario="ssp370", wbgt_model = "stull", pts=pts, cores = 6)
mri_ssp585_stull<-run_wbgt(gcm="mri", scenario="ssp585", wbgt_model = "stull", pts=pts, cores = 6)

# mri_ssp126_stull_trs<-map_dfr(mri_ssp126_stull, bind_rows) 
# mri_ssp370_stull_trs<-map_dfr(mri_ssp370_stull, bind_rows) 
# mri_ssp585_stull_trs<-map_dfr(mri_ssp585_stull, bind_rows)

mri_ssp126_bernard<-run_wbgt(gcm="mri", scenario="ssp126", wbgt_model = "bernard", pts=pts, cores = 6)
mri_ssp370_bernard<-run_wbgt(gcm="mri", scenario="ssp370", wbgt_model = "bernard", pts=pts, cores = 6)
mri_ssp585_bernard<-run_wbgt(gcm="mri", scenario="ssp585", wbgt_model = "bernard", pts=pts, cores = 6)

mri_ssp126_liljegren<-run_wbgt(gcm="mri", scenario="ssp126", wbgt_model = "liljegren", pts=pts, cores = 6)
mri_ssp370_liljegren<-run_wbgt(gcm="mri", scenario="ssp370", wbgt_model = "liljegren", pts=pts, cores = 6)
mri_ssp585_liljegren<-run_wbgt(gcm="mri", scenario="ssp585", wbgt_model = "liljegren", pts=pts, cores = 6)


#################
### UKESM1-LL ###
#################

ukes_ssp126_stull<-run_wbgt(gcm="ukes", scenario="ssp126", wbgt_model = "stull", pts=pts, cores = 6)
ukes_ssp370_stull<-run_wbgt(gcm="ukes", scenario="ssp370", wbgt_model = "stull", pts=pts, cores = 6)
ukes_ssp585_stull<-run_wbgt(gcm="ukes", scenario="ssp585", wbgt_model = "stull", pts=pts, cores = 6)

# ukes_ssp126_stull_trs<-map_dfr(ukes_ssp126_stull, bind_rows) 
# ukes_ssp370_stull_trs<-map_dfr(ukes_ssp370_stull, bind_rows) 
# ukes_ssp585_stull_trs<-map_dfr(ukes_ssp585_stull, bind_rows)

ukes_ssp126_bernard<-run_wbgt(gcm="ukes", scenario="ssp126", wbgt_model = "bernard", pts=pts, cores = 6)
ukes_ssp370_bernard<-run_wbgt(gcm="ukes", scenario="ssp370", wbgt_model = "bernard", pts=pts, cores = 6)
ukes_ssp585_bernard<-run_wbgt(gcm="ukes", scenario="ssp585", wbgt_model = "bernard", pts=pts, cores = 6)

ukes_ssp126_liljegren<-run_wbgt(gcm="ukes", scenario="ssp126", wbgt_model = "liljegren", pts=pts, cores = 6)
ukes_ssp370_liljegren<-run_wbgt(gcm="ukes", scenario="ssp370", wbgt_model = "liljegren", pts=pts, cores = 6)
ukes_ssp585_liljegren<-run_wbgt(gcm="ukes", scenario="ssp585", wbgt_model = "liljegren", pts=pts, cores = 6)


###################################################
### CREATE CONCATENATED DATAFRAMES OF WBGT DATA ###
###################################################

# Use create_master_results_df() to combine whatever datasets you like
# while specifying the temperature threshold over which to count 'heatstress days'
# Some examples of how to combine are below

# Combine SSP126 - Stull models only
example_1<-create_master_results_df(input=c("gfdl_ssp126_stull", 
                                              "ipsl_ssp126_stull", 
                                              "mpi_ssp126_stull",
                                              "mri_ssp126_stull",
                                              "ukes_ssp126_stull"), 
                                      method = "year", temperature_threshold = 30)

# If you want to make the data 'wider' you can do so with the following
gfdl_annual %>% 
  pivot_wider(values_from = days_over, names_from = c(gcm, scenario, model))


