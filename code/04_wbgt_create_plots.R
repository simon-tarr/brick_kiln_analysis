
library(raster)
library(tidyverse)
library(colorRamps)
library(patchwork)
library(janitor)
library(ggpmisc)

# RUNNING THE CODE IN THIS SCRIPT. YOU'LL GET OBJECT NOT
# FOUND ERRORS IF YOU HAVEN'T RUN WBGT MODELS FOR THE APPROPRIATE
# GCM/EMISSIONS SCENARIO/WBGT IMPLEMENTATION

# Load in India shapefile, create mask
shp <-raster::shapefile("input/study_area/study_area_adm3_noPAK.shp")
ext <- floor(extent(shp))
rr <- raster(ext, res = 0.5)
india_mask <- rasterize(shp, rr, field = 1)

# Load in brick kiln data
kilns<-read_csv("input/kiln_locations_processed.csv")

# Plot kiln heatmap to see where most kilns are
plot(rasterize(x=kilns[, 1:2], # lon-lat data
               y=india_mask, # raster object
               field=kilns$kiln_count))


# Create dataframes of various summarised heatstress data
create_heatstress_df<-function(gcm="gfdl", scenario="ssp126", wbgt_model="stull"){
  
  gcm_scenario<-paste0(gcm, scenario)
  gcm_scenario<-list()
  
  if (!exists(paste(gcm, scenario, wbgt_model, "trs", sep="_"))) {
    stop(paste0("The object ", paste(gcm, scenario, wbgt_model, "trs", sep="_"), 
                " doesn't exist. Please ensure you've run 03_wbgt_run_models.R for this GCM and emissions scenario."))
  }
  
  gcm_scenario$foo<-get(paste(gcm, scenario, wbgt_model, "trs", sep="_")) %>% 
    left_join(., pts, by = c("x", "y")) %>% 
    group_by(year = lubridate::floor_date(date, "year"), country) %>% 
    summarise(days_over = sum(wbgt > 30), n = n())
  
  # Count number of days per year WBGT > 30c
  # Count the number of 'kiln days' where WBGT >30c
  # Bind data and plot with ggplot
  gcm_scenario$heatstress_days<-get(paste(gcm, scenario, wbgt_model, "trs", sep="_")) %>% 
    group_by(year = lubridate::floor_date(date, "year")) %>% 
    summarise(days_over = sum(wbgt > 30), n = n()) %>% mutate(src = paste(gcm, scenario, sep = "_"))

  gcm_scenario$kiln_days<-get(paste(gcm, scenario, wbgt_model, "trs", sep="_")) %>% 
    group_by(year = lubridate::floor_date(date, "year")) %>% 
    left_join(., kilns, by = c("x", "y")) %>% 
    summarise(kiln_days = sum(kiln_count[wbgt>30])) %>% mutate(src = paste(gcm, scenario, sep = "_"))

  names(gcm_scenario)<-c("annual_heatstress_days", "annual_heatstress_kiln_days", "foo")
  return(gcm_scenario)
  
}

create_plots<-function(gcm, map_scenario, wbgt_model, heatstress_bound, kilndays_bound){
  
  if (!exists(paste(gcm, map_scenario, wbgt_model, sep="_"))) {
    stop(paste0("The object ", paste(gcm, map_scenario, wbgt_model, "trs", sep="_"), 
                " doesn't exist. Please ensure you've run 02_wbgt_create_climate_dataframe.R for this GCM and emissions scenario
                to create the raster map of heatstress days."))
  }
  
  my.formula <- y ~ x
  
  # Plot annual heat stress days data
  heatstress_days_plot<-ggplot(heatstress_bound, aes(x=year, y=days_over, color=src)) + 
    geom_point(alpha=0.6, size=2)+
    geom_smooth(method=lm, alpha = 0.1, aes(fill = src)) +
    labs(x = "Year", y = "No. days per year > WBGT 30c") +
    theme_linedraw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
    stat_poly_eq(formula = my.formula, 
                 method = "lm",
                 aes(label = paste(..adj.rr.label.., ..p.value.label.., sep = "~~~")), 
                 parse = TRUE)
  
  # Plot annual 'kiln days' data
  kiln_days_plot<-ggplot(kilndays_bound, aes(x=year, y=kiln_days, color=src)) + 
    geom_point(alpha=0.6, size=2) +
    geom_smooth(method=lm, alpha = 0.1, aes(fill = src)) +
    labs(x = "Year", y = "Kiln days per year > WBGT 30c") +
    theme_linedraw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
    stat_poly_eq(formula = my.formula, 
                 method = "lm",
                 aes(label = paste(..adj.rr.label.., ..p.value.label.., sep = "~~~")), 
                 parse = TRUE)
  
  # MAP PLOT 
  # Compare average of first 5 years of hot days
  # against average of the final 5 years of the study
  climate_data<-get(paste(gcm, map_scenario, wbgt_model, sep="_"))
  mean_dat<-list()
  for (i in 1:length(climate_data)){
    
    start<-climate_data[[i]] %>% filter(date >="2021-01-01" & date <"2025-12-31") %>% 
      group_by(year = lubridate::floor_date(date, "year")) %>% 
      summarise(days_over = sum(wbgt>30)) %>% 
      summarise(start_mean = mean(days_over), n = n())
    
    end<-climate_data[[i]] %>% filter(date >="2046-01-01" & date <"2050-12-31") %>% 
      group_by(year = lubridate::floor_date(date, "year")) %>% 
      summarise(days_over = sum(wbgt>30)) %>% 
      summarise(end_mean = mean(days_over), n = n())
    
    mean_dat[[i]]<-as_tibble(cbind(start$start_mean, end$end_mean))
    
  }
  
  df<-map_dfr(mean_dat, bind_rows) %>% 
    add_column(x = pts$x) %>% add_column(y = pts$y) %>% 
    rename(start = V1) %>% rename(end = V2)
  
  # Load in shapefile of study area, simplfy for quicker plotting
  # Simplifying makes the shp less complex (fewer vertices)
  shp <-raster::shapefile("input/brickbelt_shp/india_belt.shp")
  regions_gSimplify <- rgeos::gSimplify(shp, tol = 0.05, topologyPreserve = TRUE)
  
  map<-ggplot(data=df, aes(x=x, y=y)) + geom_raster(aes(fill=end-start), alpha=0.9) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
    theme_linedraw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "grey85")) +
    geom_polygon(data=regions_gSimplify, aes(x=long, y=lat, group=group), color="black", alpha=0, linetype="dashed", size=0.3) +
    annotate("text", x=min(df$x)+1, y=min(df$y), hjust=.2, label=toupper(map_scenario))
  
  
  plt<-heatstress_days_plot+kiln_days_plot+map
  return(plt)
  
}

#############
### STULL ###
#############

#################
### GFDL-ESM4 ###
#################

gfdl_ssp126_stull_wbgt<-create_heatstress_df(gcm = "gfdl", scenario = "ssp126", wbgt_model = "stull")
gfdl_ssp370_stull_wbgt<-create_heatstress_df(gcm = "gfdl", scenario = "ssp370", wbgt_model = "stull")
gfdl_ssp585_stull_wbgt<-create_heatstress_df(gcm = "gfdl", scenario = "ssp585", wbgt_model = "stull")

# Bind data together so all emissions scenarios can be plotted in 1 panel
gfdl_stull_heatstress_bound<-bind_rows(gfdl_ssp126_stull_wbgt$annual_heatstress_days, 
                            gfdl_ssp370_stull_wbgt$annual_heatstress_days, 
                            gfdl_ssp585_stull_wbgt$annual_heatstress_days)

gfdl_stull_kilndays_bound<-bind_rows(gfdl_ssp126_stull_wbgt$annual_heatstress_kiln_days, 
                                      gfdl_ssp370_stull_wbgt$annual_heatstress_kiln_days, 
                                      gfdl_ssp585_stull_wbgt$annual_heatstress_kiln_days)

gfdl_plots<-create_plots(gcm = "gfdl", 
                wbgt_model = "stull",
                map_scenario = "ssp585",
                heatstress_bound = gfdl_stull_heatstress_bound, 
                kilndays_bound = gfdl_stull_kilndays_bound)


####################
### IPSL-CM6A-LR ###
####################

ipsl_ssp126_stull_wbgt<-create_heatstress_df(gcm = "ipsl", scenario = "ssp126", wbgt_model = "stull")
ipsl_ssp370_stull_wbgt<-create_heatstress_df(gcm = "ipsl", scenario = "ssp370", wbgt_model = "stull")
ipsl_ssp585_stull_wbgt<-create_heatstress_df(gcm = "ipsl", scenario = "ssp585", wbgt_model = "stull")

# Bind data together so all emissions scenarios can be plotted in 1 panel
ipsl_stull_heatstress_bound<-bind_rows(ipsl_ssp126_stull_wbgt$annual_heatstress_days, 
                                       ipsl_ssp370_stull_wbgt$annual_heatstress_days, 
                                       ipsl_ssp585_stull_wbgt$annual_heatstress_days)

ipsl_stull_kilndays_bound<-bind_rows(ipsl_ssp126_stull_wbgt$annual_heatstress_kiln_days, 
                                     ipsl_ssp370_stull_wbgt$annual_heatstress_kiln_days, 
                                     ipsl_ssp585_stull_wbgt$annual_heatstress_kiln_days)


ipsl_plots<-create_plots(gcm = "ipsl", 
                wbgt_model = "stull",
                map_scenario = "ssp585",
                heatstress_bound = ipsl_stull_heatstress_bound, 
                kilndays_bound = ipsl_stull_kilndays_bound)

#####################
### MPI-ESM1-2-HR ###
#####################

mpi_ssp126_stull_wbgt<-create_heatstress_df(gcm = "mpi", scenario = "ssp126", wbgt_model = "stull")
mpi_ssp370_stull_wbgt<-create_heatstress_df(gcm = "mpi", scenario = "ssp370", wbgt_model = "stull")
mpi_ssp585_stull_wbgt<-create_heatstress_df(gcm = "mpi", scenario = "ssp585", wbgt_model = "stull")

# Bind data together so all emissions scenarios can be plotted in 1 panel
mpi_stull_heatstress_bound<-bind_rows(mpi_ssp126_stull_wbgt$annual_heatstress_days, 
                                       mpi_ssp370_stull_wbgt$annual_heatstress_days, 
                                       mpi_ssp585_stull_wbgt$annual_heatstress_days)

mpi_stull_kilndays_bound<-bind_rows(mpi_ssp126_stull_wbgt$annual_heatstress_kiln_days, 
                                     mpi_ssp370_stull_wbgt$annual_heatstress_kiln_days, 
                                     mpi_ssp585_stull_wbgt$annual_heatstress_kiln_days)


mpi_plots<-create_plots(gcm = "mpi", 
                         wbgt_model = "stull",
                         map_scenario = "ssp585",
                         heatstress_bound = mpi_stull_heatstress_bound, 
                         kilndays_bound = mpi_stull_kilndays_bound)




##################
### MRI-ESM2-0 ###
##################

mri_ssp126_stull_wbgt<-create_heatstress_df(gcm = "mri", scenario = "ssp126", wbgt_model = "stull")
mri_ssp370_stull_wbgt<-create_heatstress_df(gcm = "mri", scenario = "ssp370", wbgt_model = "stull")
mri_ssp585_stull_wbgt<-create_heatstress_df(gcm = "mri", scenario = "ssp585", wbgt_model = "stull")

# Bind data together so all emissions scenarios can be plotted in 1 panel
mri_stull_heatstress_bound<-bind_rows(mri_ssp126_stull_wbgt$annual_heatstress_days, 
                                       mri_ssp370_stull_wbgt$annual_heatstress_days, 
                                       mri_ssp585_stull_wbgt$annual_heatstress_days)

mri_stull_kilndays_bound<-bind_rows(mri_ssp126_stull_wbgt$annual_heatstress_kiln_days, 
                                     mri_ssp370_stull_wbgt$annual_heatstress_kiln_days, 
                                     mri_ssp585_stull_wbgt$annual_heatstress_kiln_days)


mri_plots<-create_plots(gcm = "mri", 
                         wbgt_model = "stull",
                         map_scenario = "ssp585",
                         heatstress_bound = mri_stull_heatstress_bound, 
                         kilndays_bound = mri_stull_kilndays_bound)


#################
### UKESM1-LL ###
#################

ukes_ssp126_stull_wbgt<-create_heatstress_df(gcm = "ukes", scenario = "ssp126", wbgt_model = "stull")
ukes_ssp370_stull_wbgt<-create_heatstress_df(gcm = "ukes", scenario = "ssp370", wbgt_model = "stull")
ukes_ssp585_stull_wbgt<-create_heatstress_df(gcm = "ukes", scenario = "ssp585", wbgt_model = "stull")

# Bind data together so all emissions scenarios can be plotted in 1 panel
ukes_stull_heatstress_bound<-bind_rows(ukes_ssp126_stull_wbgt$annual_heatstress_days, 
                                       ukes_ssp370_stull_wbgt$annual_heatstress_days, 
                                       ukes_ssp585_stull_wbgt$annual_heatstress_days)

ukes_stull_kilndays_bound<-bind_rows(ukes_ssp126_stull_wbgt$annual_heatstress_kiln_days, 
                                     ukes_ssp370_stull_wbgt$annual_heatstress_kiln_days, 
                                     ukes_ssp585_stull_wbgt$annual_heatstress_kiln_days)


ukes_plots<-create_plots(gcm = "ukes", 
                         wbgt_model = "stull",
                         map_scenario = "ssp585",
                         heatstress_bound = ukes_stull_heatstress_bound, 
                         kilndays_bound = ukes_stull_kilndays_bound)


# Plot everything into a single large plot
gfdl_plots/ipsl_plots/mpi_plots/mri_plots/ukes_plots



