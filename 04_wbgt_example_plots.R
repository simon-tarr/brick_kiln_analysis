library(raster)
library(tidyverse)
library(colorRamps)
library(patchwork)
library(janitor)
library(ggpmisc)

#####################
### Example Plots ###
#####################

# Create a main results dataframe that you're interested
# Here, want to plot some graphs for SSP126 - Stull
gfdl_annual<-create_master_results_df(input=c("gfdl_ssp126_stull", 
                                              "ipsl_ssp126_stull", 
                                              "mpi_ssp126_stull",
                                              "mri_ssp126_stull",
                                              "ukes_ssp126_stull"), 
                                      method = "year", temperature_threshold = 30)


# Create summary object where we sum heatstress days by year
ex<-gfdl_annual %>% 
  mutate(src = paste(gcm, scenario, model, sep="_")) %>% 
  group_by(year = lubridate::floor_date(date, "year"), src) %>% 
  summarise(days_over = sum(days_over), n = n())

my.formula <- y ~ x

# Plot annual heat stress days data
ggplot(ex, aes(x=year, y=days_over, color=src)) + 
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
