# code/01_data_source.R
# Locate and download all of the data used in the project


# Setup -------------------------------------------------------------------

# Load libraries
# remotes::install_github("EMODnet/EMODnetWFS")
library(tidyverse)
library(sf)
library(EMODnetWFS)
library(sfheaders)

# View all services
View(emodnet_wfs())

# View all available layers
# View(emodnet_get_all_wfs_info()) # NB: This takes a long time

# Citing the EMODnetWFS package
citation(package = "EMODnetWFS")


# Coastal data types ------------------------------------------------------

# View data layers from 'geology_coastal_behavior'
geo_coast_layers <- emodnet_get_wfs_info(service = "geology_coastal_behavior")

# Download coastal types point data
if(file.exists("backyard/data/coastal_type.RData")){
  load("backyard/data/coastal_type.RData")
} else {
  coastal_type <- emodnet_get_layers(
    service = "geology_coastal_behavior",
    layers = c("coastal_type_20210501_2_3m_90m"), 
    reduce_layers = TRUE, crs = 4326)
}

# Convert to dataframe
coastal_type_df <- sf_to_df(coastal_type) |> 
  left_join(coastal_type, by = c("sfg_id" = "id", "multipoint_id" = "fid")) |> 
  dplyr::select(x, y, morpho, coasttype) |> 
  dplyr::rename(lon = x, lat = y)

# Save coastal type data
save(coastal_type, file = "backyard/data/coastal_type.RData")
save(coastal_type_df, file = "backyard/data/coastal_type_df.RData")

