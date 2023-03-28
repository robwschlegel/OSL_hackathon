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
    layers = c("coastal_type_20210501_0025_80k_150k"), 
    reduce_layers = TRUE, crs = 4326)
}

# Cast to a simpler object type
coastal_type_multipoint <- st_cast(coastal_type, "MULTIPOINT")

# Cast to a simpler object type
coastal_type_point <- st_cast(coastal_type, "POINT")

# Convert to dataframe
coastal_type_df <- sf_to_df(coastal_type)

# Save coastal type data
save(coastal_type, file = "backyard/data/coastal_type.RData")
save(coastal_type_point, file = "backyard/data/coastal_type_point.RData")

