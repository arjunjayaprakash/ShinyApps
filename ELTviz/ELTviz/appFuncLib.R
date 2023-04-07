library(tidyverse)
library(raster)
library(sf)
library(viridis)
library(cowplot)

# Function to Read EQ Footprint
read_EQfootprint <- function(filename, filepath, trimmed = FALSE, threshold = 20, old = FALSE){
  if(missing(filepath)){
    filepath = getwd
  }
  
  output_footprint <- brick(paste0(filepath,"/",filename,".pgw"))
  footprint_info <- read_tsv(paste0(filepath,"/",filename,".inf"))
  
  if(old){
    bbox <- extent(as.numeric(footprint_info$Value[footprint_info$Attribute == "LEFT"]),
                   as.numeric(footprint_info$Value[footprint_info$Attribute == "RIGHT"]),
                   as.numeric(footprint_info$Value[footprint_info$Attribute == "BOTTOM"]),
                   as.numeric(footprint_info$Value[footprint_info$Attribute == "TOP"]))
  } else{ 
    bbox <- extent(as.numeric(footprint_info$Value[footprint_info$Attribute == "LonX_Left"]),
                   as.numeric(footprint_info$Value[footprint_info$Attribute == "LonX_Right"]),
                   as.numeric(footprint_info$Value[footprint_info$Attribute == "LatY_Bottom"]),
                   as.numeric(footprint_info$Value[footprint_info$Attribute == "LatY_Top"]))
  }
  extent(output_footprint) <- bbox
  
  if(trimmed) {
    output_footprint <- calc(output_footprint, fun=function(x){ x[x < threshold] <- NA; return(x)} )
  }
  return(output_footprint)
}

# Function to Plot EQ footprint

plot_EQfootprint <- function(footprint_raster, band = 1, lon_limit, lat_limit) {
  footprint <- raster(footprint_raster, band)
  fp_as_df <- raster::as.data.frame(footprint, xy = TRUE)
  names(fp_as_df) <- c("x", "y", "value")
  
  legend_colors <- c("#006100", "#498a00", "#8bb500", "#d6e600", "#ffe500", "#ffa600",
                     "#ff6f00", "#ff2200", "#ff0000", "#ffc8ff", "#ff64ff", "#ff00ff",
                     "#803280", "#800080", "#0000ff", "#000080", "#000064", "#000032",
                     "#000000")
  fp_as_df <- fp_as_df %>%
    mutate(gm_bins = cut(value, breaks = seq(from = 20, to = 200, by = 10), right = FALSE)) %>%
    filter(!is.na(gm_bins))
  
  
  if(missing(lon_limit) | missing(lat_limit)) {
    if(missing(lat_limit)) {
      ggplot() +
        geom_tile(data = fp_as_df, aes(x = x, y = y, fill = value)) +
        scale_fill_gradientn(name = "0.3 SA (%g)",colors = legend_colors) +
        coord_quickmap()  +
        theme_bw() +
        xlab("Longitude") +
        ylab("Latitude")
    } else {
      ggplot() +
        geom_tile(data = fp_as_df, aes(x = x, y = y, fill = value)) +
        scale_fill_gradientn(name = "0.3 SA (%g)",colors = legend_colors) +
        coord_quickmap(xlim = lon_limit)  +
        theme_bw() +
        xlab("Longitude") +
        ylab("Latitude")
    }
  } else {
    ggplot() +
      geom_tile(data = fp_as_df, aes(x = x, y = y, fill = value)) +
      scale_fill_gradientn(name = "0.3 SA (%g)",colors = legend_colors) +
      coord_quickmap(xlim = lon_limit, ylim = lat_limit)  +
      theme_bw() +
      xlab("Longitude") +
      ylab("Latitude")
  }
  
}