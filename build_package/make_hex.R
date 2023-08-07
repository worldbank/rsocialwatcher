# Make Hexagon Logo

if(T){
  
  # Setup ----------------------------------------------------------------------
  library(hexSticker)
  library(ggplot2)
  library(tidyverse)
  library(raster)
  library(rgeos)
  library(sp)
  library(sf)
  

  # Make ROI -------------------------------------------------------------------

  sticker(p, 
          package="blackmarbler", 
          spotlight = F,
          #l_alpha = 1, #0.15,
          p_size=23, #7 
          p_y = 1.40,
          p_family = "sans",
          p_fontface = "italic",
          s_x=1, 
          s_y=0.9, 
          s_width=2.5, 
          s_height=2.5,
          p_color = "white",
          h_fill = "black",
          h_color = "black",
          white_around_sticker = T,
          l_y = 1.4,
          l_x = 0.93,
          l_width = 3,
          l_height = 3,
          filename="~/Documents/Github/download_blackmarble/man/figures/hex.png")
  
  
}