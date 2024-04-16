# 
# if(T){
#   
#   # Setup ----------------------------------------------------------------------
#   library(hexSticker)
#   library(ggplot2)
#   library(tidyverse)
#   
#   # Make ROI -------------------------------------------------------------------
#   set.seed(42)
#   
#   p <- data.frame(n = rnorm(10000)) %>%
#     filter(n > -1.3,
#            n < 1.3) %>%
#     ggplot() +
#     geom_histogram(aes(x = n),
#                    fill = "white",
#                    alpha = 0.7,
#                    color = rgb(44, 100, 246, maxColorValue = 255),
#                    size = 0.25,
#                    bins = 30) +
#     scale_y_continuous(limits = c(0, 600)) +
#     theme_void() +
#     theme(panel.background = element_rect(fill = rgb(44, 100, 246, maxColorValue = 255) ))
#   
#   sticker(p, 
#           package="rsocialwatcher", 
#           spotlight = F,
#           #l_alpha = 0.3, #0.15,
#           p_size=19, #7 
#           p_y = 1.4, # 1.40,
#           p_family = "sans",
#           p_fontface = "italic",
#           s_x=1, 
#           s_y=0.9, 
#           s_width=2.5, 
#           s_height=2.5,
#           p_color = "white",
#           h_fill = "black",
#           h_color = "black",
#           white_around_sticker = T,
#           l_y = 1.4,
#           l_x = 0.93,
#           l_width = 3,
#           l_height = 3,
#           filename="~/Documents/Github/rsocialwatcher/man/figures/logo.png")
#   
# }
