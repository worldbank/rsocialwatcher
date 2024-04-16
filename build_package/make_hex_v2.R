# Make Hexagon Logo



if(T){
  
  # Setup ----------------------------------------------------------------------
  library(hexSticker)
  library(ggplot2)
  library(tidyverse)
  
  # Make ROI -------------------------------------------------------------------
  set.seed(42)
  
  # https://stackoverflow.com/questions/54111718/generate-random-data-in-the-form-of-a-letter
  library(png)
  
  getTextImage <- function(text) {
    filename <- tempfile()
    png(filename = filename)
    plot.new()
    cex <- 1
    repeat {
      if (strwidth(text, cex = 2*cex) > 1) break
      if (strheight(text, cex = 2*cex) > 1) break 
      cex <- 2*cex
    }
    text(0.5, 0.5, text, cex = cex)
    dev.off()
    image <- readPNG(filename)
    unlink(filename)    # clean up file
    if (length(dim(image)) == 3)
      image <- image[,,1] # just keep one channel
    image
  }
  
  randomText <- function(n, text) {
    image <- getTextImage(text)
    nx <- dim(image)[1]
    ny <- dim(image)[2]
    hits <- 0
    x <- y <- numeric(n)
    while (hits < n) {
      tryx <- runif(1)
      tryy <- runif(1)
      keep <- image[round((nx-1)*tryx + 1), round((ny-1)*tryy + 1)] == 0
      if (keep) {
        hits <- hits + 1
        # Need to rotate so it looks good
        x[hits] <- tryy
        y[hits] <- 1 - tryx
      }
    }
    cbind(x, y)
  }
  
  points_df <- randomText(1000, "f") %>%
    as.data.frame()
  
  SCALE <- 0.5
  x_min <- min(points_df$x) * (1-SCALE)
  x_max <- max(points_df$x) * (1+SCALE)
  
  y_min <- min(points_df$y) * (1-SCALE)
  y_max <- max(points_df$y) * (1+SCALE)
  
  points_df$y <- points_df$y + 0.01
  points_df$x <- points_df$x + 0.05

  points_v2_df <- points_df %>%
    mutate(x = x + runif(n(), -0.17, 0.17),
           y = y + runif(n(), -0.17, 0.17))
  
  points_v3_df <- points_df %>%
    mutate(x = x + runif(n(), -0.3, 0.3),
           y = y + runif(n(), -0.3, 0.3))
  
  p <- ggplot() + 
    geom_point(data = points_df,
               aes(x = x,
                   y = y),
               color = "white",
               alpha = 0.8,
               size = 0.05) +
    geom_point(data = points_v2_df,
               aes(x = x,
                   y = y),
               color = "white",
               size = 0.05,
               alpha = 0.3) +
    geom_point(data = points_v3_df,
               aes(x = x,
                   y = y),
               color = "white",
               size = 0.05,
               alpha = 0.1) +
    scale_x_continuous(limits = c(x_min, x_max)) +
    scale_y_continuous(limits = c(y_min, y_max)) +
    theme_void() +
    theme(panel.background = element_rect(fill = rgb(44, 100, 246, maxColorValue = 255) )) 

  sticker(p, 
          package="rsocialwatcher", 
          spotlight = F,
          #l_alpha = 0.3, #0.15,
          p_size=19, #7 
          p_y = 1.45, # 1.40,
          p_family = "sans",
          p_fontface = "italic",
          s_x=1, 
          s_y=1, 
          s_width=2.5, 
          s_height=2.5,
          p_color = "white",
          h_fill = "black",
          h_color = "black",
          white_around_sticker = T,
          l_y = 1.4,
          l_x = 0.93,
          l_width = 3,
          l_height = 2,
          filename="~/Documents/Github/rsocialwatcher/man/figures/logo.png")
  
}
