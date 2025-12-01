#!/usr/bin/env Rscript

# Advent of Code 2025 Day 1 graphic (a math summary)

library(showtext)
font_add_google("Montserrat", "montserrat")
font_add_google("Atkinson Hyperlegible", "atkinson")
showtext_auto()

create_day1_graphic <- function(
    output_path = "2025/Day1/day01_graphic.png",
    aspect_ratio = "1:1"
) {
  
  # Colorblind-safe Secret Safe palette
  colors <- list(
    bg_dark      = "#0d1b2a",
    frost        = "#415a77",
    ice_blue     = "#56B4E9",
    gold         = "#E69F00",
    silver       = "#b8c5d6",
    text_main    = "#e8eef4",
    text_dim     = "#8899aa"
  )
  
  # Dimensions
  dims <- switch(aspect_ratio,
                 "1:1"  = c(width = 1080, height = 1080),
                 "16:9" = c(width = 1920, height = 1080),
                 "4:5"  = c(width = 1080, height = 1350),
                 c(width = 1080, height = 1080)
  )
  
  png(output_path, width = dims["width"], height = dims["height"], 
      bg = colors$bg_dark, res = 150)
  
  par(mar = c(0, 0, 0, 0), bg = colors$bg_dark)
  plot.new()
  plot.window(xlim = c(0, 10), ylim = c(0, 10))
  
# COMBINATION LOCK DIAL (top right) - full 0-99 dial

  dial_cx <- 8.0
  dial_cy <- 7.8
  dial_r <- 1.8
  
  # Outer ring
  theta_full <- seq(0, 2 * pi, length.out = 100)
  lines(dial_cx + dial_r * cos(theta_full), 
        dial_cy + dial_r * sin(theta_full), 
        col = colors$frost, lwd = 3)
  
  # Inner ring
  lines(dial_cx + (dial_r - 0.12) * cos(theta_full), 
        dial_cy + (dial_r - 0.12) * sin(theta_full), 
        col = colors$frost, lwd = 1.5)
  
  # Tick marks and numbers (0-99, every 10 labeled)
  for (i in 0:99) {
    # Angle: 0 at top, going clockwise
    angle <- pi/2 - (i / 100) * 2 * pi
    
    if (i %% 10 == 0) {
      # Major tick + number
      lines(c(dial_cx + (dial_r - 0.25) * cos(angle), 
              dial_cx + (dial_r - 0.08) * cos(angle)),
            c(dial_cy + (dial_r - 0.25) * sin(angle), 
              dial_cy + (dial_r - 0.08) * sin(angle)),
            col = colors$silver, lwd = 2)
      
      # Number label
      text(dial_cx + (dial_r - 0.45) * cos(angle),
           dial_cy + (dial_r - 0.45) * sin(angle),
           labels = i,
           col = colors$silver, cex = 0.45, family = "atkinson")
    } else if (i %% 5 == 0) {
      # Medium tick (every 5)
      lines(c(dial_cx + (dial_r - 0.18) * cos(angle), 
              dial_cx + (dial_r - 0.08) * cos(angle)),
            c(dial_cy + (dial_r - 0.18) * sin(angle), 
              dial_cy + (dial_r - 0.08) * sin(angle)),
            col = colors$frost, lwd = 1.5)
    } else {
      # Minor tick
      lines(c(dial_cx + (dial_r - 0.14) * cos(angle), 
              dial_cx + (dial_r - 0.08) * cos(angle)),
            c(dial_cy + (dial_r - 0.14) * sin(angle), 
              dial_cy + (dial_r - 0.08) * sin(angle)),
            col = colors$frost, lwd = 0.8)
    }
  }
  
  # Dial pointer (pointing at 0, which is at top)
  pointer_angle <- pi / 2  # 0 is at top
  lines(c(dial_cx, dial_cx + 0.9 * cos(pointer_angle)),
        c(dial_cy, dial_cy + 0.9 * sin(pointer_angle)),
        col = colors$gold, lwd = 3)
  
  # Center hub
  points(dial_cx, dial_cy, pch = 19, col = colors$gold, cex = 1.2)
  points(dial_cx, dial_cy, pch = 19, col = colors$bg_dark, cex = 0.5)
  
  # Arrow indicator at top (fixed position showing where dial points)
  arrows(dial_cx, dial_cy + dial_r + 0.35, 
         dial_cx, dial_cy + dial_r + 0.12,
         col = colors$gold, lwd = 2, length = 0.08)
  
# HEADER

  text(0.8, 9.1, "advent of code 2025", 
       col = colors$gold, cex = 1.8, adj = 0, family = "montserrat", font = 2)
  text(0.8, 8.5, "day 1: secret entrance \n(what is the password?)",
       col = colors$silver, cex = 1.1, adj = 0, family = "atkinson")
  
# SETUP

  text(0.8, 7.7, "setup", 
       col = colors$text_dim, cex = 0.85, adj = 0, family = "montserrat", font = 2)
  
  text(0.8, 7.2,
       expression(italic(D) == "{0, 1, 2, ..., 99}"),
       col = colors$text_main, cex = 1.0, adj = 0, family = "atkinson")
  
  text(0.8, 6.7,
       expression(p[0] == 50 ~~~ "(dial starts at 50)"),
       col = colors$text_main, cex = 1.0, adj = 0, family = "atkinson")
  
  text(0.8, 6.2,
       expression(r[i] == +n ~ "if R," ~~ -n ~ "if L"),
       col = colors$text_main, cex = 1.0, adj = 0, family = "atkinson")
  
# PART 1

  segments(0.8, 5.55, 2.6, 5.55, col = colors$ice_blue, lwd = 3)
  
  text(0.8, 5.2, "part 1: no. of times pointing at zero",
       col = colors$ice_blue, cex = 1.0, adj = 0, family = "montserrat", font = 2)
  
  text(0.8, 4.7,
       expression(p[i] == (p[i-1] + r[i]) ~ "mod 100"),
       col = colors$text_main, cex = 1.0, adj = 0, family = "atkinson")
  
  text(0.8, 4.2, "count final positions where", 
       col = colors$text_dim, cex = 0.9, adj = 0, family = "atkinson")
  
  text(0.8, 3.75,
       expression(p[i] == 0),
       col = colors$text_main, cex = 1.0, adj = 0, family = "atkinson")
  
# PART 2

  segments(0.8, 3.1, 2.6, 3.1, col = colors$gold, lwd = 3)
  
  text(0.8, 2.75, "part 2: password method 0x434C49434B; crossings of zero",
       col = colors$gold, cex = 1.0, adj = 0, family = "montserrat", font = 2)
  
  text(0.8, 2.25, "track unwrapped position",
       col = colors$text_dim, cex = 0.9, adj = 0, family = "atkinson")
  
  text(0.8, 1.8,
       expression(tilde(p)[i] == p[i-1] + r[i]),
       col = colors$text_main, cex = 1.0, adj = 0, family = "atkinson")
  
  text(0.8, 1.3, "count zero-crossings per rotation",
       col = colors$text_dim, cex = 0.9, adj = 0, family = "atkinson")
  
# FOOTER

  segments(0.8, 0.6, 9.2, 0.6, col = colors$frost, lwd = 1)
  
  text(0.8, 0.3, "GRID", 
       col = colors$text_dim, cex = 0.7, adj = 0, family = "montserrat", font = 2)
  
  text(9.2, 0.3, "adventofcode.com/2025/day/1",
       col = colors$text_dim, cex = 0.65, adj = 1, family = "atkinson")
  
  dev.off()
  message("Graphic saved to: ", output_path)
}

# Generate
create_day1_graphic("2025/Day1/day01_graphic.png", aspect_ratio = "1:1")