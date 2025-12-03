#!/usr/bin/env Rscript

# Advent of Code 2025 Day 2 graphic

library(showtext)
font_add_google("Montserrat", "montserrat")
font_add_google("Atkinson Hyperlegible", "atkinson")
font_add_google("JetBrains Mono", "jetbrains")
showtext_auto()

create_day2_graphic <- function(
    output_path = "2025/Day2/day02_graphic.png",
    aspect_ratio = "1:1"
) {
  
  # Gift shop palette
  colors <- list(
    bg_dark      = "#1a1a2e",
    shelf        = "#16213e",
    ice_blue     = "#56B4E9",
    gold         = "#E69F00",
    holly_green  = "#009E73",
    ribbon_red   = "#D55E00",
    silver       = "#b8c5d6",
    text_main    = "#e8eef4",
    text_dim     = "#8899aa",
    barcode      = "#0f0f1a"
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
  
  # Helper function to draw a product tag
  draw_tag <- function(cx, cy, tag_w, tag_h, product_id, pattern_text, 
                       accent_color, seed_val) {
    
    # Tag background
    rect(cx - tag_w/2, cy - tag_h/2, 
         cx + tag_w/2, cy + tag_h/2,
         col = colors$text_main, border = colors$silver, lwd = 2)
    
    # Hole for string at top
    points(cx, cy + tag_h/2 - 0.12, pch = 19, col = colors$bg_dark, cex = 1.2)
    points(cx, cy + tag_h/2 - 0.12, pch = 1, col = colors$silver, cex = 1.2, lwd = 2)
    
    # String loop
    theta <- seq(pi/2, 5*pi/2, length.out = 50)
    string_r <- 0.18
    lines(cx + string_r * cos(theta) * 0.6, 
          cy + tag_h/2 - 0.12 + string_r * sin(theta) + 0.18,
          col = colors$ribbon_red, lwd = 2)
    
    # Barcode area
    barcode_y <- cy - 0.05
    barcode_h <- 0.45
    barcode_w <- 1.5
    
    # Generate barcode pattern
    set.seed(seed_val)
    n_bars <- 28
    bar_widths <- sample(c(0.02, 0.035, 0.05), n_bars, replace = TRUE, prob = c(0.5, 0.35, 0.15))
    gap_widths <- sample(c(0.012, 0.025), n_bars, replace = TRUE)
    
    x_pos <- cx - barcode_w/2
    for (i in 1:n_bars) {
      rect(x_pos, barcode_y - barcode_h/2,
           x_pos + bar_widths[i], barcode_y + barcode_h/2,
           col = colors$barcode, border = NA)
      x_pos <- x_pos + bar_widths[i] + gap_widths[i]
      if (x_pos > cx + barcode_w/2 - 0.03) break
    }
    
    # Product ID label
    text(cx, barcode_y + barcode_h/2 + 0.18, "PRODUCT ID",
         col = colors$text_dim, cex = 0.35, family = "montserrat", font = 2)
    
    # Product ID number
    text(cx, barcode_y - barcode_h/2 - 0.15, product_id,
         col = colors$barcode, cex = 0.6, family = "jetbrains", font = 2)
    
    # Invalid stamp (diagonal)
    text(cx, cy + 0.4, "INVALID",
         col = accent_color, cex = 0.55, family = "montserrat", font = 2, srt = -12)
    
    # Pattern breakdown at bottom
    text(cx, cy - tag_h/2 + 0.15, pattern_text,
         col = colors$holly_green, cex = 0.4, family = "jetbrains")
  }
  
  # PART 1 TAG (right side) - exactly twice
  draw_tag(cx = 8.0, cy = 4.5, 
           tag_w = 2.2, tag_h = 1.7,
           product_id = "6464", 
           pattern_text = "64 | 64",
           accent_color = colors$ice_blue,
           seed_val = 6464)
  
  # PART 2 TAG (right side) - at least twice
  draw_tag(cx = 8.0, cy = 1.8,
           tag_w = 2.2, tag_h = 1.7,
           product_id = "123123123",
           pattern_text = "123 | 123 | 123",
           accent_color = colors$gold,
           seed_val = 123)
  
  # HEADER
  
  text(0.8, 9.1, "advent of code 2025", 
       col = colors$gold, cex = 1.8, adj = 0, family = "montserrat", font = 2)
  text(0.8, 8.5, "day 2: gift shop\n(find the invalid product IDs)",
       col = colors$silver, cex = 1.1, adj = 0, family = "atkinson")
  
  # SETUP
  
  text(0.8, 7.7, "setup", 
       col = colors$text_dim, cex = 0.85, adj = 0, family = "montserrat", font = 2)
  
  text(0.8, 7.25,
       "input: one string of comma-delimited ranges of product IDs (start-end)",
       col = colors$text_main, cex = 0.95, adj = 0, family = "atkinson")
  
  text(0.8, 6.8,
       "11-22,95-115,998-1012,...",
       col = colors$text_dim, cex = 0.85, adj = 0, family = "jetbrains")
  
  text(0.8, 6.35,
       "no leading zeros (0101 is not valid input)",
       col = colors$text_main, cex = 0.95, adj = 0, family = "atkinson")
  
  # PART 1
  
  segments(0.8, 5.7, 2.6, 5.7, col = colors$ice_blue, lwd = 3)
  
  text(0.8, 5.35, "part 1: pattern repeated exactly twice",
       col = colors$ice_blue, cex = 1.0, adj = 0, family = "montserrat", font = 2)
  
  text(0.8, 4.85, "invalid if ID = (pattern)(pattern)",
       col = colors$text_main, cex = 0.95, adj = 0, family = "atkinson")
  
  # Regex explanation
  text(0.8, 4.35, "regex:  ^(.+)\\1$",
       col = colors$holly_green, cex = 1.0, adj = 0, family = "jetbrains")
  
  text(0.8, 3.9, "examples: 55, 6464, 123123",
       col = colors$text_dim, cex = 0.85, adj = 0, family = "atkinson")
  
  # Answer format
  text(0.8, 3.45, "answer: sum of all invalid IDs",
       col = colors$text_main, cex = 0.9, adj = 0, family = "atkinson")
  
  # PART 2
  
  segments(0.8, 2.9, 2.6, 2.9, col = colors$gold, lwd = 3)
  
  text(0.8, 2.55, "part 2: pattern repeated 2+ times",
       col = colors$gold, cex = 1.0, adj = 0, family = "montserrat", font = 2)
  
  text(0.8, 2.05, "invalid if ID = (pattern)(pattern)+",
       col = colors$text_main, cex = 0.95, adj = 0, family = "atkinson")
  
  # Regex explanation
  text(0.8, 1.55, "regex:  ^(.+)\\1+$",
       col = colors$holly_green, cex = 1.0, adj = 0, family = "jetbrains")
  
  text(0.8, 1.1, "examples: 12341234, 123123123, 1111111",
       col = colors$text_dim, cex = 0.85, adj = 0, family = "atkinson")
  
  # FOOTER
  
  segments(0.8, 0.6, 9.2, 0.6, col = colors$shelf, lwd = 1)
  
  text(0.8, 0.3, "GRID", 
       col = colors$text_dim, cex = 0.7, adj = 0, family = "montserrat", font = 2)
  
  text(9.2, 0.3, "adventofcode.com/2025/day/2",
       col = colors$text_dim, cex = 0.65, adj = 1, family = "atkinson")
  
  dev.off()
  message("Graphic saved to: ", output_path)
}


create_day2_graphic("2025/Day2/day02_graphic.png", aspect_ratio = "1:1")