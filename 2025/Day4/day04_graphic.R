#!/usr/bin/env Rscript

# Advent of Code 2025 Day 3 graphic

library(showtext)
font_add_google("Montserrat", "montserrat")
font_add_google("Atkinson Hyperlegible", "atkinson")
font_add_google("JetBrains Mono", "jetbrains")
showtext_auto()

create_day3_graphic <- function(
    output_path = "2025/Day3/day03_graphic.png",
    aspect_ratio = "1:1"
) {
  
  # Electrical/lobby palette
  colors <- list(
    bg_dark      = "#0d1117",
    panel        = "#161b22",
    volt_yellow  = "#f0c020",
    power_green  = "#3fb950",
    warning_red  = "#f85149",
    wire_copper  = "#da8a67",
    metal        = "#8b949e",
    text_main    = "#e6edf3",
    text_dim     = "#7d8590",
    battery_body = "#21262d"
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
  
  # Helper function to draw a battery bank
  draw_battery_bank <- function(cx, cy, digits, selected_indices, 
                                accent_color, label, joltage) {
    
    n_batteries <- nchar(digits)
    digit_vec <- as.numeric(strsplit(digits, "")[[1]])
    
    batt_w <- 0.42
    batt_h <- 0.75
    gap <- 0.06
    total_w <- n_batteries * batt_w + (n_batteries - 1) * gap
    start_x <- cx - total_w / 2
    
    # Panel background
    rect(cx - total_w/2 - 0.3, cy - batt_h/2 - 0.55,
         cx + total_w/2 + 0.3, cy + batt_h/2 + 0.45,
         col = colors$panel, border = colors$metal, lwd = 2)
    
    # Bank label
    text(cx - total_w/2 - 0.15, cy + batt_h/2 + 0.25, label,
         col = accent_color, cex = 0.6, adj = 0, family = "montserrat", font = 2)
    
    # Draw each battery
    for (i in 1:n_batteries) {
      bx <- start_x + (i - 1) * (batt_w + gap) + batt_w/2
      is_selected <- i %in% selected_indices
      
      # Battery body
      body_col <- if (is_selected) accent_color else colors$battery_body
      rect(bx - batt_w/2, cy - batt_h/2,
           bx + batt_w/2, cy + batt_h/2,
           col = body_col, border = colors$metal, lwd = 1.5)
      
      # Battery terminal (top nub)
      rect(bx - batt_w/4, cy + batt_h/2,
           bx + batt_w/4, cy + batt_h/2 + 0.1,
           col = colors$metal, border = NA)
      
      # Digit label
      text_col <- if (is_selected) colors$bg_dark else colors$text_dim
      text(bx, cy, digit_vec[i],
           col = text_col, cex = 0.8, family = "jetbrains", font = 2)
    }
    
    # Joltage output display
    rect(cx - 1.1, cy - batt_h/2 - 0.45,
         cx + 1.1, cy - batt_h/2 - 0.15,
         col = colors$bg_dark, border = accent_color, lwd = 1.5)
    
    text(cx, cy - batt_h/2 - 0.30, paste0(joltage, " jolts"),
         col = accent_color, cex = 0.6, family = "jetbrains", font = 2)
  }
  
  # Warning light (offline indicator)
  points(9.3, 9.0, pch = 19, col = colors$warning_red, cex = 3)
  points(9.3, 9.0, pch = 1, col = colors$metal, cex = 3, lwd = 2)
  text(9.3, 8.5, "OFFLINE", col = colors$warning_red, cex = 0.5, 
       family = "montserrat", font = 2)
  
  # HEADER
  text(0.5, 9.1, "advent of code 2025", 
       col = colors$volt_yellow, cex = 1.8, adj = 0, family = "montserrat", font = 2)
  text(0.5, 8.5, "day 3: lobby  (power the escalator with batteries)",
       col = colors$metal, cex = 1.1, adj = 0, family = "atkinson")
  
  # SETUP
  text(0.5, 7.8, "setup", 
       col = colors$text_dim, cex = 0.85, adj = 0, family = "montserrat", font = 2)
  
  text(0.5, 7.4,
       "input: lines of digits (1-9), each line is a battery bank",
       col = colors$text_main, cex = 0.95, adj = 0, family = "atkinson")
  
  text(0.5, 7.0,
       "joltage = number formed by selected battery digits (in order)",
       col = colors$text_main, cex = 0.95, adj = 0, family = "atkinson")
  
  # PART 1 text
  segments(0.5, 6.4, 2.0, 6.4, col = colors$volt_yellow, lwd = 3)
  
  text(0.5, 6.1, "part 1: select exactly 2 batteries",
       col = colors$volt_yellow, cex = 1.0, adj = 0, family = "montserrat", font = 2)
  
  text(0.5, 5.7, "greedy: pick max digit, then max remaining",
       col = colors$wire_copper, cex = 0.9, adj = 0, family = "jetbrains")
  
  # PART 1 BATTERY BANK - pick 2 (positions 7 and 12 for 9 and 2)
  draw_battery_bank(cx = 5.0, cy = 4.7,
                    digits = "818181911112111",
                    selected_indices = c(7, 12),
                    accent_color = colors$volt_yellow,
                    label = "PART 1",
                    joltage = "92")
  
  # PART 2 text
  segments(0.5, 3.5, 2.0, 3.5, col = colors$power_green, lwd = 3)
  
  text(0.5, 3.2, "part 2: select exactly 12 batteries",
       col = colors$power_green, cex = 1.0, adj = 0, family = "montserrat", font = 2)
  
  text(0.5, 2.8, "for pos 1..k: pick max in valid range",
       col = colors$wire_copper, cex = 0.9, adj = 0, family = "jetbrains")
  
  # PART 2 BATTERY BANK - pick 12
  draw_battery_bank(cx = 5.0, cy = 1.8,
                    digits = "818181911112111",
                    selected_indices = c(1, 3, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15),
                    accent_color = colors$power_green,
                    label = "PART 2",
                    joltage = "888911112111")
  
  # FOOTER
  segments(0.5, 0.5, 9.5, 0.5, col = colors$panel, lwd = 1)
  
  text(0.5, 0.25, "GRID", 
       col = colors$text_dim, cex = 0.7, adj = 0, family = "montserrat", font = 2)
  
  text(9.5, 0.25, "adventofcode.com/2025/day/3",
       col = colors$text_dim, cex = 0.65, adj = 1, family = "atkinson")
  
  dev.off()
  message("Graphic saved to: ", output_path)
}


create_day3_graphic("2025/Day3/day03_graphic.png", aspect_ratio = "1:1")