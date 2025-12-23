#!/usr/bin/env Rscript

# Advent of Code 2025 Day 5 graphic
# Base R version - refined layout

create_day5_graphic <- function(
    output_path = "day05_graphic.png",
    aspect_ratio = "1:1"
) {
  
  # Cafeteria / kitchen inventory palette
  colors <- list(
    bg_dark      = "#0d1117",
    panel        = "#161b22",
    fresh_green  = "#3fb950",
    spoiled_red  = "#f85149",
    range_blue   = "#58a6ff",
    range_purple = "#a371f7",
    range_teal   = "#3ddbd9",
    range_orange = "#f0883e",
    overlap_gold = "#d29922",
    metal        = "#8b949e",
    text_main    = "#e6edf3",
    text_dim     = "#7d8590",
    grid_line    = "#30363d",
    ingredient   = "#f0e6d3"
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
  
  # ============ DECORATIVE ICONS ============
  
  # Chef hat icon (larger)
  draw_chef_hat <- function(x, y, size = 1.0) {
    s <- size * 0.5
    
    # Hat puffs (top)
    symbols(x - s*0.35, y + s*0.5, circles = s*0.25, 
            inches = FALSE, add = TRUE, bg = colors$ingredient, fg = colors$metal, lwd = 1.5)
    symbols(x, y + s*0.65, circles = s*0.3, 
            inches = FALSE, add = TRUE, bg = colors$ingredient, fg = colors$metal, lwd = 1.5)
    symbols(x + s*0.35, y + s*0.5, circles = s*0.25, 
            inches = FALSE, add = TRUE, bg = colors$ingredient, fg = colors$metal, lwd = 1.5)
    
    # Hat band
    rect(x - s*0.5, y - s*0.1, x + s*0.5, y + s*0.25,
         col = colors$ingredient, border = colors$metal, lwd = 2)
    
    # Band stripe
    rect(x - s*0.5, y - s*0.03, x + s*0.5, y + s*0.07,
         col = colors$fresh_green, border = NA)
  }
  
  # Ingredient jar icon (with label)
  draw_jar <- function(x, y, size = 1.0, fill_color, label = "") {
    s <- size * 0.35
    
    # Jar body
    rect(x - s*0.4, y - s*0.7, x + s*0.4, y + s*0.3,
         col = adjustcolor(fill_color, alpha = 0.25), 
         border = colors$metal, lwd = 2)
    
    # Jar lid
    rect(x - s*0.35, y + s*0.3, x + s*0.35, y + s*0.5,
         col = colors$metal, border = colors$text_dim, lwd = 1.5)
    
    # Fill level
    rect(x - s*0.35, y - s*0.65, x + s*0.35, y + s*0.15,
         col = adjustcolor(fill_color, alpha = 0.5), border = NA)
    
    # Label on jar
    if (label != "") {
      text(x, y - s*0.2, label, col = colors$bg_dark, cex = 0.3, font = 2)
    }
  }
  
  # Place decorative elements
  draw_chef_hat(8.0, 9.0, size = 1.2)
  draw_jar(8.9, 9.1, size = 1.0, fill_color = colors$fresh_green, label = "")
  draw_jar(9.5, 8.85, size = 0.85, fill_color = colors$spoiled_red, label = "")
  
  # ============ HEADER ============
  text(0.5, 9.2, "advent of code 2025",
       col = colors$fresh_green, cex = 1.9, adj = 0, font = 2)
  text(0.5, 8.6, "day 5: cafeteria",
       col = colors$text_main, cex = 1.15, adj = 0, font = 2)
  text(0.5, 8.2, "identify fresh ingredients by ID ranges",
       col = colors$text_dim, cex = 0.85, adj = 0)
  
  # ============ SETUP ============
  text(0.5, 7.65, "setup",
       col = colors$metal, cex = 0.8, adj = 0, font = 2)
  
  text(0.5, 7.3,
       "input: fresh ingredient ID ranges (inclusive), then available IDs",
       col = colors$text_main, cex = 0.85, adj = 0)
  
  text(0.5, 6.95,
       "ranges can overlap; an ID is fresh if it falls within any range",
       col = colors$text_main, cex = 0.85, adj = 0)
  
  # ============ PART 1 ============
  segments(0.5, 6.5, 2.0, 6.5, col = colors$fresh_green, lwd = 3)
  text(0.5, 6.2, "part 1: check available IDs",
       col = colors$fresh_green, cex = 1.0, adj = 0, font = 2)
  text(0.5, 5.88, "count IDs that fall within any fresh range",
       col = colors$ingredient, cex = 0.65, adj = 0)
  
  # Part 1 Panel
  panel1_cx <- 5.0
  panel1_cy <- 4.55
  panel1_w <- 4.5
  panel1_h <- 2.1
  
  rect(panel1_cx - panel1_w/2, panel1_cy - panel1_h/2,
       panel1_cx + panel1_w/2, panel1_cy + panel1_h/2,
       col = colors$panel, border = colors$metal, lwd = 2)
  
  # Example data
  example_ranges <- matrix(c(3, 5, 10, 14, 16, 20, 12, 18), ncol = 2, byrow = TRUE)
  available_ids <- c(1, 5, 8, 11, 17, 32)
  range_colors <- c(colors$range_blue, colors$range_purple, 
                    colors$range_teal, colors$range_orange)
  
  # Number line for Part 1
  line_width <- 4.0
  min_val <- 0
  max_val <- 35
  start_x <- panel1_cx - line_width/2
  end_x <- panel1_cx + line_width/2
  line_y <- panel1_cy - 0.2
  
  # Draw number line
  segments(start_x, line_y, end_x, line_y, col = colors$metal, lwd = 2)
  
  # Tick marks
  for (tick in seq(0, 35, by = 5)) {
    tick_x <- start_x + (tick - min_val) / (max_val - min_val) * line_width
    segments(tick_x, line_y - 0.06, tick_x, line_y + 0.06, col = colors$metal, lwd = 1.5)
    text(tick_x, line_y - 0.18, tick, col = colors$text_dim, cex = 0.4)
  }
  
  # Draw ranges as bars above number line
  range_y <- line_y + 0.32
  range_height <- 0.16
  
  for (i in 1:nrow(example_ranges)) {
    r_start <- example_ranges[i, 1]
    r_end <- example_ranges[i, 2]
    x1 <- start_x + (r_start - min_val) / (max_val - min_val) * line_width
    x2 <- start_x + (r_end - min_val) / (max_val - min_val) * line_width
    
    # Offset vertically to show overlap clearly
    y_off <- (i - 1) * 0.19
    
    rect(x1, range_y + y_off, x2, range_y + y_off + range_height,
         col = adjustcolor(range_colors[i], alpha = 0.6), 
         border = range_colors[i], lwd = 1.5)
    
    # Range label
    text((x1 + x2) / 2, range_y + y_off + range_height/2, 
         paste0(r_start, "-", r_end),
         col = colors$text_main, cex = 0.35, font = 2)
  }
  
  # Draw available IDs below number line
  id_y <- line_y - 0.42
  
  for (id in available_ids) {
    id_x <- start_x + (id - min_val) / (max_val - min_val) * line_width
    is_fresh <- any(id >= example_ranges[, 1] & id <= example_ranges[, 2])
    pt_col <- if (is_fresh) colors$fresh_green else colors$spoiled_red
    
    points(id_x, id_y, pch = 19, col = pt_col, cex = 1.2)
    text(id_x, id_y - 0.17, id, col = pt_col, cex = 0.4, font = 2)
  }
  
  # "available IDs" label on the left, outside panel
  text(panel1_cx - panel1_w/2 - 0.1, id_y, "available\nIDs", 
       col = colors$text_dim, cex = 0.35, adj = 1)
  
  # Result box for Part 1 (below the panel)
  result1_y <- panel1_cy - panel1_h/2 - 0.22
  rect(panel1_cx - 1.2, result1_y - 0.13,
       panel1_cx + 1.2, result1_y + 0.13,
       col = colors$bg_dark, border = colors$fresh_green, lwd = 2)
  text(panel1_cx, result1_y, "fresh IDs: 3  (5, 11, 17)",
       col = colors$fresh_green, cex = 0.55, font = 2)
  
  # ============ TRANSITION ARROW ============
  arrow_y_top <- 3.05
  arrow_y_bottom <- 2.75
  
  arrows(panel1_cx, arrow_y_top, panel1_cx, arrow_y_bottom, 
         col = colors$metal, lwd = 2, length = 0.1)
  text(panel1_cx + 0.15, (arrow_y_top + arrow_y_bottom)/2, 
       "merge overlapping ranges", 
       col = colors$text_dim, cex = 0.45, adj = 0)
  
  # ============ PART 2 ============
  segments(0.5, 2.55, 2.0, 2.55, col = colors$range_orange, lwd = 3)
  text(0.5, 2.25, "part 2: count all fresh IDs",
       col = colors$range_orange, cex = 1.0, adj = 0, font = 2)
  text(0.5, 1.95, "merge overlapping ranges, sum (end - start + 1)",
       col = colors$ingredient, cex = 0.65, adj = 0)
  
  # Part 2 Panel
  panel2_cx <- 5.0
  panel2_cy <- 1.2
  panel2_w <- 4.5
  panel2_h <- 1.0
  
  rect(panel2_cx - panel2_w/2, panel2_cy - panel2_h/2,
       panel2_cx + panel2_w/2, panel2_cy + panel2_h/2,
       col = colors$panel, border = colors$metal, lwd = 2)
  
  # Merged ranges
  merged_ranges <- matrix(c(3, 5, 10, 20), ncol = 2, byrow = TRUE)
  
  # Number line for Part 2
  min_val2 <- 0
  max_val2 <- 25
  start_x2 <- panel2_cx - line_width/2
  end_x2 <- panel2_cx + line_width/2
  line_y2 <- panel2_cy - 0.15
  
  segments(start_x2, line_y2, end_x2, line_y2, col = colors$metal, lwd = 2)
  
  # Tick marks
  for (tick in seq(0, 25, by = 5)) {
    tick_x <- start_x2 + (tick - min_val2) / (max_val2 - min_val2) * line_width
    segments(tick_x, line_y2 - 0.05, tick_x, line_y2 + 0.05, col = colors$metal, lwd = 1.5)
    text(tick_x, line_y2 - 0.15, tick, col = colors$text_dim, cex = 0.38)
  }
  
  # Draw merged ranges
  range_y2 <- line_y2 + 0.15
  range_height2 <- 0.22
  
  for (i in 1:nrow(merged_ranges)) {
    r_start <- merged_ranges[i, 1]
    r_end <- merged_ranges[i, 2]
    x1 <- start_x2 + (r_start - min_val2) / (max_val2 - min_val2) * line_width
    x2 <- start_x2 + (r_end - min_val2) / (max_val2 - min_val2) * line_width
    
    rect(x1, range_y2, x2, range_y2 + range_height2,
         col = adjustcolor(colors$fresh_green, alpha = 0.7), 
         border = colors$fresh_green, lwd = 2)
    
    # Range label above bar
    text((x1 + x2) / 2, range_y2 + range_height2 + 0.1, 
         paste0(r_start, "-", r_end),
         col = colors$fresh_green, cex = 0.4, font = 2)
    
    # Count label inside bar
    count <- r_end - r_start + 1
    text((x1 + x2) / 2, range_y2 + range_height2/2, 
         count,
         col = colors$bg_dark, cex = 0.5, font = 2)
  }
  
  # Result for Part 2 (to the right of panel)
  result2_x <- panel2_cx + panel2_w/2 + 0.25
  text(result2_x, panel2_cy + 0.05, "=  14",
       col = colors$range_orange, cex = 1.1, adj = 0, font = 2)
  text(result2_x, panel2_cy - 0.22, "(3 + 11)",
       col = colors$text_dim, cex = 0.45, adj = 0)
  
  # ============ LEGEND ============
  legend_y <- 0.38
  legend_cx <- 5.0
  legend_w <- 3.8
  
  # Legend background
  rect(legend_cx - legend_w, legend_y - 0.18,
       legend_cx + legend_w, legend_y + 0.18,
       col = colors$panel, border = colors$grid_line, lwd = 1)
  
  # Fresh indicator
  points(1.8, legend_y, pch = 19, col = colors$fresh_green, cex = 1.0)
  text(2.0, legend_y, "fresh", col = colors$text_main, cex = 0.48, adj = 0)
  
  # Spoiled indicator
  points(3.2, legend_y, pch = 19, col = colors$spoiled_red, cex = 1.0)
  text(3.4, legend_y, "spoiled", col = colors$text_main, cex = 0.48, adj = 0)
  
  # Range bar indicator
  rect(4.9, legend_y - 0.07, 5.3, legend_y + 0.07,
       col = adjustcolor(colors$range_blue, alpha = 0.6), 
       border = colors$range_blue, lwd = 1)
  text(5.45, legend_y, "ID range", col = colors$text_main, cex = 0.48, adj = 0)
  
  # Merged range indicator  
  rect(6.8, legend_y - 0.07, 7.2, legend_y + 0.07,
       col = adjustcolor(colors$fresh_green, alpha = 0.7), 
       border = colors$fresh_green, lwd = 1)
  text(7.35, legend_y, "merged", col = colors$text_main, cex = 0.48, adj = 0)
  
  # ============ FOOTER ============
  text(0.5, 0.1, "GRID",
       col = colors$text_dim, cex = 0.55, adj = 0, font = 2)
  
  text(9.5, 0.1, "adventofcode.com/2025/day/5",
       col = colors$text_dim, cex = 0.5, adj = 1)
  
  dev.off()
  message("Graphic saved to: ", output_path)
}


create_day5_graphic("2025/Day5/day05_graphic.png", aspect_ratio = "1:1")