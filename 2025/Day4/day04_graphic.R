#!/usr/bin/env Rscript

# Advent of Code 2025 Day 4 graphic

library(showtext)
font_add_google("Montserrat", "montserrat")
font_add_google("Atkinson Hyperlegible", "atkinson")
font_add_google("JetBrains Mono", "jetbrains")
showtext_auto()

create_day4_graphic <- function(
    output_path = "2025/Day4/day04_graphic.png",
    aspect_ratio = "1:1"
) {
  
  # Printing department / warehouse palette
  colors <- list(
    bg_dark      = "#0d1117",
    panel        = "#161b22",
    paper_white  = "#f0e6d3",
    forklift_org = "#f0883e",
    access_green = "#3fb950",
    blocked_red  = "#f85149",
    metal        = "#8b949e",
    text_main    = "#e6edf3",
    text_dim     = "#7d8590",
    grid_line    = "#30363d",
    wheel_dark   = "#1a1a1a"
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
  
  # Helper function to draw a mini grid of paper rolls
  draw_roll_grid <- function(cx, cy, grid_str, highlight_type = "part1",
                             accent_color, label, result_text) {
    
    rows <- strsplit(grid_str, "\n")[[1]]
    n_rows <- length(rows)
    n_cols <- nchar(rows[1])
    
    cell_size <- 0.28
    total_w <- n_cols * cell_size
    total_h <- n_rows * cell_size
    start_x <- cx - total_w / 2
    start_y <- cy + total_h / 2
    
    # Panel background
    rect(cx - total_w/2 - 0.4, cy - total_h/2 - 0.6,
         cx + total_w/2 + 0.4, cy + total_h/2 + 0.5,
         col = colors$panel, border = colors$metal, lwd = 2)
    
    # Label
    text(cx - total_w/2 - 0.25, cy + total_h/2 + 0.3, label,
         col = accent_color, cex = 0.6, adj = 0, family = "montserrat", font = 2)
    
    # Draw grid
    for (r in 1:n_rows) {
      chars <- strsplit(rows[r], "")[[1]]
      for (c in 1:n_cols) {
        cell_x <- start_x + (c - 0.5) * cell_size
        cell_y <- start_y - (r - 0.5) * cell_size
        
        char <- chars[c]
        
        # Grid cell background
        rect(cell_x - cell_size/2, cell_y - cell_size/2,
             cell_x + cell_size/2, cell_y + cell_size/2,
             col = colors$bg_dark, border = colors$grid_line, lwd = 0.5)
        
        if (char == "@") {
          # Paper roll (blocked, many neighbors)
          points(cell_x, cell_y, pch = 19, col = colors$paper_white, cex = 1.4)
          text(cell_x, cell_y, "@", col = colors$bg_dark, cex = 0.45, 
               family = "jetbrains", font = 2)
        } else if (char == "x") {
          # Accessible roll (highlighted)
          points(cell_x, cell_y, pch = 19, col = accent_color, cex = 1.4)
          text(cell_x, cell_y, "@", col = colors$bg_dark, cex = 0.45,
               family = "jetbrains", font = 2)
        } else if (char == ".") {
          # Empty cell
          text(cell_x, cell_y, "·", col = colors$grid_line, cex = 0.4,
               family = "jetbrains")
        }
      }
    }
    
    # Result display
    rect(cx - 1.3, cy - total_h/2 - 0.5,
         cx + 1.3, cy - total_h/2 - 0.2,
         col = colors$bg_dark, border = accent_color, lwd = 1.5)
    
    text(cx, cy - total_h/2 - 0.35, result_text,
         col = accent_color, cex = 0.55, family = "jetbrains", font = 2)
  }
  
  # Forklift icon - side view facing right
  draw_forklift <- function(x, y, size = 1.0) {
    s <- size * 0.5  # scale factor
    
    # Ground line reference: y is the ground level
    ground <- y - s * 0.4
    
    # Wheels (circles at ground level)
    # Back wheel (larger)
    symbols(x - s*0.5, ground + s*0.15, circles = s*0.15, 
            inches = FALSE, add = TRUE, bg = colors$wheel_dark, fg = colors$metal, lwd = 2)
    # Front wheel (smaller)
    symbols(x + s*0.2, ground + s*0.12, circles = s*0.12, 
            inches = FALSE, add = TRUE, bg = colors$wheel_dark, fg = colors$metal, lwd = 2)
    
    # Chassis (connects wheels)
    rect(x - s*0.6, ground + s*0.15, x + s*0.25, ground + s*0.28,
         col = colors$forklift_org, border = "#c96a28", lwd = 1)
    
    # Main cab body
    rect(x - s*0.55, ground + s*0.28, x + s*0.05, ground + s*0.75,
         col = colors$forklift_org, border = "#c96a28", lwd = 2)
    
    # Cab roof / overhead guard
    segments(x - s*0.55, ground + s*0.75, x + s*0.15, ground + s*0.75,
             col = "#c96a28", lwd = 3)
    # Roof supports
    segments(x + s*0.1, ground + s*0.28, x + s*0.1, ground + s*0.75,
             col = "#c96a28", lwd = 2)
    
    # Counterweight at back
    rect(x - s*0.7, ground + s*0.2, x - s*0.55, ground + s*0.55,
         col = "#c96a28", border = NA)
    
    # Mast (vertical) - in front of cab
    rect(x + s*0.2, ground + s*0.1, x + s*0.32, ground + s*0.9,
         col = colors$metal, border = "#6e7681", lwd = 1.5)
    
    # Forks (two horizontal prongs extending right)
    rect(x + s*0.32, ground + s*0.08, x + s*0.9, ground + s*0.14,
         col = colors$metal, border = "#6e7681", lwd = 1)
    rect(x + s*0.32, ground + s*0.28, x + s*0.9, ground + s*0.34,
         col = colors$metal, border = "#6e7681", lwd = 1)
    
    # Fork backrest (vertical bar behind forks)
    rect(x + s*0.28, ground + s*0.05, x + s*0.35, ground + s*0.45,
         col = "#6e7681", border = NA)
  }
  
  # Forklift decoration
  draw_forklift(8.8, 9.0, size = 1.0)
  
  # HEADER
  text(0.5, 9.1, "advent of code 2025", 
       col = colors$forklift_org, cex = 1.8, adj = 0, family = "montserrat", font = 2)
  text(0.5, 8.5, "day 4: printing department  (help forklifts access paper rolls)",
       col = colors$metal, cex = 1.0, adj = 0, family = "atkinson")
  
  # SETUP
  text(0.5, 7.9, "setup", 
       col = colors$text_dim, cex = 0.85, adj = 0, family = "montserrat", font = 2)
  
  text(0.5, 7.55,
       "input: grid where @ = paper roll, . = empty space",
       col = colors$text_main, cex = 0.95, adj = 0, family = "atkinson")
  
  text(0.5, 7.2,
       "accessible: roll has < 4 neighbors in 8 adjacent cells",
       col = colors$text_main, cex = 0.95, adj = 0, family = "atkinson")
  
  # Grid center x positions (symmetric around center 5.0)
  grid1_cx <- 2.7
  grid2_cx <- 7.3
  grid_cy <- 3.7  # Same y for both grids
  
  # PART 1 section - centered over left grid
  segments(grid1_cx - 1.5, 6.7, grid1_cx + 0.0, 6.7, col = colors$access_green, lwd = 3)
  text(grid1_cx - 1.5, 6.4, "part 1: count accessible",
       col = colors$access_green, cex = 0.95, adj = 0, family = "montserrat", font = 2)
  text(grid1_cx - 1.5, 6.05, "count neighbors for each @",
       col = colors$paper_white, cex = 0.7, adj = 0, family = "jetbrains")
  
  # PART 2 section - centered over right grid
  segments(grid2_cx - 1.5, 6.7, grid2_cx + 0.0, 6.7, col = colors$forklift_org, lwd = 3)
  text(grid2_cx - 1.5, 6.4, "part 2: remove iteratively",
       col = colors$forklift_org, cex = 0.95, adj = 0, family = "montserrat", font = 2)
  text(grid2_cx - 1.5, 6.05, "remove accessible, repeat",
       col = colors$paper_white, cex = 0.7, adj = 0, family = "jetbrains")
  
  # Part 1 grid (accessible highlighted)
  part1_grid <- "..xx.xx@x.
x@@.@.@.@@
@@@@@.x.@@
@.@@@@..@.
x@.@@@@.@x
.@@@@@@@.@
.@.@.@.@@@
x.@@@.@@@@
.@@@@@@@@.
x.x.@@@.x."
  
  draw_roll_grid(cx = grid1_cx, cy = grid_cy,
                 grid_str = part1_grid,
                 highlight_type = "part1",
                 accent_color = colors$access_green,
                 label = "INITIAL STATE",
                 result_text = "accessible: 13 rolls")
  
  # Part 2 grid (showing final state after many removals)
  part2_grid <- "..........
..........
....x.....
...@@@....
...@@@@...
...@@@@@..
...@.@.@@.
...@@.@@@.
...@@@@@..
....@@@..."
  
  draw_roll_grid(cx = grid2_cx, cy = grid_cy,
                 grid_str = part2_grid,
                 highlight_type = "part2",
                 accent_color = colors$forklift_org,
                 label = "AFTER 8 ROUNDS",
                 result_text = "total removed: 43 rolls")
  
  # Process arrow between grids (centered between the two grids)
  arrow_x <- (grid1_cx + grid2_cx) / 2
  arrows(arrow_x - 0.5, grid_cy, arrow_x + 0.5, grid_cy, 
         col = colors$metal, lwd = 2, length = 0.1)
  text(arrow_x, grid_cy + 0.25, "repeat", col = colors$text_dim, cex = 0.55, family = "atkinson")
  
  # Legend - centered at bottom with better spacing
  legend_cx <- 5.0
  legend_w <- 2.3
  legend_bottom <- 0.55
  legend_top <- 1.55
  
  rect(legend_cx - legend_w, legend_bottom, legend_cx + legend_w, legend_top, 
       col = colors$panel, border = colors$metal, lwd = 1)
  
  # Legend title - centered at top with padding
  text(legend_cx, legend_top - 0.2, "legend", col = colors$text_dim, cex = 0.6, 
       family = "montserrat", font = 2)
  
  # Legend items - centered row below title
  item_y <- legend_bottom + 0.35
  
  # Blocked item (left side)
  points(legend_cx - 1.5, item_y, pch = 19, col = colors$paper_white, cex = 1.2)
  text(legend_cx - 1.25, item_y, "blocked (≥4 neighbors)", col = colors$text_main, 
       cex = 0.55, adj = 0, family = "atkinson")
  
  # Accessible item (right side)
  points(legend_cx + 0.6, item_y, pch = 19, col = colors$access_green, cex = 1.2)
  text(legend_cx + 0.85, item_y, "accessible (<4)", col = colors$text_main, 
       cex = 0.55, adj = 0, family = "atkinson")
  
  # FOOTER
  segments(0.5, 0.4, 9.5, 0.4, col = colors$panel, lwd = 1)
  
  text(0.5, 0.2, "GRID", 
       col = colors$text_dim, cex = 0.7, adj = 0, family = "montserrat", font = 2)
  
  text(9.5, 0.2, "adventofcode.com/2025/day/4",
       col = colors$text_dim, cex = 0.65, adj = 1, family = "atkinson")
  
  dev.off()
  message("Graphic saved to: ", output_path)
}


create_day4_graphic("2025/Day4/day04_graphic.png", aspect_ratio = "1:1")