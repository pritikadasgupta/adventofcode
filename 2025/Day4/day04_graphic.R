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
    grid_line    = "#30363d"
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
  
  # Improved forklift icon
  draw_forklift <- function(x, y, size = 0.5) {
    # Cab/body
    rect(x - size*0.4, y - size*0.2, x + size*0.2, y + size*0.4,
         col = colors$forklift_org, border = colors$metal, lwd = 1.5)
    # Roof
    rect(x - size*0.35, y + size*0.4, x + size*0.1, y + size*0.55,
         col = colors$forklift_org, border = colors$metal, lwd = 1)
    # Mast (vertical)
    rect(x + size*0.2, y - size*0.3, x + size*0.3, y + size*0.5,
         col = colors$metal, border = NA)
    # Forks (horizontal prongs)
    rect(x + size*0.3, y - size*0.25, x + size*0.7, y - size*0.15,
         col = colors$metal, border = NA)
    rect(x + size*0.3, y - size*0.05, x + size*0.7, y + 0.05,
         col = colors$metal, border = NA)
    # Wheels (simple circles)
    symbols(x - size*0.2, y - size*0.35, circles = size*0.12, 
            inches = FALSE, add = TRUE, bg = "#1a1a1a", fg = colors$text_dim, lwd = 2)
    symbols(x + size*0.05, y - size*0.35, circles = size*0.09, 
            inches = FALSE, add = TRUE, bg = "#1a1a1a", fg = colors$text_dim, lwd = 2)
  }
  
  # Forklift decoration
  draw_forklift(8.8, 8.9, size = 0.6)
  
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
  
  # PART 1 section - centered over left grid
  segments(1.3, 6.7, 2.8, 6.7, col = colors$access_green, lwd = 3)
  text(1.3, 6.4, "part 1: count accessible",
       col = colors$access_green, cex = 0.95, adj = 0, family = "montserrat", font = 2)
  text(1.3, 6.05, "count neighbors for each @",
       col = colors$paper_white, cex = 0.7, adj = 0, family = "jetbrains")
  
  # PART 2 section - centered over right grid
  segments(5.7, 6.7, 7.2, 6.7, col = colors$forklift_org, lwd = 3)
  text(5.7, 6.4, "part 2: remove iteratively",
       col = colors$forklift_org, cex = 0.95, adj = 0, family = "montserrat", font = 2)
  text(5.7, 6.05, "remove accessible, repeat",
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
  
  draw_roll_grid(cx = 2.8, cy = 3.7,
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
  
  draw_roll_grid(cx = 7.2, cy = 3.7,
                 grid_str = part2_grid,
                 highlight_type = "part2",
                 accent_color = colors$forklift_org,
                 label = "AFTER 8 ROUNDS",
                 result_text = "total removed: 43 rolls")
  
  # Process arrow between grids
  arrows(4.4, 3.7, 5.4, 3.7, col = colors$metal, lwd = 2, length = 0.1)
  text(4.9, 3.95, "repeat", col = colors$text_dim, cex = 0.55, family = "atkinson")
  
  # Legend
  rect(0.4, 0.7, 5.0, 1.5, col = colors$panel, border = colors$metal, lwd = 1)
  text(0.6, 1.3, "legend:", col = colors$text_dim, cex = 0.6, 
       family = "montserrat", font = 2)
  
  # Legend items
  points(0.9, 1.0, pch = 19, col = colors$paper_white, cex = 1.2)
  text(1.15, 1.0, "blocked (≥4 neighbors)", col = colors$text_main, 
       cex = 0.55, adj = 0, family = "atkinson")
  
  points(3.1, 1.0, pch = 19, col = colors$access_green, cex = 1.2)
  text(3.35, 1.0, "accessible (<4 neighbors)", col = colors$text_main, 
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