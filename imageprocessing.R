# Check if required packages are installed, if not, install them
if (!requireNamespace("jpeg", quietly = TRUE)) install.packages("jpeg")
if (!requireNamespace("png", quietly = TRUE)) install.packages("png")
if (!requireNamespace("tools", quietly = TRUE)) install.packages("tools")
if (!requireNamespace("grid", quietly = TRUE)) install.packages("grid")

# Load the required packages
library(jpeg)
library(png)
library(tools)
library(grid)

# Set the directory path and quote file name
dir_path <- "/home/cake/Downloads/Archive"
quote_file <- "quotes.txt"

# Read the quotes
quotes <- readLines(file.path(dir_path, quote_file), warn = FALSE)
# Remove quotation marks and trim whitespace
quotes <- gsub('^"|"$', '', trimws(quotes))

# Function to wrap text
wrap_text <- function(text, width = 40) {
  paste(strwrap(text, width = width), collapse = "\n")
}

# Prepare wrapped quotes
wrapped_quotes <- sapply(quotes, wrap_text)

# Your website URL
website_url <- "www.yourwebsite.com"

# Function to get dominant colors
get_dominant_colors <- function(img, n = 3) {
  colors <- rgb(img[,,1], img[,,2], img[,,3])
  col_freq <- table(colors)
  dominant <- names(sort(col_freq, decreasing = TRUE)[1:n])
  return(dominant)
}

# Function to process a single image
process_image <- function(image_file, quote, 
                          overlay_height = 0.3, 
                          overlay_opacity = 0.8,
                          quote_size = 1.2,
                          quote_y_position = NULL,
                          url_size = 0.8,
                          url_x_position = 0.95,
                          url_y_position = 0.02) {
  # Determine file type and read the image
  file_ext <- tolower(file_ext(image_file))
  if (file_ext == "jpg" || file_ext == "jpeg") {
    img <- readJPEG(file.path(dir_path, image_file))
  } else if (file_ext == "png") {
    img <- readPNG(file.path(dir_path, image_file))
  } else {
    stop("Unsupported file format")
  }
  
  # Get image dimensions
  height <- dim(img)[1]
  width <- dim(img)[2]

  # Get dominant colors
  dominant_colors <- get_dominant_colors(img)
  overlay_color <- col2rgb(dominant_colors[1]) / 255
  
  # Create overlay
  overlay <- rgb(overlay_color[1], overlay_color[2], overlay_color[3], alpha = overlay_opacity)
  
  # Create a new plot
  output_file <- file.path(dir_path, paste0("processed_", tools::file_path_sans_ext(image_file), ".png"))
  png(output_file, width = width, height = height, units = "px")
  par(mar = c(0,0,0,0))
  plot(1, type = "n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
  
  # Add original image
  rasterImage(img, 0, 0, 1, 1)
  
  # Add overlay
  rect(0, 0, 1, overlay_height, col = overlay, border = NA)
  
  text_grob <- textGrob(gsub("\n", " ", quote), gp = gpar(cex = quote_size), x = 0, y = 0)
  text_width <- grid.width(text_grob)
  
  print(text_width)
  
  x_position <- 0 + (1 - text_width / width) / 2
  # Add quote
  if (is.null(quote_y_position)) {
    quote_y_position <- overlay_height / 2
  }
  text(x = x_position, y = quote_y_position, labels = quote, col = "white", cex = quote_size, adj = c(0, 1))
  
  # Add website URL to the right
  text(x = url_x_position, y = url_y_position, labels = website_url, col = "white", cex = url_size, adj = c(1, 0.5))
  
  dev.off()
  
  cat("Processed:", image_file, "with quote:", substr(quote, 1, 30), "...\n")
}

# Get all JPEG and PNG images in the directory
image_files <- list.files(dir_path, pattern = "\\.(jpe?g|png)$", ignore.case = TRUE)

# Process each image with a different quote
for (i in seq_along(image_files)) {
  quote_index <- (i - 1) %% length(wrapped_quotes) + 1
  current_image <- image_files[i]
  current_quote <- wrapped_quotes[quote_index]
  
  tryCatch({
    process_image(
      current_image, 
      current_quote, 
      overlay_height = 0.4, 
      overlay_opacity = 0.7,
      quote_size = 0.15 * 12,
      quote_y_position = 0.2,
      url_size = 1.5,
      url_x_position = 0.98,
      url_y_position = 0.03
    )
  }, error = function(e) {
    cat("Error processing", current_image, ":", conditionMessage(e), "\n")
  })
}

cat("All images processed.\n")