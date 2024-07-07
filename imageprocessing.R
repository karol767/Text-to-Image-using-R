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
# Font Parameter recommend (24, 2.3, 1.05, 002, 1.5)
g_quote_size <- 22           # range (12, 30)            Verse font size                           To increase font size, select value < 24
g_url_size <- 2.3            # range (1, 4)              Url font size                             To increase font size, select value >
x_axis_padding <- 1.05       # range (1, 1.1)            X axis padding ( left, right padding )    To increase font size, select value >
top_padding_size <- 0.02     # range (0, 0.03)           Y axis padding ( top, bottom padding )    To increase font size, select value >
bottom_padding_size <- 1.5   # range (1, 2.5)            Y axis padding ( top, bottom padding )    To increase font size, select value >

dir_path <- "/home/cake/Downloads/Archive"               # Set the directory path and quote file name
quote_file <- "quotes.txt"                               # Set the directory path and quote file name

# Read the quotes
quotes <- readLines(file.path(dir_path, quote_file), warn = FALSE)
# Remove quotation marks and trim whitespace
quotes <- gsub('^"|"$', '', trimws(quotes))

quotes <- toupper(quotes)
# Function to wrap text

wrap_text <- function(text, width = g_quote_size) {
  paste(strwrap(text, width = width), collapse = "\n")
}

# Prepare wrapped quotes
wrapped_quotes <- sapply(quotes, wrap_text)
# Your website URL
website_url <- "GODSVERSE.ORG"

# Function to get dominant colors
get_dominant_colors <- function(img, n = 3) {
  colors <- rgb(img[,,1], img[,,2], img[,,3])
  col_freq <- table(colors)
  dominant <- names(sort(col_freq, decreasing = TRUE)[1:n])
  return(dominant)
}

calculate_image_brightness <- function(img) {
  # Convert to grayscale if it's an RGB image
  if (dim(img)[3] == 3) {
    gray_img <- 0.2989 * img[,,1] + 0.5870 * img[,,2] + 0.1140 * img[,,3]
  } else {
    gray_img <- img
  }
  
  # Calculate average brightness
  mean(gray_img)
}
get_contrasting_color <- function(color) {
  rgb <- col2rgb(color)
  if ((rgb[1]*0.299 + rgb[2]*0.587 + rgb[3]*0.114) / 255 > 0.5) {
    return("black")
  } else {
    return("white")
  }
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
  par(mar = c(0, 0, 0, 0))
  plot(1, type = "n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
  
  # Add original image
  rasterImage(img, 0, 0, 1, 1)

  total_height <- (lengths(regmatches(quote, gregexpr("\n", quote))) + bottom_padding_size) * strheight("A", cex = quote_size) * 1.75
  total_height <- height / 700 * total_height

  brightness <- calculate_image_brightness(img)
  
  # Adjust overlay opacity based on brightness
  adjusted_opacity <- overlay_opacity + (brightness - 0.5) * 0.4
  adjusted_opacity <- max(0.6, min(0.95, adjusted_opacity))  # Keep opacity between 0.6 and 0.95
  
  # Get dominant colors
  dominant_colors <- get_dominant_colors(img)
  overlay_color <- col2rgb(dominant_colors[1]) / 255
  
  # Darken the overlay color for lighter images
  overlay_color <- overlay_color * (1 - brightness * 0.5)
  
  # Create overlay
  overlay <- rgb(overlay_color[1], overlay_color[2], overlay_color[3], alpha = adjusted_opacity)
  
  # Determine text color based on overlay color
  text_color <- get_contrasting_color(overlay)
  # Add overlay
  rect(0, 0, 1, total_height, col = overlay, border = NA)
  
  # Add quote
  if (is.null(quote_y_position)) {
    quote_y_position <- 0.35
  }
  
  text(x = 0.5, y = total_height - top_padding_size, labels = quote, col = "white", cex = c((quote_size * width / 466 * x_axis_padding), (quote_size * height / 700)), adj = c(0.5, 1, 3), family = "Copperplate Gothic Std 29 BC")

  # Add website URL to the right
  text(x = 0.5, y = url_y_position, labels = website_url, col = "white", cex = c((url_size * width / 466 * x_axis_padding), (url_size * height / 700)), adj = c(0.5, 1))
  
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
  changed_quote <- paste0(current_quote, '\n', '')
  changed_quote <- paste0(changed_quote, '\n', '')
  
  tryCatch({
    process_image(
      current_image, 
      changed_quote, 
      overlay_height = 0.5,
      overlay_opacity = 0.2,
      quote_size = 1.6 + (30 - g_quote_size) / 10,
      quote_y_position = 0.45,
      url_size = g_url_size,
      url_x_position = 0.98,
      url_y_position = 0.05
    )
  }, error = function(e) {
    cat("Error processing", current_image, ":", conditionMessage(e), "\n")
  })
}
cat("All images processed.\n")