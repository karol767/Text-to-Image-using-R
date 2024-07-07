# Check if required packages are installed, if not, install them
if (!requireNamespace("jpeg", quietly = TRUE)) install.packages("jpeg")
if (!requireNamespace("png", quietly = TRUE)) install.packages("png")
if (!requireNamespace("tools", quietly = TRUE)) install.packages("tools")
if (!requireNamespace("grid", quietly = TRUE)) install.packages("grid")

# Load the required packages
library(jpeg)
library(png)
library(tools)

# Set the directory path and quote file name
dir_path <- "/home/cake/Downloads/Archive"
quote_file <- "quotes.txt"

# Read the quotes
quotes <- readLines(file.path(dir_path, quote_file), warn = FALSE)
# Remove quotation marks and trim whitespace
quotes <- gsub('^"|"$', '', trimws(quotes))

# Function to wrap text
wrap_text <- function(text, width = 32) {
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
calculate_text_width <- function(text, font_size) {
  # Split the text into lines
  lines <- strsplit(text, "\n")[[1]]
  
  # Calculate the width of each line
  line_widths <- sapply(lines, function(line) {
    strwidth(line, units = "inches", cex = font_size)  # Assuming 12 pt is the base font size
  })
  
  # Return the maximum width
  print(lines)
  max(line_widths)
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
  
  cake <- calculate_text_width(quote, quote_size) * 96
  total_height <- (lengths(regmatches(quote, gregexpr("\n", quote)))) * strheight("A", cex = 2) * 2 * width / cake
  total_height <- total_height + 0.07
  
  # Add overlay
  rect(0, 0, 1, total_height, col = overlay, border = NA)

  # Add quote
  if (is.null(quote_y_position)) {
    quote_y_position <- 0.35
  }
  
  text(x = 0.5, y = total_height - 0.01, labels = quote, col = "white", cex = width / cake * quote_size, adj = c(0.5, 1), family = "Copperplate Gothic Std 29 BC")
  
  # Add website URL to the right
  # text(x = 0.5, y = url_y_position, labels = website_url, col = "white", cex = width / cake * url_size, adj = c(0.5, 1))
  
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
  changed_quote <- paste0(current_quote, '\n', 'W3SCHOOL.COM')
  
  tryCatch({
    process_image(
      current_image, 
      changed_quote, 
      overlay_height = 0.5, 
      overlay_opacity = 0.2,
      quote_size = 2.4,
      quote_y_position = 0.45,
      url_size = 2.4,
      url_x_position = 0.98,
      url_y_position = 0.05
    )
  }, error = function(e) {
    cat("Error processing", current_image, ":", conditionMessage(e), "\n")
  })
}
cat("All images processed.\n")