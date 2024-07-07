# Set the directory path and file names


# Check if jpeg package is installed, if not, install it
if (!requireNamespace("jpeg", quietly = TRUE)) {
  install.packages("jpeg")
}

# Load the jpeg package
library(jpeg)

# Set the directory path and file names
dir_path <- "/home/cake/Downloads"
image_file <- "test.jpg"
quote_file <- "quotes.txt"

# Read the image
img <- readJPEG(file.path(dir_path, image_file))

# Get image dimensions
height <- dim(img)[1]
width <- dim(img)[2]

# Function to get dominant colors
get_dominant_colors <- function(img, n = 3) {
  colors <- rgb(img[,,1], img[,,2], img[,,3])
  col_freq <- table(colors)
  dominant <- names(sort(col_freq, decreasing = TRUE)[1:n])
  return(dominant)
}

# Get dominant colors
dominant_colors <- get_dominant_colors(img)
overlay_color <- col2rgb(dominant_colors[1]) / 255

# Create overlay
overlay <- rgb(overlay_color[1], overlay_color[2], overlay_color[3], alpha = 0.8)

# Read the quote
quote <- readLines(file.path(dir_path, quote_file))
quote <- paste(strwrap(quote, width = 40), collapse = "\n")  # Adjust width as needed

# Your website URL
website_url <- "GODSVERSE.ORG"

# Create a new plot
jpeg(file.path(dir_path, "result.jpg"), width = width, height = height, units = "px", quality = 100)
par(mar = c(0,0,0,0))
plot(1, type = "n", xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
  
# Add original image
rasterImage(img, 0, 0, 1, 1)

# Add overlay
rect(0, 0, 1, 0.3, col = overlay, border = NA)

# Add quote
text(0.5, 0.15, quote, col = "white", cex = 3, font = 1, adj = c(0.5, 0))

# Add website URL
text(0.5, 0.05, website_url, col = "white", cex = 3, font = 1, adj = c(0.5, 0))

dev.off()
