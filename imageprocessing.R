# Load the necessary libraries
library(raster)
library(ggplot2)
library(stringr)
library(grid)

# Set the directory path and file name
dir_path <- "/home/cake/Downloads"
file_name <- "test.jpg"

print(dir_path)
print(file_name)

if (!dir.exists(dir_path)) {
  print("Directory does not exist!")
} else {
  print("Directory exists!")
}

# Read the image
tryCatch({
  img <- raster(file.path(file_path, file_name))
}, error = function(e) {
  print(paste("Error reading raster:", e))
})

# Get the dominant colors of the image
dominant_colors <- hist(img[], breaks = 3, plot = FALSE)$colors

# Set the transparency of the overlay
alpha <- 0.8

# Create a rectangle with the dominant colors
overlay <- rgb( dominant_colors[1], dominant_colors[2], dominant_colors[3], alpha )

# Read the quote from the file
quote_file <- "/home/cake/Downloads/quotes.txt"
quote <- readLines(quote_file)

# Determine the font size and color based on the image dimensions
img_width <- nrow(img)
img_height <- ncol(img)
font_size <- min(img_width / 20, img_height / 20)
font_color <- rgb(0, 0, 0)  # black

# Create a text object for the quote
quote_text <- paste0(quote, "\n", "Your Website URL")
quote_text_grob <- textGrob(quote_text, hjust = 0, vjust = 1, gp = gpar(fontsize = font_size, col = font_color))

# Add the text to the overlay
overlay_grob <- grobTree(rectGrob(x = c(0.1 * img_width, 0.9 * img_width), y = c(0.9 * img_height, 0.9 * img_height), width = 0.8 * img_width, height = 0.1 * img_height),
                         quote_text_grob)
print("#######")
# Combine the image and overlay
print(alpha)
alpha_raster <- raster(alpha)
combined_img <- overlay(img, alpha_raster, fun = "*")

print("@@@@@@")
# Save the output image
png(file.path(dir_path, "output.png"), width = img_width, height = img_height)
grid.draw(gList(combined_img))
dev.off()
