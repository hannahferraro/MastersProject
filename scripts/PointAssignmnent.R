#Hannah Ferraro Masters Thesis
#Intended to add points to jpeg files for percent cover on tiles 
#THIS WILL NOT WORK WITH FILES THAT ARE NOT JPEG, JGP FORMAT
#Note that you may have to install Rtools44 to install this jpeg library
#Note that the size of the points may need to be changed using the cex = option in the last section of code before downloading the new pts photos (default is 2, 0.5)


#load library or install
library(jpeg)

#set working directory to the folder with the images 
setwd("../../Photographs/Quadrats/Cropped/") 

pt_assign <- function(pts, path, color) {
  
  # Create point labels
  labs <- if(pts <= 40) {
    seq(from = 1, to = pts)
  } else {
    seq(from = 1, to = 40) # Labels 1-40, ignoring points beyond 40
  
  }
  
  # List image files located in the file pathway
  files <- dir(path = ".", pattern = "\\.(JPG|jpeg)$", full.names = TRUE)
  
  # Define point and text color if different from default
  if (missing(color)) {
    color <- "red"
  } else {
    color <- color
  }
  
  # Assign random points and save images
  for (c in 1:length(files)) {
    
    # Load the JPEG image
    img <- jpeg::readJPEG(files[c])
    
    # Generate random x and y coordinates for the points
    
    # Set a margin (5% of width/height)
    margx <- dim(img)[2] * 0.05
    margy <- dim(img)[1] * 0.05
    
    # Generate random coordinates *inside* the margin bounds
    x_coords <- runif(pts, min = margx, max = dim(img)[2] - margx)
    y_coords <- runif(pts, min = margy, max = dim(img)[1] - margy)
  
    
    # Sort the coordinates based on x values
    sorted_indices <- order(x_coords)
    x_coords <- x_coords[sorted_indices]
    y_coords <- y_coords[sorted_indices]
    
    #3% margin
    margx <- dim(img)[3]/33
    margy <- dim(img)[2]/33
    
    #making vector posit to set position of text so can move based on where point is on image.
    posit <- rep(3, length(x_coords))
    
    if(any(x_coords<margx)){
      posit[which(x_coords<margx)] <- 4
    }
    if(any(x_coords>dim(img)[2]-margx)){
      posit[which(x_coords>dim(img)[2]-margx)] <- 2
    }
    if(any(y_coords>dim(img)[1]-margy)){
      posit[which(y_coords>dim(img)[1]-margy)] <- 1
    }
    
    #set new filename
    export_file <- paste0(tools::file_path_sans_ext(files[c]), "_", "pts.jpg") # Create new file name with "_pts" extension
    
    #start process of making jpeg
    jpeg(filename = export_file, width = dim(img)[2], height = dim(img)[1], units = "px", res = 300, bg=NA)
    
    # Create a blank plot with the same dimensions as the image
    par(mar=c(0,0,0,0),xaxs = 'i',yaxs='i')
    plot(1, 1, type = "n", xlim = c(0, dim(img)[2]), ylim = c(0, dim(img)[1]), xlab = "", ylab = "", xaxt = "n", yaxt = "n")
    
    # Add the image without borders
    rasterImage(img, 0, 0, dim(img)[2], dim(img)[1], interpolate = FALSE)
    
    # Add the labeled points
    points(x_coords, y_coords, pch = 3, col = color)
    text(x_coords, y_coords+3, labs, pos = posit, col = color, cex = 2, font = 2)
    
    dev.off()
  }
}

pt_assign(40,"Photographs/Quadrats/Cropped")