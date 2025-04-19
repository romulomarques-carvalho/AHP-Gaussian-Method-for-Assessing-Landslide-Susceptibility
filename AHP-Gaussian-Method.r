# Landslide Susceptibility Analysis using Gaussian AHP Method
# R script for calculating weighted inference from multiple raster variables
# GitHub Repository: https://github.com/romulomarques-carvalho/AHP-Gaussian-Method-for-Assessing-Landslide-Susceptibility/blob/main/AHP-Gaussian-Method.r

# Load required packages
if (!require("raster")) install.packages("raster")
library(raster)

# Function to process raster variables
process_raster <- function(raster_path) {
  # Load raster
  r <- raster(raster_path)
  
  # Extract values and remove NA
  vals <- values(r)
  vals_clean <- vals[!is.na(vals)]
  
  # Normalize values to sum to 1
  vals_normalized <- vals_clean / sum(vals_clean)
  
  # Calculate statistics
  mean_val <- mean(vals_normalized)
  sd_val <- sd(vals_normalized)
  gaussian_factor <- sd_val / mean_val
  
  return(list(raster = r, gaussian_factor = gaussian_factor))
}

# Main analysis function
run_analysis <- function(raster_paths, output_path) {
  # Process all rasters
  results <- lapply(raster_paths, process_raster)
  
  # Extract components from results
  rasters <- lapply(results, function(x) x$raster)
  gaussian_factors <- sapply(results, function(x) x$gaussian_factor)
  
  # Normalize gaussian factors
  total <- sum(gaussian_factors)
  normalized_weights <- gaussian_factors / total
  
  # Calculate weighted inference
  inference <- 0
  for (i in seq_along(rasters)) {
    inference <- inference + (rasters[[i]] * normalized_weights[i])
  }
  
  # Set projection and resolution (example values - adjust as needed)
  projection(inference) <- "+proj=utm +zone=23 +south +datum=WGS84 +units=m +no_defs"
  res(inference) <- c(5, 5)
  
  # Save output
  writeRaster(inference, filename = output_path, format = "GTiff", overwrite = TRUE)
  
  return(list(inference = inference, weights = normalized_weights))
}

# Example usage (replace with your actual paths)
if (FALSE) {  # Change to TRUE to run the example
  # List of raster paths (replace with your actual files)
  raster_files <- paste0("data/input/v", sprintf("%02d", 1:17), ".tif")
  
  # Output path
  output_file <- "output/inference_ahp_gauss.tif"
  
  # Run analysis
  analysis_results <- run_analysis(raster_files, output_file)
  
  # View weights
  print(data.frame(Variable = paste0("v", sprintf("%02d", 1:17)),
                   Weight = analysis_results$weights))
}
