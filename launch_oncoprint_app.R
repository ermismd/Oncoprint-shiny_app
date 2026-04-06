## =========================
##  Launcher for Oncoprint Shiny App
## =========================

# Check and install required packages
required_packages <- c(
  "shiny", "shinyWidgets", "shinyjs",
  "ComplexHeatmap", 
  "circlize", "dplyr", "tidyr", "tibble", 
  "stringr", "RColorBrewer", "data.table", "DT"
)

# Check which packages need to be installed
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  cat("Installing missing packages...\n")
  
  # CRAN packages
  cran_packages <- c("shiny", "shinyWidgets", "shinyjs", "dplyr", "tidyr", 
                     "tibble", "stringr", "RColorBrewer", "data.table", "DT")
  cran_missing <- intersect(missing_packages, cran_packages)
  if (length(cran_missing) > 0) {
    install.packages(cran_missing)
  }
  
  # Bioconductor packages
  bioc_packages <- c("ComplexHeatmap", "circlize")
  bioc_missing <- intersect(missing_packages, bioc_packages)
  if (length(bioc_missing) > 0) {
    if (!requireNamespace("BiocManager", quietly = TRUE)) {
      install.packages("BiocManager")
    }
    BiocManager::install(bioc_missing)
  }
}

# Launch the app

#  specific path to the folder that contains the app.R
shiny::runApp("C:/Users/S234866/OneDrive - University of Texas Southwestern/Desktop/OncoPrint_app", launch.browser = TRUE)

# Use relative path if this script is IN the app folder
shiny::runApp(launch.browser = TRUE)
