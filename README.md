# Stopover and Home Range: Birds Migration Analysis

## Project Purpose

This project analyzes bird migration patterns, focusing on:
- **Stopover sites**: Locations where birds rest during migration.
- **Home range at wintering**: Areas occupied by birds during the winter.
- **Habitat preference**: Analysis of environmental factors influencing site selection.

The goal is to provide insights into migratory behavior and habitat use, supporting conservation and ecological research.

## Directory Structure
- **setup.R**  
  Script for initializing the R environment, installing required packages, and setting up project paths.

- **datasets/**  
  Contains raw and processed data:
  - `hivernages.csv`: Main dataset with bird tracking or observation data.
  - `aoi_extend/`: Extent of the area of interest data - shapefile.
  - `stopovers/`: Data related to identified stopover sites.

- **home_range/**  
  Spatial data (e.g., shapefiles: `.shp`, `.dbf`, `.prj`, `.shx`) for home range 
  analysis at wintering sites.

- **plots/**  
  Output directory for generated figures and visualizations.

- **scripts/**  
  Contains R scripts for data processing, analysis, and visualization. Typical 
  scripts may include:
  - Data cleaning and preparation
  - Stopover site detection - `01_stopover.R`
  - Home range estimation - `02_hivernage_hr.R`
  - Habitat preference modeling (not yet added)

- **tables/**  
  Output directory for summary tables, results, and intermediate data.

## Getting Started

1. Open the project in RStudio using the `.Rproj` file.
2. Run `setup.R` to install dependencies and configure paths.
3. Explore scripts in the `scripts/` directory for specific analyses.
4. Results will be saved in the `plots/` and `tables/` directories.
