# Create directory for input and output flow
dirs <- c("scripts", # to store scripts
          "plots/stopovers", # to save plots
          "tables", # for table output - e.g. table for stopover site per ind/species
          "datasets/stopovers/sud_nord", # where to put the original datasets
          "datasets/stopovers/nord_sud"
          )
# Loop to create these directory
for (dr in dirs) {
  if (!dir.exists(dr)) {
    dir.create(dr, recursive = TRUE)
  }
}

## Install the packages required for this project
if (! "pak" %in% rownames(installed.packages())) {
  install.packages(pak)
}

if (! "maimer" %in% rownames(installed.packages())) {
  pak::pkg_install("stangandaho/maimer")
}

packages <- c("sf", "dplyr", "ggplot2", "tidyr", "giscoR", 
              "adehabitatHR", "rstatix", "lmtest", "geomtextpath")
# Loop to installed the package if not installed.
for (pkg in packages) {
  if(! pkg %in% rownames(installed.packages())){
    pak::pkg_install(pkg = pkg)
  }
}

# Load the packages
suppressMessages({
  lapply(packages, require, character.only = TRUE)
})

# Remove previous objects created
rm(dirs, dr, packages, pkg)

## Load 
library(showtext)
font_add("mr", "fonts/montserrat/Montserrat-Regular.ttf")
font_add("msbi", "fonts/montserrat/Montserrat-SemiBoldItalic.ttf")
font_add("mm", "fonts/montserrat/Montserrat-Medium.ttf")

showtext_auto()
# A message to inform the setup is set successfully
message("Setup loaded successfully!")