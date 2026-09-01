# Sourced automatically by testthat::test_dir()/test_file() before any test file.
# Loads the app's R/ modules (not network calls) so tests can call the functions
# directly without spinning up Shiny.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(zoo)
  library(slider)
  library(glue)
  library(htmltools)
})

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))

source(file.path(app_root, "R", "cache_layer.R"))
source(file.path(app_root, "R", "utils.R"))
source(file.path(app_root, "R", "evapotranspiration.R"))
source(file.path(app_root, "R", "growth_calcs.R"))
source(file.path(app_root, "R", "predict_model.R"))
source(file.path(app_root, "R", "pedotransfer.R"))
source(file.path(app_root, "R", "fetch_soil.R"))
source(file.path(app_root, "R", "fetch_power.R"))
source(file.path(app_root, "R", "yield.R"))
source(file.path(app_root, "R", "report.R"))
