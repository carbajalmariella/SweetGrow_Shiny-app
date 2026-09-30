# SweetGrow

Sweetpotato storage-root dry weight (growth) predictor.

**Live app:** https://carbajalmariella.shinyapps.io/sweetgrow/

## What it does

Given a location, planting date, cultivar, and irrigation scenario, SweetGrow pulls live weather (NASA POWER) and soil data (SSURGO in the US, SoilGrids elsewhere) and predicts storage-root dry weight over time, along with an estimated fresh/marketable weight, growing degree days (GDD), and a water stress index (WSI).

Growth curves were fit offline on two NC State research trials (Caswell 2021, Sandhills 2022); see the in-app **Methodology** tab for the model equations, calibration details, and known limitations. Reference: Carbajal, M., Weidner, D., Huseth, A., Hoogenboom, G., Raymundo, R., Williams, C., and Nelson, N., *How effectively can an intermediate modeling approach predict the growth and development of sweetpotatoes?* (manuscript in preparation).

## Running locally

```r
shiny::runApp("app.R")
```

Requires the packages listed at the top of `app.R` (shiny, dplyr, ggplot2, plotly, leaflet, soilDB, sf, rmarkdown, etc.).

## Repo structure

- `app.R` -- UI + server (single-file Shiny app)
- `R/` -- backend modules: weather/soil fetching, evapotranspiration, GDD/water-stress calcs, model prediction, yield conversion, formatting helpers, and the downloadable-report builder
- `inst/` -- fitted model catalog, bootstrap draws, and the R Markdown report template
- `tests/testthat/` -- unit tests (`Rscript run_tests.R` to run them)

## Deployment

The live app is deployed to shinyapps.io separately from this repo (via `rsconnect::deployApp()`) -- pushing or merging changes here does **not** automatically redeploy it. Re-deploy manually after merging to update the live app.
