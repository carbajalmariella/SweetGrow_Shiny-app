library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(curl)
library(jsonlite)
library(tidyr)
library(purrr)
library(zoo)
library(slider)
library(sf)
library(soilDB)
library(glue)
library(DT)
library(leaflet)
library(cachem)
library(memoise)
library(rmarkdown)
library(htmltools)
library(plotly)

# ============================================================
# Load modules
# ============================================================
source("R/cache_layer.R")
source("R/utils.R")
source("R/evapotranspiration.R")
source("R/fetch_power.R")
source("R/pedotransfer.R")
source("R/fetch_soil.R")
source("R/growth_calcs.R")
source("R/predict_model.R")
source("R/yield.R")
source("R/report.R")

catalog <- load_catalog("inst/model_catalog.rds")
lgg_boot_draws <- load_lgg_boot_draws("inst/lgg_boot_draws_gdd.rds")

# ============================================================
# A few non-ASCII symbols used in UI text, built from their Unicode
# codepoint via intToUtf8() rather than typed as literal source bytes.
# Some hosting environments launch R in a non-UTF-8 (e.g. "C") locale,
# under which a literal non-ASCII byte in the source can fail to parse
# correctly and render as a garbled "<U+00B0>"-style escape -- intToUtf8()
# builds the character at runtime from a plain ASCII integer, which
# sidesteps that regardless of the launching environment's locale.
DEG  <- intToUtf8(176)   # deg sign, as in 16.9 C
SUP2 <- intToUtf8(178)   # superscript 2, as in R-squared
GEQ  <- intToUtf8(8805)  # >=
WARN_SIGN <- intToUtf8(9888) # warning triangle

# ============================================================
# Backend constants -- not user-facing. These were choices made once
# during model development, not something a stakeholder (or a researcher
# just running a prediction) should need to know exists, let alone tune
# per run.
# ============================================================
SOIL_AGG_DEPTH_CM <- 30    # depth used to average the fetched soil profile into one SLLL/SDUL/BD layer
ROOT_DEPTH_CM <- 60        # effective rooting depth used in the WSI water-holding calculation
Y0_STRESSED <- 0.05        # baseline offset for the water-stressed logistic integration
APPLY_STRESS <- TRUE       # always apply the WSI stress response (the whole point of this tool)
USE_FW_TRAPZ <- FALSE      # trapezoid vs. rectangular fw(WSI) integration -- rectangular matches the manuscript's own fits
POWER_LAG_DAYS <- 7        # NASA POWER's typical backfill lag; recommended 5-7 days

# ============================================================
# Reusable "compact/expandable" table widget: shows the first 5 or last 5
# rows by default (toggle, not two separate tables), or the full table
# paginated 15 rows/page when expanded.
# ============================================================
# ggplotly() mangles trace names into "(label,1)" whenever a plot mixes more
# than one legend-producing aesthetic (fill + color, or color + linetype) --
# clean the trace names back up before handing the widget to the UI.
clean_plotly_legend <- function(gp) {
  gp$x$data <- lapply(gp$x$data, function(tr) {
    if (!is.null(tr$name)) tr$name <- sub("^\\(([^,]+),.*\\)$", "\\1", tr$name)
    tr
  })
  gp
}

# Adds a "Days after planting" axis along the top of an already-built
# plotly chart. ggplotly() silently drops ggplot2's native
# scale_*_*(sec.axis = ...) on conversion (confirmed empirically, for both
# scale_x_date and scale_x_continuous), so this reconstructs it by hand: a
# second x-axis overlaying the primary one, its range padded and locked to
# match the primary exactly so ticks line up pixel-for-pixel, with tick
# positions given in the PRIMARY axis's own units (x, e.g. Date or GDD) but
# labeled with the DAT value that corresponds to each one -- interpolated
# via approx() since GDD accumulates at a variable daily rate (not linear
# in DAT) while Date is (kept generic so one code path covers both).
# y_ref must be a y-value already inside the plot's real data range, so the
# invisible dummy trace this needs doesn't perturb the primary y-axis scale.
add_dat_axis <- function(ply, x, dat, y_ref) {
  # ggplotly() represents a Date x-axis internally as plain numeric
  # epoch-day integers (its axis "type" stays "linear", not a real plotly
  # date axis) -- everything handed back to plotly here (range, tickvals,
  # the dummy trace's x) has to be plain numeric to match that, or it's
  # silently misinterpreted (e.g. a Date range serializes to ISO strings,
  # which a linear axis can't parse, and the chart renders blank). pretty()
  # still runs on the original x so Date input gets "nice" date-based
  # breaks rather than nice-looking-but-meaningless raw epoch-day numbers.
  x_num <- as.numeric(x)
  rng <- range(x_num, na.rm = TRUE)
  span <- diff(rng); if (span == 0) span <- 1
  padded <- c(rng[1] - 0.04 * span, rng[2] + 0.04 * span)
  breaks_num <- as.numeric(pretty(x, n = 6))
  breaks_num <- breaks_num[breaks_num >= rng[1] & breaks_num <= rng[2]]
  dat_at_breaks <- stats::approx(x = x_num, y = dat, xout = breaks_num,
                                  ties = "ordered", rule = 2)$y

  ply %>%
    add_trace(
      data = data.frame(x_ = breaks_num, y_ = y_ref),
      x = ~x_, y = ~y_, xaxis = "x2", type = "scatter", mode = "markers",
      marker = list(opacity = 0), hoverinfo = "skip", showlegend = FALSE,
      inherit = FALSE
    ) %>%
    layout(
      xaxis = list(range = padded),
      xaxis2 = list(
        overlaying = "x", side = "top", range = padded, showgrid = FALSE,
        tickmode = "array", tickvals = breaks_num, ticktext = as.character(round(dat_at_breaks)),
        title = list(text = "Days after planting", font = list(size = 11))
      ),
      # ggplotly sizes the top margin for the ggplot title alone, before this
      # function adds a second tick row + axis title above the plot area --
      # without extra headroom the two crowd/overlap each other.
      margin = list(t = 130)
    )
}

table_widget_ui <- function(id, title) {
  tagList(
    h5(title),
    radioButtons(paste0(id, "_headtail"), NULL, choices = c("First 5" = "head", "Last 5" = "tail"),
                 selected = "head", inline = TRUE),
    checkboxInput(paste0(id, "_full"), "Show full table (15 rows/page)", value = FALSE),
    conditionalPanel(condition = sprintf("input['%s_full'] == false", id), tableOutput(paste0(id, "_preview"))),
    conditionalPanel(condition = sprintf("input['%s_full'] == true", id), DTOutput(paste0(id, "_dt")))
  )
}

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  tags$style(HTML(".nav-tabs { flex-wrap: nowrap; } .nav-tabs > li > a { padding: 8px 10px; font-size: 14px; white-space: nowrap; }")),
  fluidRow(
    column(9,
      titlePanel(
        tags$span("SweetGrow", tags$small(style = "font-weight:normal; color:#666; margin-left:10px; font-size:21px;",
                                           "Sweetpotato storage-root dry Growth predictor")),
        windowTitle = "SweetGrow"
      )
    ),
    column(3, style = "padding-top: 22px; text-align: right;",
      radioButtons("temp_unit", "Temperature unit",
                   choices = setNames(c("C", "F"), paste0(DEG, c("C", "F"))),
                   selected = "C", inline = TRUE)
    )
  ),
  fluidRow(
    column(12,
           tags$p(
             style = "font-size:14px; margin-bottom:2px;",
             "Predicts sweetpotato storage-root dry weight (t/ha) from live weather and soil data for any location -- ",
             "pick a spot, planting date, and cultivar, then click ", tags$b("Run"), ".", tags$br(),
             "Need more control? ", tags$b("Advanced options"), " lets you set the trial calibration, model type, and stress parameters."
           ),
           tags$p(
             style = "font-size:12px; color:#b45309; margin-bottom:4px;",
             WARN_SIGN, " Fit on 2 NC research trials only -- treat as a rough guide elsewhere. See ",
             tags$b("Methodology"), " for details."
           ),
           tags$hr()
    )
  ),

  sidebarLayout(
    sidebarPanel(
      h4("Location"),
      leafletOutput("map", height = 260),
      tags$small(style = "color:#666;", "Click the map to set a location, or type coordinates below."),
      br(), br(),
      fluidRow(
        column(6, numericInput("lat", "Latitude", value = 35.7796, step = 0.0001)),
        column(6, numericInput("lon", "Longitude", value = -78.6382, step = 0.0001))
      ),

      hr(style = "margin: 10px 0;"),
      h4("Dates"),
      fluidRow(
        column(6, dateInput("planting_date", "Planting date", value = as.Date("2024-06-01"))),
        column(6,
          conditionalPanel(
            condition = "input.harvest_spec_type == 'date'",
            dateInput("harvest_date", "Harvest date", value = as.Date("2024-06-01") + 110)
          ),
          conditionalPanel(
            condition = "input.harvest_spec_type == 'dat'",
            numericInput("harvest_dat_input", "DAT (90-120)", value = 110, min = 90, max = 120, step = 1)
          )
        )
      ),
      radioButtons("harvest_spec_type", NULL,
                   choices = c("By harvest date" = "date", "By days after planting (DAT)" = "dat"),
                   selected = "date", inline = TRUE),
      helpText("Harvest must be 90-120 days after planting -- adjusted automatically if it drifts outside that."),

      hr(style = "margin: 10px 0;"),
      h4("Cultivar"),
      uiOutput("cultivar_ui"),
      uiOutput("bellevue_warning_ui"),

      hr(style = "margin: 10px 0;"),
      h4("Irrigation"),
      radioButtons("irr_simple", NULL, choices = c("Rainfed (no irrigation)" = "rainfed", "Irrigated" = "irrigated"), selected = "rainfed"),
      conditionalPanel(
        condition = "input.irr_simple == 'irrigated'",
        radioButtons("irr_irrigated_type", NULL,
                     choices = c("Every week (mm)" = "weekly", "Specific dates (CSV)" = "dates"),
                     selected = "weekly", inline = TRUE),
        conditionalPanel(
          condition = "input.irr_irrigated_type == 'weekly'",
          numericInput("irr_simple_mm", "Amount per week (mm)", value = 25, min = 0, step = 1)
        ),
        conditionalPanel(
          condition = "input.irr_irrigated_type == 'dates'",
          fileInput("irr_simple_file", "Upload CSV (columns: date, irrigation_mm)", accept = c(".csv")),
          tags$div(style = "font-size:12px; color:#666; margin: -6px 0 6px;", "...or add events one at a time:"),
          fluidRow(
            column(6, dateInput("irr_manual_date", "Date", value = Sys.Date())),
            column(6, numericInput("irr_manual_mm", "mm", value = 10, min = 0, step = 1))
          ),
          actionButton("irr_manual_add", "Add row", class = "btn-sm"),
          actionButton("irr_manual_clear", "Clear all", class = "btn-sm"),
          br(), br(),
          tableOutput("irr_manual_tbl")
        )
      ),

      hr(),
      checkboxInput("show_advanced", tags$b("Advanced options"), value = FALSE),
      conditionalPanel(
        condition = "input.show_advanced == true",
        helpText("Override the trial calibration, model family, or water-stress curve. Everything else stays",
                 " fixed to the manuscript's settings -- see Methodology & Notes.")
      ),

      conditionalPanel(
        condition = "input.show_advanced == true",

        hr(), h4("Model (advanced)"),
        helpText("Auto-picks Caswell 2021 or Sandhills 2022 by matching your scenario's WSI to whichever trial",
                 " ran closer to it. Override here to force a trial or the pooled fit."),
        selectInput("season", "Trial site/year (overrides auto-selection)", choices = sort(unique(catalog$Season)),
                    selected = "Seasons 2021-2022"),
        radioButtons("model_type", "Model type", choices = c("logistic","piecewise"), selected = "logistic"),
        uiOutput("model_explain_ui"),

        hr(), h4("Water-stress response"),
        helpText("Water stress is always applied. Defaults are the manuscript's fitted values for the",
                 " selected model (PWL: 0.25/0.50/0.35; LGG: 0.30/0.65/0.65) -- override only for sensitivity testing."),
        fluidRow(
          column(4, numericInput("w1", "w1", value = 0.30, min = 0, max = 1, step = 0.01)),
          column(4, numericInput("w2", "w2", value = 0.65, min = 0, max = 1, step = 0.01)),
          column(4, numericInput("fmin", "f_min", value = 0.65, min = 0, max = 1, step = 0.01))
        ),
        uiOutput("fw_eqn_ui"),
        helpText(
          "w1/w2: WSI range over which the growth multiplier ramps from f_min (full stress) to 1 (no stress). ",
          "f_min: the minimum multiplier, under full water stress."
        )
      ),

      hr(),
      actionButton("run", "Run", class = "btn-primary"),
      hr(),
      downloadButton("dl_csv", "Download predictions (CSV)"),
      br(), br(),
      downloadButton("dl_report", "Download report (HTML)")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Environment",
                 br(),
                 h4("Soil profile"),
                 tableOutput("tbl_soil_profile"),
                 br(),
                 h4("Soil (aggregated to rooting depth)"),
                 tableOutput("tbl_soil_agg"),
                 br(),
                 h4("Climate summary"),
                 tableOutput("tbl_climate_summary")
        ),
        tabPanel("Temp & Water",
                 plotlyOutput("p_temp", height = 460),
                 br(),
                 plotlyOutput("p_water_et", height = 460),
                 br(),
                 table_widget_ui("tbl_clim", "Daily climate data")
        ),
        tabPanel("GDD & WSI",
                 tags$p(
                   style = "font-size:13px; color:#444; margin-top:8px;",
                   tags$b("GDD"), " (Growing Degree Days) tracks accumulated heat units that drive crop development. ",
                   tags$b("WSI"), " (Water Stress Index, 0-1) tracks how much plant-available soil water there is on a given day (1 = no stress, 0 = maximum stress). ",
                   "See ", tags$b("Methodology"), " for the equations, key values, and the source paper."
                 ),
                 plotlyOutput("p_gdd", height = 460),
                 br(),
                 plotlyOutput("p_wsi", height = 460),
                 br(),
                 table_widget_ui("tbl_gdd", "Daily GDD & WSI data")
        ),
        tabPanel("Prediction",
                 uiOutput("eqn_box"),
                 plotlyOutput("p_pred", height = 540),
                 uiOutput("pred_caption_ui"),
                 br(),
                 h4("Run summary"),
                 tableOutput("tbl_pred_summary"),
                 br(),
                 uiOutput("explain_ui"),
                 br(),
                 table_widget_ui("tbl_pred", "Daily prediction data")
        ),
        tabPanel("Methodology",
          tags$div(
            style = "max-width: 780px; font-size: 14px; line-height: 1.5; margin-top: 12px;",

            tags$h4("Reference"),
            tags$p(
              "Carbajal, M., Weidner, D., Huseth, A., Hoogenboom, G., Raymundo, R., Williams, C., and Nelson, N. ",
              tags$em("How effectively can an intermediate modeling approach predict the growth and development of sweetpotatoes?"),
              " (manuscript in preparation)."
            ),

            tags$h4("What this tool does"),
            tags$p(
              "Growth curves were fit once, offline, on two NC State research trials (Caswell 2021, Sandhills ",
              "2022 -- one growing season each). This app doesn't refit anything: it pulls live daily weather ",
              "(NASA POWER) and soil water-holding properties (SSURGO in the US, SoilGrids + a texture-based ",
              "pedotransfer function elsewhere) for whatever location and dates you enter, runs the same water-",
              "balance and growing-degree-day calculations the original study used, and evaluates the ",
              "already-fitted curve against that new weather/soil trajectory."
            ),

            tags$div(
              style = "background:#fff3cd; border:1px solid #ffe08a; border-radius:6px; padding:10px 14px; margin: 10px 0;",
              tags$strong("Extrapolation caution: "),
              "these curves were fit on two NC research trials only, one season each. Cross-validating the ",
              "model of one trial against the other's actual weather already shows real prediction loss within ",
              "this same dataset -- mean R", SUP2, " around 0.68 for the piecewise model, and worse than just predicting ",
              "the average for the logistic model under some configurations; one cultivar's cross-trial transfer ",
              "stays poor regardless of how its parameters are shared. Applying this tool to a new location -- ",
              "especially one far from central NC's climate and soils -- is a bigger extrapolation than that. ",
              "Treat predictions as a rough guide, not a calibrated forecast, outside conditions similar to ",
              "those two trials."
            ),

            tags$h4("Which trial's calibration gets used"),
            tags$p(
              "By default the app doesn't ask you to pick a trial by name -- it computes your scenario's own ",
              "water stress index (WSI) from your real weather/soil/irrigation inputs, and matches it to ",
              "whichever trial's ", tags$em("actually realized"), " water stress it's closer to: Caswell 2021 was ",
              "rain-dominant with minimal supplemental irrigation (mean WSI ~0.26 during the trial); Sandhills ",
              "2022 was irrigated weekly and rain-buffered (mean WSI ~0.79). \"Advanced\" mode lets you override ",
              "this and pick a trial (or the pooled \"Seasons 2021-2022\" fit) manually."
            ),

            tags$h4("Growing Degree Days (GDD) & Water Stress Index (WSI)"),
            tags$p(
              tags$code("GDD_day"), " = 0 whenever ", tags$code("Tmin"), paste0(" < 16.9", DEG, "C (the crop's estimated base "),
              "temperature); otherwise ", tags$code(paste0("min(Tmax, 29.2", DEG, "C) - 16.9", DEG, "C")),
              paste0(" (Tmax is capped at a 29.2", DEG, "C "),
              "ceiling, above which extra heat doesn't speed development further). ", tags$code("GDD_cum"), " is the ",
              "running sum of ", tags$code("GDD_day"), " since planting."
            ),
            tags$p(
              tags$b("WSI"), " (0-1, 1 = no stress) comes from a day-by-day \"tipping bucket\" soil-water balance: ",
              "available water = rain + irrigation - crop ET, capped between 0 and a maximum ",
              tags$code("AW_max = (field capacity - wilting point) x root depth x bulk density x 10"), " (mm), ",
              "then normalized to 0-1 and smoothed with a 10-day trailing average. The dashed line at 0.8 in the ",
              "WSI plot is used elsewhere in this app as a rough \"comfort threshold\" below which stress starts mattering."
            ),
            tags$p(
              "Full derivation and the fitted growth equations themselves are in the source paper -- see ",
              tags$b("Reference"), " above."
            ),

            tags$h4("Model families: piecewise (PWL) vs. logistic (LGG)"),
            tags$p(
              "Piecewise applies water stress by rescaling elapsed time (an \"effective GDD\" axis) before a ",
              "breakpoint regression; logistic applies it to daily growth increments directly, integrated day by ",
              "day. PWL was fit against Kc-adjusted evapotranspiration (a crop-coefficient ramp over the season, ",
              "Mulovhedzi et al. 2020); LGG against raw reference ET0 -- each model's own best-scoring choice, ",
              "confirmed structural rather than arbitrary by testing both models forced onto the other's ET ",
              "source. The pooled \"Seasons 2021-2022\" fit under the logistic model specifically is flagged by ",
              "the underlying analysis as a non-reproduced approximation."
            ),

            tags$h4("Uncertainty band"),
            tags$p(
              "For the logistic model, the shaded band is a real joint prediction interval: the curve evaluated ",
              "at 40 residual-bootstrap refits of the fitted model, shown as the 25th-75th percentile (an ",
              "interquartile \"typical range\", not the wider and noisier 95% tails -- several fits are only ",
              "loosely constrained by ~8-9 sampling points, and a handful of bootstrap refits land on the ",
              "optimizer's starting-grid boundary rather than a distinct optimum). For the piecewise model, only ",
              "the breakpoint has a fitted confidence interval, so the band there is a rougher envelope from ",
              "that single parameter."
            ),

            tags$h4("Dry weight vs. fresh (marketable) yield"),
            tags$p(
              "The fitted models predict storage-root ", tags$b("dry"), " weight ",
              "in t/ha -- not what gets harvested or sold. The app also shows an estimated ", tags$b("fresh"),
              " weight, dividing by each cultivar's dry-matter fraction (dry weight / fresh weight), sourced ",
              "where possible from published cultivar-release literature rather than guessed:"
            ),
            tags$ul(
              tags$li(tags$b("Covington: 19.8%"), " -- Yencho et al. (2008), HortScience 43(6):1911-1914, ",
                      "averaged over 2001-2006 NC field trials (the same growing region as this manuscript's own).",
                      " Supersedes an earlier estimate derived from this manuscript's own raw plant samples (39.1%),",
                      " which was roughly double the published figure and inconsistent sample-to-sample -- kept",
                      " out as unreliable."),
              tags$li(tags$b("Bayou Belle: 23.0%"), " -- US Plant Patent PP23,785 (LSU AgCenter, 2013),",
                      " LA/MS/AR trials 2009-2011 -- a different growing region than this manuscript's NC trials."),
              tags$li(tags$b("Bellevue: 25.4%"), " and ", tags$b("Monaco: 26.6%"), " -- no directly comparable",
                      " published fresh-tissue figure was found for either; these are this manuscript's own",
                      paste0(" raw paired-sample estimate (Caswell 2021 only, DAT ", GEQ, " 90, n=12 per cultivar) -- treat as"),
                      " a rougher approximation than the growth curve itself.")
            ),

            tags$h4("Known simplifications"),
            tags$ul(
              tags$li("Root depth (60 cm) and the soil-profile aggregation depth (30 cm) are fixed constants, not fit to any particular site."),
              tags$li("NASA POWER precipitation and temperature don't exactly match ground-station data used to fit the models (checked directly against the two trials' own stations: ~8-10% precipitation difference, and Sandhills-area temperatures ran noticeably warmer in POWER)."),
              tags$li("Evapotranspiration is computed as FAO-56 Penman-Monteith reference ET0 from POWER's raw fields (POWER has no reference-ET parameter of its own).")
            )
          )
        )
      )
    )
  )
)

# ============================================================
# Server
# ============================================================
server <- function(input, output, session) {

  # Wires up the head/tail preview + full paginated DT for a table_widget_ui(id, ...).
  make_table_widget <- function(id, tbl_reactive) {
    output[[paste0(id, "_preview")]] <- renderTable({
      df <- tbl_reactive()
      if (identical(input[[paste0(id, "_headtail")]], "tail")) utils::tail(df, 5) else utils::head(df, 5)
    }, rownames = FALSE)
    output[[paste0(id, "_dt")]] <- renderDT({
      DT::datatable(tbl_reactive(), options = list(dom = "tip", pageLength = 15, scrollX = TRUE), rownames = FALSE)
    })
  }

  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = isolate(input$lon), lat = isolate(input$lat), zoom = 6) %>%
      addMarkers(lng = isolate(input$lon), lat = isolate(input$lat), layerId = "site")
  })

  observeEvent(input$map_click, {
    click <- input$map_click
    updateNumericInput(session, "lat", value = round(click$lat, 4))
    updateNumericInput(session, "lon", value = round(click$lng, 4))
  })

  observeEvent(list(input$lat, input$lon), {
    req(is.numeric(input$lat), is.numeric(input$lon))
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(lng = input$lon, lat = input$lat, layerId = "site")
  }, ignoreInit = TRUE)

  output$cultivar_ui <- renderUI({
    req(input$season)
    cults <- catalog %>% filter(Season == input$season) %>% pull(Cultivar) %>% unique() %>% sort()
    selectInput("cultivar", NULL, choices = cults, selected = cults[1])
  })

  # Bellevue's logistic (LGG) fit doesn't transfer across trials at all
  # (R2 = -13.07 Caswell->Sandhills, vs. 0.35-0.64 for piecewise/PWL --
  # pwlogit_metrics_gdd.csv) -- restrict it to piecewise and warn, rather
  # than silently letting a stakeholder pick a model known not to work for
  # this cultivar.
  observeEvent(input$cultivar, {
    if (identical(input$cultivar, "Bellevue")) {
      updateRadioButtons(session, "model_type", choices = c("piecewise" = "piecewise"), selected = "piecewise")
    } else {
      cur <- if (identical(input$model_type, "piecewise")) "piecewise" else "logistic"
      updateRadioButtons(session, "model_type", choices = c("logistic", "piecewise"), selected = cur)
    }
  })

  output$bellevue_warning_ui <- renderUI({
    req(input$cultivar)
    if (!identical(input$cultivar, "Bellevue")) return(NULL)
    tags$div(
      style = "background:#f8d7da; border:1px solid #f1aeb5; border-radius:6px; padding:8px 10px; margin-top:6px; font-size:12px; color:#58151c;",
      tags$strong("Low reliability for Bellevue: "),
      paste0("its logistic (LGG) fit doesn't transfer across trials at all (R", SUP2, " = -13.07 predicting Sandhills from the "),
      "Caswell fit) -- this app restricts Bellevue to the piecewise (PWL) model, which transfers only moderately ",
      paste0("well itself (R", SUP2, " = 0.35-0.64, versus ", GEQ, " 0.82 for the other cultivars). Treat Bellevue predictions with "),
      "extra caution."
    )
  })

  output$model_explain_ui <- renderUI({
    pooled_logistic_warning <- if (identical(input$season, "Seasons 2021-2022") && input$model_type == "logistic") {
      tags$div(
        style = "font-size: 12px; color: #8a6100; background:#fff3cd; border-radius:4px; padding:6px 8px; margin-top:6px;",
        tags$b("Note: "),
        "the pooled fit is flagged as a non-reproduced approximation under the logistic model -- prefer a single",
        " trial, or the piecewise model, if reproducibility matters."
      )
    }
    approach_txt <- if (input$model_type == "logistic") {
      tags$div(
        style = "font-size: 12px; color: #555; margin-top: 6px;",
        tags$b("Logistic approach: "),
        "Fit vs GDD_cum. Water stress scales the daily growth increment by fw(WSI) as it's integrated."
      )
    } else {
      tags$div(
        style = "font-size: 12px; color: #555; margin-top: 6px;",
        tags$b("Piecewise approach: "),
        "Fit vs GDD_eff, which accumulates GDD_cum increments scaled by fw(WSI)."
      )
    }
    tagList(approach_txt, pooled_logistic_warning)
  })

  output$fw_eqn_ui <- renderUI({
    req(input$w1, input$w2, input$fmin)
    tags$div(
      style = "padding:6px 8px; background:#f6f6f6; border-radius:6px; font-size:12px; margin:6px 0;",
      tags$b("fw(WSI) = "),
      tags$code(sprintf(
        "%.2f  [WSI <= %.2f];   %.2f + %.2f x (WSI-%.2f)/(%.2f-%.2f)  [%.2f < WSI < %.2f];   1  [WSI >= %.2f]",
        input$fmin, input$w1, input$fmin, 1 - input$fmin, input$w1, input$w2, input$w1, input$w1, input$w2, input$w2
      ))
    )
  })

  # Keep the stress-parameter defaults matched to whichever model type is
  # selected (PWL vs LGG were each fit with their own universal w1/w2/f_min
  # and Kc treatment -- see R/growth_calcs.R). A researcher can still edit
  # these by hand afterward for sensitivity testing.
  observeEvent(input$model_type, {
    if (input$model_type == "piecewise") {
      updateNumericInput(session, "w1", value = 0.25)
      updateNumericInput(session, "w2", value = 0.50)
      updateNumericInput(session, "fmin", value = 0.35)
    } else {
      updateNumericInput(session, "w1", value = 0.30)
      updateNumericInput(session, "w2", value = 0.65)
      updateNumericInput(session, "fmin", value = 0.65)
    }
  })

  # Keep harvest_date within [planting+90, planting+120] automatically instead
  # of a slider -- typing an out-of-range harvest date snaps back to +110.
  # Only matters in "By harvest date" mode; DAT mode is clamped in harvest_date_r().
  observeEvent(input$planting_date, {
    req(input$planting_date)
    new_min <- as.Date(input$planting_date) + 90
    new_max <- as.Date(input$planting_date) + 120
    cur <- input$harvest_date
    new_val <- if (is.null(cur) || is.na(cur) || cur < new_min || cur > new_max) {
      as.Date(input$planting_date) + 110
    } else {
      as.Date(cur)
    }
    updateDateInput(session, "harvest_date", value = new_val, min = new_min, max = new_max)
  })

  harvest_date_r <- reactive({
    req(input$planting_date)
    if (identical(input$harvest_spec_type, "dat")) {
      req(input$harvest_dat_input)
      dat <- min(120, max(90, input$harvest_dat_input))
      as.Date(input$planting_date) + dat
    } else {
      req(input$harvest_date)
      as.Date(input$harvest_date)
    }
  })

  # Manually-entered irrigation events (date, mm), an alternative to uploading a CSV.
  irr_manual_rows <- reactiveVal(tibble::tibble(date = as.Date(character()), irrigation_mm = numeric()))

  observeEvent(input$irr_manual_add, {
    req(input$irr_manual_date, input$irr_manual_mm)
    new_row <- tibble::tibble(date = as.Date(input$irr_manual_date), irrigation_mm = input$irr_manual_mm)
    irr_manual_rows(dplyr::bind_rows(irr_manual_rows(), new_row) %>% dplyr::arrange(date))
  })

  observeEvent(input$irr_manual_clear, {
    irr_manual_rows(tibble::tibble(date = as.Date(character()), irrigation_mm = numeric()))
  })

  output$irr_manual_tbl <- renderTable({
    req(nrow(irr_manual_rows()) > 0)
    irr_manual_rows() %>% mutate(date = format(date, "%Y-%m-%d"))
  }, rownames = FALSE)

  # Builds the day-by-day irrigation series: Rainfed, a flat weekly amount, or
  # a specific-dates schedule (uploaded CSV, or rows added manually above --
  # the upload takes priority if both are present).
  irrigation_series_r <- reactive({
    req(harvest_date_r())
    dates <- seq(as.Date(input$planting_date), harvest_date_r(), by = "day")

    if (!identical(input$irr_simple, "irrigated")) {
      tibble::tibble(date = dates, irrigation_mm = 0)
    } else if (identical(input$irr_irrigated_type, "dates")) {
      file_df <- if (!is.null(input$irr_simple_file)) {
        read_irrigation_csv(input$irr_simple_file$datapath)
      } else {
        irr_manual_rows()
      }
      shiny::validate(shiny::need(nrow(file_df) > 0, "Upload a CSV or add at least one irrigation event."))
      make_irrigation_series(dates = dates, mode = "file", file_df = file_df)
    } else {
      make_irrigation_schedule(
        dates = dates, planting_date = input$planting_date,
        every_n = 7, amount_mm = input$irr_simple_mm, offset_days = 0
      )
    }
  })

  results <- eventReactive(input$run, {

    harvest_date <- harvest_date_r()
    DAT <- as.integer(harvest_date - as.Date(input$planting_date))

    clim <- get_climate_power_api(
      lat = input$lat, lon = input$lon,
      start_date = input$planting_date,
      end_date = harvest_date,
      lag_days_power = POWER_LAG_DAYS
    )

    df0 <- clim %>%
      mutate(
        tmax_in = if (input$temp_unit == "F") c_to_f(tmax_c) else tmax_c,
        tmin_in = if (input$temp_unit == "F") c_to_f(tmin_c) else tmin_c
      )

    irr <- irrigation_series_r()

    # Soil (profile -> aggregate), source dispatched by input$soil_source
    soil_prof <- tryCatch(
      get_soil_profile(input$lat, input$lon, soil_source = "auto"),
      error = function(e) {
        shiny::validate(shiny::need(FALSE, conditionMessage(e)))
      }
    )
    soil_agg <- aggregate_soil_to_depth(soil_prof, target_depth_cm = SOIL_AGG_DEPTH_CM)

    df <- df0 %>%
      left_join(irr, by = "date") %>%
      mutate(
        date = as.Date(date),
        irrigation_mm = replace_na(irrigation_mm, 0),
        water = precip_mm + irrigation_mm,
        ET = et_mm
      ) %>%
      filter(date >= as.Date(input$planting_date), date <= harvest_date) %>%
      arrange(date)

    df_gdd <- compute_gdd2(df, temp_unit = input$temp_unit) %>%
      mutate(DAT = as.integer(date - as.Date(input$planting_date)))

    # PWL (piecewise) was fit against Kc-adjusted ET; LGG (logistic) against
    # raw ET0 -- see kc_ramp()'s doc comment in R/growth_calcs.R.
    kc_fn <- if (input$model_type == "piecewise") kc_ramp else NULL

    df_wsi <- compute_wsi_daily(
      df_gdd,
      root_depth_cm = ROOT_DEPTH_CM,
      SLLL = soil_agg$SLLL,
      SDUL = soil_agg$SDUL,
      BD   = soil_agg$bulk_density_g_cm3,
      kc_fun = kc_fn
    )

    df_stress <- df_wsi
    if (isTRUE(APPLY_STRESS)) {
      df_stress <- compute_GDD_eff(
        df_stress,
        x_var = "GDD_cum",
        WSI_var = "WSI",
        w1 = input$w1,
        w2 = input$w2,
        f_min = input$fmin,
        use_fw_trapezoid = USE_FW_TRAPZ
      )
    } else {
      df_stress$GDD_eff <- df_stress$GDD_cum
    }

    # Which trial's calibration to apply: in advanced mode, whatever the user
    # explicitly picked; otherwise, matched to whichever trial's real,
    # realized water stress the current scenario's own computed WSI is
    # closest to (see pick_season_by_wsi() in R/predict_model.R).
    mean_wsi <- mean(df_stress$WSI, na.rm = TRUE)
    season_used <- if (isTRUE(input$show_advanced)) input$season else pick_season_by_wsi(mean_wsi)

    row <- catalog %>% filter(Season == season_used, Cultivar == input$cultivar) %>% slice(1)
    if (nrow(row) == 0) stop("No model found for Season/Cultivar.")

    row_boot_draws <- if (!is.null(lgg_boot_draws)) {
      lgg_boot_draws %>% filter(Season == season_used, Cultivar == input$cultivar)
    } else {
      NULL
    }

    pred_df <- predict_growth(
      df_stress, row, input$model_type, APPLY_STRESS,
      input$w1, input$w2, input$fmin, USE_FW_TRAPZ, Y0_STRESSED,
      boot_draws = row_boot_draws
    )
    # DW_roots (t/ha, dry weight) -> an estimated fresh/marketable yield --
    # see R/yield.R for where the conversion factor comes from and its limits.
    pred_df$pred_fw       <- dw_to_fw(pred_df$pred,       input$cultivar)
    pred_df$pred_fw_lower <- dw_to_fw(pred_df$pred_lower, input$cultivar)
    pred_df$pred_fw_upper <- dw_to_fw(pred_df$pred_upper, input$cultivar)
    x_used <- if (input$model_type == "piecewise") "GDD_eff" else "GDD_cum"

    list(
      climate = clim,
      soil_prof = soil_prof,
      soil_agg = soil_agg,
      root_depth = ROOT_DEPTH_CM,
      df = df_stress,
      pred = pred_df,
      DAT = DAT,
      x_used = x_used,
      season_used = season_used,
      mean_wsi = mean_wsi,
      season_auto_selected = !isTRUE(input$show_advanced),
      band_is_bootstrap = identical(input$model_type, "logistic") && !is.null(row_boot_draws) && nrow(row_boot_draws) > 0,
      cultivar = input$cultivar
    )
  })

  # -----------------------
  # Reactive table/plot builders (shared between on-screen output and report)
  # -----------------------
  irrigation_desc_r <- reactive({
    if (!identical(input$irr_simple, "irrigated")) {
      "Rainfed (none)"
    } else if (identical(input$irr_irrigated_type, "dates")) {
      if (!is.null(input$irr_simple_file)) {
        "Irrigated: uploaded schedule (specific dates)"
      } else {
        paste0("Irrigated: ", nrow(irr_manual_rows()), " manually-added date(s)")
      }
    } else {
      paste0("Irrigated: ", input$irr_simple_mm, " mm/week")
    }
  })

  summary_tbl_r <- reactive({
    req(harvest_date_r())
    kv_table(list(
      "Planting date" = as.Date(input$planting_date),
      "Harvest date"  = harvest_date_r(),
      "DAT (days)"    = as.integer(harvest_date_r() - as.Date(input$planting_date)),
      "Cultivar"      = input$cultivar,
      "Irrigation" = irrigation_desc_r()
    ), digits = 0)
  })

  climate_summary_tbl_r <- reactive({
    req(results())
    df <- results()$df
    pred <- results()$pred
    planting <- as.Date(input$planting_date)
    date_90  <- planting + 90
    harvest  <- harvest_date_r()

    kv_table(list(
      "Total precipitation (mm)" = sum(df$precip_mm, na.rm = TRUE),
      "Total irrigation (mm)"    = sum(df$irrigation_mm, na.rm = TRUE),
      "Total ET (mm)"            = sum(df$ET, na.rm = TRUE),
      "GDD (cumulative) at harvest (deg C-days)" = max(df$GDD_cum, na.rm = TRUE),
      "Effective GDD (cumulative) at harvest (deg C-days)"  = max(df$GDD_eff, na.rm = TRUE),
      "Prediction at 90 DAT (t/ha)"     = pick_pred_at_date(pred, date_90),
      "Prediction at harvest (t/ha)"    = pick_pred_at_date(pred, harvest)
    ), digits = 2)
  })

  soil_profile_tbl_r <- reactive({
    req(results())
    results()$soil_prof %>%
      mutate(
        `Depth (cm)` = paste0(depth_top_cm, "-", depth_bot_cm),
        `Wilting point (cm3/cm3)` = round(SLLL, 3),
        `Field capacity (cm3/cm3)`  = round(SDUL, 3),
        `Bulk density (g/cm3)` = round(bulk_density_g_cm3, 2)
      ) %>%
      select(`Depth (cm)`, `Wilting point (cm3/cm3)`, `Field capacity (cm3/cm3)`, `Bulk density (g/cm3)`)
  })

  soil_agg_tbl_r <- reactive({
    req(results())
    s <- results()$soil_agg
    kv_table(list(
      "Soil data source"            = s$soil_source,
      "Soil aggregation depth (cm)" = s$agg_depth_cm,
      "Root depth for WSI (cm)"     = ROOT_DEPTH_CM,
      "Wilting point used (cm3/cm3)" = s$SLLL,
      "Field capacity used (cm3/cm3)" = s$SDUL,
      "Bulk density used (g/cm3)"   = s$bulk_density_g_cm3
    ), digits = 3)
  })

  # x-axis is GDD (what the model is actually fit against -- GDD_eff for
  # piecewise, since that model rescales elapsed time by water stress;
  # GDD_cum for logistic), not calendar date. sec_axis maps it back to DAT
  # via approx() since GDD accumulates at a variable daily rate (not linear
  # in DAT the way Date is) -- fine here since this is a static, non-plotly
  # ggplot render (also reused as-is for the downloadable report); the live
  # interactive version rebuilds the same secondary axis in output$p_pred
  # via add_dat_axis(), since ggplotly() drops a native sec_axis silently.
  pred_plot_r <- reactive({
    req(results())
    x_var <- results()$x_used
    planting <- as.Date(input$planting_date)
    df <- results()$pred %>%
      mutate(Date = as.Date(date), DAT = as.integer(Date - planting), GDD = .data[[x_var]], DW = pred)
    x_lab <- if (x_var == "GDD_eff") "Effective GDD (cumulative)" else "GDD (cumulative)"
    has_band <- !all(is.na(df$pred_lower))
    band_subtitle <- if (!has_band) {
      NULL
    } else if (input$model_type == "logistic") {
      "Shaded band: 50% joint prediction band (interquartile range) from 40 residual-bootstrap refits"
    } else {
      "Shaded band: rough envelope from the breakpoint's own 95% CI, not a joint prediction interval"
    }
    p <- ggplot(df, aes(x = GDD, y = DW))
    if (has_band) {
      p <- p + geom_ribbon(aes(ymin = pred_lower, ymax = pred_upper), alpha = 0.15)
    }
    p +
      geom_line(linewidth = 0.9) +
      scale_x_continuous(
        name = x_lab,
        sec.axis = sec_axis(
          ~ stats::approx(x = df$GDD, y = df$DAT, xout = ., ties = "ordered", rule = 2)$y,
          name = "Days after planting",
          labels = function(v) as.character(round(v))
        )
      ) +
      labs(y = "Root dry weight (t/ha)", title = "Model prediction over time", subtitle = band_subtitle) +
      theme_gray(base_size = 15) +
      theme(
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12),
        plot.subtitle = element_text(size = 11, color = "grey30")
      )
  })

  pred_tbl_r <- reactive({
    req(results())
    planting <- as.Date(input$planting_date)
    results()$pred %>%
      mutate(DAT = as.integer(as.Date(date) - planting)) %>%
      select(date, DAT, pred, pred_lower, pred_upper, pred_fw, pred_fw_lower, pred_fw_upper,
             GDD_cum, GDD_eff, WSI) %>%
      mutate(date = as.Date(date)) %>%
      mutate(date = format(date, "%Y-%m-%d")) %>%
      rename(
        `DW t/ha` = pred, `DW lower` = pred_lower, `DW upper` = pred_upper,
        `FW t/ha (est.)` = pred_fw, `FW lower` = pred_fw_lower, `FW upper` = pred_fw_upper,
        `GDD (cumulative)` = GDD_cum, `Effective GDD (cumulative)` = GDD_eff
      ) %>%
      mutate(across(where(is.numeric), fmt2))
  })

  # -----------------------
  # Environmental conditions tab
  # -----------------------
  output$explain_ui <- renderUI({
    req(results())
    HTML(explain_plain_language(results()))
  })
  output$tbl_climate_summary <- renderTable(climate_summary_tbl_r(), rownames = FALSE)
  output$tbl_soil_profile <- renderTable(soil_profile_tbl_r(), rownames = FALSE)
  output$tbl_soil_agg <- renderTable(soil_agg_tbl_r(), rownames = FALSE)

  # -----------------------
  # Temperature & Water Availability plots/table
  # -----------------------
  output$p_temp <- renderPlotly({
    req(results())
    # Reads the live toggle, not a value captured at Run time, so switching
    # C/F updates the plot immediately -- safe because tmax_c/tmin_c (the
    # raw Celsius NASA POWER pulled) are already fetched and just need a
    # unit conversion for display, no new data/Run required.
    unit_lab <- paste0(DEG, if (input$temp_unit == "F") "F" else "C")
    df <- results()$df %>%
      mutate(
        GDD = GDD_cum,
        Tmax = if (input$temp_unit == "F") c_to_f(tmax_c) else tmax_c,
        Tmin = if (input$temp_unit == "F") c_to_f(tmin_c) else tmin_c
      )
    p <- ggplot(df, aes(x = GDD)) +
      geom_line(aes(y = Tmax, color = "Maximum temperature")) +
      geom_line(aes(y = Tmin, color = "Minimum temperature")) +
      scale_color_manual(name = NULL, values = c("Maximum temperature" = "#c0392b", "Minimum temperature" = "#2980b9")) +
      labs(x = "GDD (cumulative)", y = paste0("Temperature (", unit_lab, ")"), title = "Daily Maximum and Minimum Temperature")
    y_ref <- mean(range(c(df$Tmax, df$Tmin), na.rm = TRUE))
    ggplotly(p) %>%
      layout(legend = list(orientation = "h", y = -0.2)) %>%
      add_dat_axis(x = df$GDD, dat = df$DAT, y_ref = y_ref)
  })

  output$p_water_et <- renderPlotly({
    req(results())
    df <- results()$df %>% mutate(GDD = GDD_cum, `Water input` = water, ET_mm = ET)
    p <- ggplot(df, aes(x = GDD)) +
      geom_col(aes(y = `Water input`, fill = "Water input (rain + irrigation)"), alpha = 0.6) +
      geom_line(aes(y = ET_mm, color = "Evapotranspiration (ET)"), linewidth = 0.8) +
      scale_fill_manual(name = NULL, values = c("Water input (rain + irrigation)" = "#5dade2")) +
      scale_color_manual(name = NULL, values = c("Evapotranspiration (ET)" = "#e67e22")) +
      labs(x = "GDD (cumulative)", y = "mm/day", title = "Daily Water Input and Evapotranspiration")
    y_ref <- mean(range(c(df$`Water input`, df$ET_mm), na.rm = TRUE))
    # ggplotly mangles trace names into "(label,1)" when a plot mixes a fill
    # legend (bars) and a color legend (line) -- clean them back up.
    ggplotly(p) %>%
      clean_plotly_legend() %>%
      layout(legend = list(orientation = "h", y = -0.2)) %>%
      add_dat_axis(x = df$GDD, dat = df$DAT, y_ref = y_ref)
  })

  clim_tbl_r <- reactive({
    req(results())
    results()$df %>%
      select(date, tmax_c, tmin_c, precip_mm, irrigation_mm, et_mm) %>%
      format_dt(temp_unit = input$temp_unit)
  })
  make_table_widget("tbl_clim", clim_tbl_r)

  # -----------------------
  # Growing Degree Days & Water Stress plots/table
  # -----------------------
  output$p_gdd <- renderPlotly({
    req(results())
    df <- results()$df %>% mutate(Date = as.Date(date), GDD = GDD_cum)
    p <- ggplot(df, aes(x = Date, y = GDD, color = "Cumulative GDD")) +
      geom_line() +
      scale_color_manual(name = NULL, values = c("Cumulative GDD" = "#27ae60")) +
      labs(x = NULL, y = "GDD (cumulative)", title = "Cumulative Growing Degree Days (GDD)")
    y_ref <- mean(range(df$GDD, na.rm = TRUE))
    ggplotly(p) %>%
      layout(legend = list(orientation = "h", y = -0.2)) %>%
      add_dat_axis(x = df$Date, dat = df$DAT, y_ref = y_ref)
  })

  output$p_wsi <- renderPlotly({
    req(results())
    df <- results()$df %>% mutate(GDD = GDD_cum)
    p <- ggplot(df, aes(x = GDD)) +
      geom_line(aes(y = WSI, color = "WSI (10-day smoothed)")) +
      geom_hline(aes(yintercept = 0.8, linetype = "0.8 comfort threshold"), color = "grey40") +
      scale_color_manual(name = NULL, values = c("WSI (10-day smoothed)" = "#8e44ad")) +
      scale_linetype_manual(name = NULL, values = c("0.8 comfort threshold" = "dashed")) +
      coord_cartesian(ylim = c(0, 1)) +
      labs(x = "GDD (cumulative)", y = "WSI (0-1, 1 = no stress)", title = "Water Stress Index (WSI)")
    ggplotly(p) %>%
      clean_plotly_legend() %>%
      layout(legend = list(orientation = "h", y = -0.2)) %>%
      add_dat_axis(x = df$GDD, dat = df$DAT, y_ref = 0.5)
  })

  gdd_tbl_r <- reactive({
    req(results())
    results()$df %>%
      select(date, gdd, GDD_cum, GDD_eff, water, ET, WSI) %>%
      mutate(date = as.Date(date)) %>%
      mutate(date = format(date, "%Y-%m-%d")) %>%
      mutate(across(where(is.numeric), fmt2)) %>%
      rename(
        `GDD (daily)` = gdd,
        `GDD (cumulative)` = GDD_cum,
        `Effective GDD (cumulative)` = GDD_eff,
        `Water input (mm)` = water,
        `ET (mm)` = ET
      )
  })
  make_table_widget("tbl_gdd", gdd_tbl_r)

  # -----------------------
  # Prediction
  # -----------------------
  output$eqn_box <- renderUI({
    req(results())
    eqn <- attr(results()$pred, "equation")
    if (is.null(eqn) || is.na(eqn)) eqn <- "(no equation text)"
    tags$div(
      style = "padding:10px; background:#f6f6f6; border-radius:8px; margin-bottom:10px;",
      tags$b("Equation: "),
      tags$code(eqn)
    )
  })

  output$p_pred <- renderPlotly({
    req(results())
    x_var <- results()$x_used
    planting <- as.Date(input$planting_date)
    df <- results()$pred %>%
      mutate(Date = as.Date(date), DAT = as.integer(Date - planting), GDD = .data[[x_var]])
    ggplotly(pred_plot_r(), tooltip = c("x", "y")) %>%
      layout(legend = list(orientation = "h", y = -0.2)) %>%
      add_dat_axis(x = df$GDD, dat = df$DAT, y_ref = mean(range(df$pred, na.rm = TRUE)))
  })

  output$pred_caption_ui <- renderUI({
    req(results())
    res <- results()
    pred <- res$pred
    final_dw <- pred$pred[which.max(pred$date)]
    final_fw <- pred$pred_fw[which.max(pred$date)]
    fw_txt <- if (!is.na(final_fw)) sprintf(" (~%.1f t/ha estimated fresh/marketable weight)", final_fw) else ""
    tags$p(
      style = "font-size:14px; color:#444; margin-top:4px;",
      sprintf(
        "Predicted for %s under this scenario: %.1f t/ha dry weight at harvest%s, using the %s calibration.",
        input$cultivar, final_dw, fw_txt, res$season_used
      )
    )
  })

  tbl_pred_summary_r <- reactive({
    req(results())
    res <- results()
    pred <- res$pred
    final_dw <- pred$pred[which.max(pred$date)]
    final_fw <- pred$pred_fw[which.max(pred$date)]
    lo <- pred$pred_lower[which.max(pred$date)]
    hi <- pred$pred_upper[which.max(pred$date)]
    band_label <- if (isTRUE(res$band_is_bootstrap)) "50% bootstrap range (DW)" else "Rough envelope (DW)"

    rows <- list(
      "Planting date" = as.Date(input$planting_date),
      "Harvest date"  = harvest_date_r(),
      "DAT (days)"    = res$DAT,
      "Irrigation"    = irrigation_desc_r(),
      "Cultivar" = input$cultivar,
      "Growth calibration" = res$season_used,
      "Predicted dry weight at harvest (t/ha)" = final_dw,
      "Estimated fresh/marketable weight at harvest (t/ha)" = final_fw
    )
    if (!is.na(lo) && !is.na(hi)) {
      rows[[band_label]] <- sprintf("%.2f - %.2f", lo, hi)
    }
    kv_table(rows, digits = 2)
  })

  output$tbl_pred_summary <- renderTable(tbl_pred_summary_r(), rownames = FALSE)

  make_table_widget("tbl_pred", pred_tbl_r)

  # -----------------------
  # Downloads
  # -----------------------
  output$dl_csv <- downloadHandler(
    filename = function() paste0("sweetpotato_prediction_", Sys.Date(), ".csv"),
    content = function(file) pred_to_csv(results(), file)
  )

  output$dl_report <- downloadHandler(
    filename = function() paste0("sweetpotato_report_", Sys.Date(), ".html"),
    content = function(file) {
      snap <- list(
        summary_tbl = summary_tbl_r(),
        climate_summary_tbl = climate_summary_tbl_r(),
        soil_profile_tbl = soil_profile_tbl_r(),
        soil_agg_tbl = soil_agg_tbl_r(),
        pred_plot = pred_plot_r(),
        pred_tbl = pred_tbl_r()
      )
      render_report(results(), snap, file)
    }
  )
}

shinyApp(ui, server)
