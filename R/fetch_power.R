# ============================================================
# NASA POWER (direct API)
# ============================================================
power_json_to_df <- function(js) {
  param_list <- js$properties$parameter
  pars <- names(param_list)

  long <- purrr::map_dfr(pars, function(p) {
    v <- param_list[[p]]
    tibble(date = names(v), parameter = p, value = as.numeric(unname(v)))
  })

  long %>%
    mutate(date = as.Date(date, format = "%Y%m%d")) %>%
    pivot_wider(names_from = parameter, values_from = value) %>%
    arrange(date) %>%
    rename(
      tmax_c = T2M_MAX,
      tmin_c = T2M_MIN,
      precip_mm = PRECTOTCORR,
      rh_pct = RH2M,
      wind2m_ms = WS2M,
      rs_mj = ALLSKY_SFC_SW_DWN
    )
}

# Flags POWER's -999 fill value as NA across the raw fields, then trims any
# trailing run of missing days once real data has started (POWER sometimes
# hasn't backfilled the last few days near "now" yet).
clean_power_daily_strict <- function(df) {
  raw_cols <- c("tmax_c", "tmin_c", "precip_mm", "rh_pct", "wind2m_ms", "rs_mj")
  df2 <- df %>%
    mutate(across(all_of(raw_cols), ~ ifelse(.x <= -900, NA_real_, .x))) %>%
    arrange(date)

  ok <- !is.na(df2$tmax_c)
  first_ok <- which(ok)[1]
  if (is.na(first_ok)) return(df2[0, ])

  ok_from_first <- ok[first_ok:length(ok)]
  first_na_after <- which(!ok_from_first)[1]
  if (!is.na(first_na_after)) {
    cut_idx <- first_ok + first_na_after - 2
    df2 <- df2[1:cut_idx, ]
  }
  df2
}

# NASA POWER has no reference-evapotranspiration parameter (see
# R/evapotranspiration.R's doc comment for why EVPTRNS isn't it) -- ET0 is
# computed from the raw meteorological fields via FAO-56 Penman-Monteith.
add_et0 <- function(df, lat, elev_m) {
  df %>%
    mutate(
      doy = as.integer(format(date, "%j")),
      et_mm = compute_et0_pm(tmax_c, tmin_c, rh_pct, wind2m_ms, rs_mj,
                              lat_deg = lat, elev_m = elev_m, doy = doy)
    ) %>%
    select(-doy)
}

# One HTTP attempt against the POWER API. Raises on non-200 or network error.
.get_climate_power_api_once <- function(lat, lon, start_date, end_date,
                                         lag_days_power = 7,
                                         community = "AG",
                                         parameters = c("T2M_MAX", "T2M_MIN", "PRECTOTCORR",
                                                        "RH2M", "WS2M", "ALLSKY_SFC_SW_DWN"),
                                         timeout_s = 30) {
  end_safe <- min(as.Date(end_date), Sys.Date() - lag_days_power)

  url <- paste0(
    "https://power.larc.nasa.gov/api/temporal/daily/point?",
    "parameters=", paste(parameters, collapse = ","),
    "&community=", community,
    "&longitude=", lon,
    "&latitude=", lat,
    "&start=", fmt_yyyymmdd(start_date),
    "&end=", fmt_yyyymmdd(end_safe),
    "&format=JSON"
  )

  h <- curl::new_handle()
  curl::handle_setheaders(h, "User-Agent" = "R-curl")
  curl::handle_setopt(h, timeout = timeout_s, connecttimeout = 15)

  res <- curl::curl_fetch_memory(url, handle = h)
  if (res$status_code != 200) stop("POWER HTTP status: ", res$status_code)

  js <- jsonlite::fromJSON(rawToChar(res$content))
  elev_m <- js$geometry$coordinates[[3]]
  if (is.null(elev_m) || is.na(elev_m)) elev_m <- 0

  power_json_to_df(js) %>%
    clean_power_daily_strict() %>%
    add_et0(lat = lat, elev_m = elev_m)
}

# Retries a couple of times with backoff: POWER occasionally times out or
# hiccups under load, and a stakeholder shouldn't see the whole run fail
# because of one dropped request.
get_climate_power_api_raw <- function(lat, lon, start_date, end_date,
                                       lag_days_power = 7,
                                       community = "AG",
                                       parameters = c("T2M_MAX", "T2M_MIN", "PRECTOTCORR",
                                                      "RH2M", "WS2M", "ALLSKY_SFC_SW_DWN"),
                                       timeout_s = 30,
                                       n_retries = 2) {
  attempt <- 0
  repeat {
    attempt <- attempt + 1
    out <- tryCatch(
      .get_climate_power_api_once(lat, lon, start_date, end_date, lag_days_power,
                                   community, parameters, timeout_s),
      error = function(e) e
    )
    if (!inherits(out, "error")) return(out)
    if (attempt > n_retries) {
      stop("NASA POWER request failed after ", attempt, " attempts: ", conditionMessage(out))
    }
    Sys.sleep(2 * attempt)
  }
}

# Cached wrapper: identical (lat, lon, dates, lag) inputs hit disk, not the network.
get_climate_power_api <- cached(get_climate_power_api_raw)
