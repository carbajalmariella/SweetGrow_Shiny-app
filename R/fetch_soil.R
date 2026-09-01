# ============================================================
# Soil: SSURGO (US, via soilDB/SDA) and SoilGrids (global, via ISRIC REST)
#
# Both paths resolve to the same shape: a profile data.frame with
#   depth_top_cm, depth_bot_cm, SLLL (wilting point), SDUL (field capacity),
#   bulk_density_g_cm3
# so `aggregate_soil_to_depth()` and everything downstream doesn't care
# which source produced it.
#
# SSURGO only covers the US. SoilGrids is global but its REST endpoint is
# known to be flaky in practice (ISRIC's own service intermittently returns
# null for a query that succeeds a few seconds later on retry) -- so the
# SoilGrids path retries with backoff and fails loudly and clearly rather
# than silently returning nonsense soil values.
# ============================================================

# Rough CONUS + AK + HI bounding boxes, just to pick a sensible default
# source automatically. Not a precise border check -- the user can always
# override the soil source manually in the UI.
is_us_coordinate <- function(lat, lon) {
  conus <- lat >= 24.5 & lat <= 49.5 & lon >= -125 & lon <= -66.5
  alaska <- lat >= 51 & lat <= 71.5 & lon >= -172 & lon <= -129
  hawaii <- lat >= 18.5 & lat <= 22.5 & lon >= -160.5 & lon <= -154.5
  isTRUE(conus | alaska | hawaii)
}

# ============================================================
# SSURGO profile -> per-horizon SLLL/SDUL/BD (unchanged from original)
# ============================================================
get_soil_ssurgo_profile_raw <- function(lat, lon) {
  pt <- sf::st_as_sf(
    data.frame(id = "site1", lon = lon, lat = lat),
    coords = c("lon", "lat"),
    crs = 4326
  )

  muk <- soilDB::SDA_spatialQuery(pt, what = "mukey")
  if (is.null(muk) || nrow(muk) == 0) stop("Could not get SSURGO mukey from SDA.")
  mukey <- muk$mukey[1]

  sql <- glue("
    SELECT
      mu.mukey,
      mu.muname,
      c.cokey,
      c.compname,
      c.comppct_r,
      hz.hzname,
      hz.hzdept_r,
      hz.hzdepb_r,
      hz.wthirdbar_r,
      hz.wfifteenbar_r,
      hz.dbovendry_r
    FROM mapunit mu
    INNER JOIN component c ON c.mukey = mu.mukey
    INNER JOIN chorizon hz ON hz.cokey = c.cokey
    WHERE mu.mukey = '{mukey}'
      AND c.majcompflag = 'Yes'
    ORDER BY c.comppct_r DESC, hz.hzdept_r ASC
  ")

  hz <- soilDB::SDA_query(sql)
  if (is.null(hz) || nrow(hz) == 0) stop("SDA_query returned no horizons.")

  dom_cokey <- hz$cokey[which.max(hz$comppct_r)]
  hz <- hz %>% filter(cokey == dom_cokey)

  hz %>%
    mutate(
      depth_top_cm = as.numeric(hzdept_r),
      depth_bot_cm = as.numeric(hzdepb_r),
      SDUL = as.numeric(wthirdbar_r) / 100,
      SLLL = as.numeric(wfifteenbar_r) / 100,
      bulk_density_g_cm3 = as.numeric(dbovendry_r)
    ) %>%
    select(
      mukey, muname, compname, comppct_r,
      hzname, depth_top_cm, depth_bot_cm,
      SLLL, SDUL, bulk_density_g_cm3
    ) %>%
    mutate(soil_source = "SSURGO (dominant component)")
}

get_soil_ssurgo_profile <- cached(get_soil_ssurgo_profile_raw)

# ============================================================
# SoilGrids profile via ISRIC REST API
#
# Requests texture (clay/sand/silt) + organic carbon + bulk density --
# SoilGrids' pre-computed static properties -- rather than its wv0033/
# wv1500 "water content at 33/1500 kPa" layers, which are themselves
# derived on the fly by ISRIC's own (undocumented) pedotransfer function
# and were, in testing, the least reliable thing behind this endpoint.
# SLLL/SDUL are then computed locally via Rawls et al. (1982)
# (R/pedotransfer.R) -- an explicit, citable, testable approximation
# instead of an opaque one.
# ============================================================
.SG_DEPTHS <- c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm")

.soilgrids_query_once <- function(lat, lon, timeout_s = 25) {
  url <- paste0(
    "https://rest.isric.org/soilgrids/v2.0/properties/query?",
    "lon=", lon, "&lat=", lat,
    "&property=clay&property=sand&property=silt&property=soc&property=bdod&",
    paste0("depth=", .SG_DEPTHS, collapse = "&"),
    "&value=mean"
  )

  h <- curl::new_handle()
  curl::handle_setheaders(h, "User-Agent" = "R-curl")
  curl::handle_setopt(h, timeout = timeout_s, connecttimeout = 15)

  res <- curl::curl_fetch_memory(url, handle = h)
  if (res$status_code != 200) stop("SoilGrids HTTP status: ", res$status_code)

  js <- jsonlite::fromJSON(rawToChar(res$content))
  layers <- js$properties$layers
  if (is.null(layers) || nrow(layers) == 0) stop("SoilGrids returned no layers.")

  out <- purrr::map_dfr(seq_len(nrow(layers)), function(i) {
    name <- layers$name[i]
    d_factor <- layers$unit_measure$d_factor[i]
    depths <- layers$depths[[i]]
    tibble(
      name = name,
      depth_top_cm = depths$range$top_depth,
      depth_bot_cm = depths$range$bottom_depth,
      raw = depths$values$mean,
      d_factor = d_factor
    )
  })

  # clay/sand/silt target units are "%" directly; soc target units are
  # "g/kg" directly; bdod target units are kg/dm3 == g/cm3 directly --
  # none of these need the extra *0.01 the old wv0033/wv1500 path did.
  out %>% mutate(value = raw / d_factor)
}

# All-NA (or empty) means the request "succeeded" (HTTP 200) but ISRIC had
# no usable prediction to hand back for this point/instant -- that's the
# flaky-service case a retry should recover from.
.soilgrids_all_na <- function(df) {
  is.null(df) || nrow(df) == 0 || all(is.na(df$value))
}

get_soil_soilgrids_profile_raw <- function(lat, lon, n_retries = 3, timeout_s = 25) {
  attempt <- 0
  last_err <- NULL
  repeat {
    attempt <- attempt + 1
    long <- tryCatch(.soilgrids_query_once(lat, lon, timeout_s), error = function(e) e)

    if (!inherits(long, "error") && !.soilgrids_all_na(long)) break
    last_err <- if (inherits(long, "error")) conditionMessage(long) else "SoilGrids returned no data for this location"

    if (attempt > n_retries) {
      stop(
        "SoilGrids (ISRIC) did not return usable soil data after ", attempt,
        " attempts for lat=", round(lat, 4), ", lon=", round(lon, 4),
        ". This is usually a transient outage on ISRIC's side -- try again in a minute, ",
        "nudge the location slightly, or switch soil source to SSURGO if the site is in the US. ",
        "(", last_err, ")"
      )
    }
    Sys.sleep(3 * attempt)
  }

  wide <- long %>%
    select(name, depth_top_cm, depth_bot_cm, value) %>%
    tidyr::pivot_wider(names_from = name, values_from = value) %>%
    arrange(depth_top_cm)

  if (!all(c("clay", "sand", "silt", "soc", "bdod") %in% names(wide))) {
    stop("SoilGrids response missing expected properties (clay/sand/silt/soc/bdod).")
  }

  wide %>%
    rowwise() %>%
    mutate(theta = list(soil_water_from_texture(sand, silt, clay, soc / 10))) %>%
    ungroup() %>%
    transmute(
      depth_top_cm, depth_bot_cm,
      SDUL = purrr::map_dbl(theta, "SDUL"),
      SLLL = purrr::map_dbl(theta, "SLLL"),
      bulk_density_g_cm3 = bdod,
      soil_source = "SoilGrids v2.0 (ISRIC) + Rawls (1982) PTF"
    ) %>%
    filter(!is.na(SLLL), !is.na(SDUL), !is.na(bulk_density_g_cm3))
}

get_soil_soilgrids_profile <- cached(get_soil_soilgrids_profile_raw)

# ============================================================
# Dispatcher: soil_source = "auto" | "SSURGO" | "SoilGrids"
# "auto" picks SSURGO inside the US (reliable, higher resolution) and
# SoilGrids elsewhere; if SSURGO errors (point not actually covered) it
# falls back to SoilGrids automatically.
# ============================================================
get_soil_profile <- function(lat, lon, soil_source = "auto") {
  use_ssurgo <- switch(soil_source,
    "SSURGO" = TRUE,
    "SoilGrids" = FALSE,
    is_us_coordinate(lat, lon)
  )

  if (use_ssurgo) {
    out <- tryCatch(get_soil_ssurgo_profile(lat, lon), error = function(e) e)
    if (!inherits(out, "error")) return(out)
    if (identical(soil_source, "SSURGO")) stop(out)
    message("SSURGO lookup failed (", conditionMessage(out), "); falling back to SoilGrids.")
  }

  get_soil_soilgrids_profile(lat, lon)
}

aggregate_soil_to_depth <- function(soil_df, target_depth_cm = 30) {
  soil_sub <- soil_df %>%
    mutate(
      overlap_top = pmax(depth_top_cm, 0),
      overlap_bot = pmin(depth_bot_cm, target_depth_cm),
      thickness = pmax(0, overlap_bot - overlap_top)
    ) %>%
    filter(thickness > 0)

  if (nrow(soil_sub) == 0) stop("No horizons overlap the selected aggregation depth.")

  tibble(
    agg_depth_cm = target_depth_cm,
    soil_source = soil_sub$soil_source[1],
    SLLL = sum(soil_sub$SLLL * soil_sub$thickness, na.rm = TRUE) / sum(soil_sub$thickness),
    SDUL = sum(soil_sub$SDUL * soil_sub$thickness, na.rm = TRUE) / sum(soil_sub$thickness),
    bulk_density_g_cm3 =
      sum(soil_sub$bulk_density_g_cm3 * soil_sub$thickness, na.rm = TRUE) / sum(soil_sub$thickness)
  )
}
