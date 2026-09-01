# ============================================================
# FAO-56 Penman-Monteith daily reference evapotranspiration (ET0, mm/day).
#
# NASA POWER has no reference-ET parameter: EVPTRNS is an "Evapotranspiration
# Energy Flux" in MJ/m^2/day (a MERRA-2 land-surface actual-ET estimate, not
# even the same physical quantity as a reference ET0), and EVLAND is bare/land
# evaporation, not crop-reference ET. Using EVPTRNS's raw numeric value as if
# it were mm/day (this app's original approach) mixed up a water depth with
# an energy flux entirely. The standard, textbook-correct way to get ET0 from
# POWER's raw meteorological fields (Tmax, Tmin, RH2M, WS2M,
# ALLSKY_SFC_SW_DWN) is to compute FAO-56 Penman-Monteith directly -- Allen
# et al. (1998), FAO Irrigation and Drainage Paper 56, chapters 3-4.
#
# Validated against FAO-56's own worked example (Example 18, Brussels,
# Belgium, DOY 187): tmax=21.5, tmin=12.3, rh_mean=73.5, wind2m=2.078,
# rs=22.07, lat=50.80, elev=100 -> ET0 = 3.9 mm/day (see tests).
# ============================================================
compute_et0_pm <- function(tmax_c, tmin_c, rh_mean_pct, wind2m_ms, rs_mj, lat_deg, elev_m, doy) {
  Tmean <- (tmax_c + tmin_c) / 2

  # saturation & actual vapor pressure (kPa)
  e0 <- function(T) 0.6108 * exp(17.27 * T / (T + 237.3))
  es_tmax <- e0(tmax_c)
  es_tmin <- e0(tmin_c)
  es <- (es_tmax + es_tmin) / 2
  ea <- es * (rh_mean_pct / 100)

  # slope of the saturation vapor pressure curve (kPa/degC)
  delta <- 4098 * e0(Tmean) / (Tmean + 237.3)^2

  # psychrometric constant (kPa/degC)
  P <- 101.3 * ((293 - 0.0065 * elev_m) / 293)^5.26
  gamma <- 0.665e-3 * P

  # extraterrestrial radiation Ra (MJ/m^2/day)
  lat_rad <- lat_deg * pi / 180
  dr <- 1 + 0.033 * cos(2 * pi * doy / 365)
  sol_decl <- 0.409 * sin(2 * pi * doy / 365 - 1.39)
  ws <- acos(pmin(1, pmax(-1, -tan(lat_rad) * tan(sol_decl))))
  Ra <- (24 * 60 / pi) * 0.0820 * dr *
    (ws * sin(lat_rad) * sin(sol_decl) + cos(lat_rad) * cos(sol_decl) * sin(ws))

  # clear-sky radiation and net radiation (MJ/m^2/day)
  Rso <- (0.75 + 2e-5 * elev_m) * Ra
  albedo <- 0.23
  Rns <- (1 - albedo) * rs_mj

  sigma <- 4.903e-9
  Tmax_K <- tmax_c + 273.16
  Tmin_K <- tmin_c + 273.16
  rs_rso <- pmin(1, pmax(0.3, rs_mj / Rso))
  Rnl <- sigma * ((Tmax_K^4 + Tmin_K^4) / 2) * (0.34 - 0.14 * sqrt(pmax(0, ea))) * (1.35 * rs_rso - 0.35)

  Rn <- Rns - Rnl
  G <- 0  # soil heat flux, negligible for daily time steps (FAO-56 eq 42)

  et0 <- (0.408 * delta * (Rn - G) + gamma * (900 / (Tmean + 273)) * wind2m_ms * (es - ea)) /
    (delta + gamma * (1 + 0.34 * wind2m_ms))

  pmax(0, et0)
}
