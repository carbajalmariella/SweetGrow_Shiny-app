# ============================================================
# GDD2 (Celsius internally)
# ============================================================
GDD2_C <- function(tmax_c, tmin_c) {
  t_base <- 16.9
  t_ceiling <- 29.2
  if (is.na(tmax_c) || is.na(tmin_c)) return(NA_real_)

  if (tmax_c > t_ceiling) {
    Tmax <- t_ceiling
    if (tmin_c < t_base) 0 else (Tmax - t_base)
  } else {
    if (tmin_c < t_base) 0 else (tmax_c - t_base)
  }
}

compute_gdd2 <- function(df, temp_unit = c("C", "F")) {
  temp_unit <- match.arg(temp_unit)

  df %>%
    mutate(
      tmax_c_int = if (temp_unit == "F") f_to_c(tmax_in) else tmax_in,
      tmin_c_int = if (temp_unit == "F") f_to_c(tmin_in) else tmin_in,
      gdd = purrr::map2_dbl(tmax_c_int, tmin_c_int, GDD2_C),
      gdd_cum = cumsum(replace_na(gdd, 0)),
      GDD_cum = gdd_cum
    )
}

# ============================================================
# Irrigation
# ============================================================
read_irrigation_csv <- function(file_path) {
  irr <- read.csv(file_path, stringsAsFactors = FALSE)
  if (!all(c("date", "irrigation_mm") %in% names(irr))) {
    stop("Irrigation file must contain columns: date, irrigation_mm")
  }
  irr %>%
    mutate(date = as.Date(date)) %>%
    select(date, irrigation_mm)
}

make_irrigation_series <- function(dates, mode = c("none", "constant", "file"),
                                    constant_mm = 0, file_df = NULL) {
  mode <- match.arg(mode)
  out <- tibble(date = as.Date(dates))

  if (mode == "none") {
    out$irrigation_mm <- 0
    return(out)
  }
  if (mode == "constant") {
    out$irrigation_mm <- constant_mm
    return(out)
  }
  if (mode == "file") {
    if (is.null(file_df)) stop("file_df is NULL in irrigation mode 'file'")
    out %>%
      left_join(file_df, by = "date") %>%
      mutate(irrigation_mm = replace_na(irrigation_mm, 0))
  } else out
}

make_irrigation_schedule <- function(dates, planting_date, every_n = 7, amount_mm = 10, offset_days = 0) {
  dates <- as.Date(dates)
  planting_date <- as.Date(planting_date)

  if (every_n < 1) every_n <- 1
  if (offset_days < 0) offset_days <- 0

  day_idx <- as.integer(dates - planting_date)
  is_event <- (day_idx >= offset_days) & ((day_idx - offset_days) %% every_n == 0)

  tibble::tibble(
    date = dates,
    irrigation_mm = ifelse(is_event, amount_mm, 0)
  )
}

# ============================================================
# FAO-56-style crop coefficient ramp (Kc_ini -> Kc_mid -> Kc_end), fraction
# of season elapsed (0-1) in. Ported verbatim from weather_preprocess.R in
# the manuscript repo -- lets ET reflect actual crop water use instead of
# raw reference ET0, which overstates early-season demand. PWL was fit with
# this Kc adjustment; LGG was fit with raw ET0 (kc_fun = NULL) -- see
# predict_growth()'s dispatch.
#
# Kc_ini/mid/end = 0.47/0.97/0.44: time-averaged values measured via eddy
# covariance on irrigated sweetpotato (Ipomoea batatas) in a semi-arid
# climate -- Mulovhedzi et al. (2020), Agricultural Water Management
# 223:106099. Stage-length fractions are generic row-crop timing, not from
# that source.
kc_ramp <- function(frac, kc_ini = 0.47, kc_mid = 0.97, kc_end = 0.44,
                     ini_end = 0.15, mid_start = 0.15, mid_end = 0.70, late_end = 1.00) {
  dplyr::case_when(
    frac <= ini_end ~ kc_ini,
    frac <= mid_end ~ kc_ini + (kc_mid - kc_ini) * (frac - mid_start) / (mid_end - mid_start),
    frac <= 0.85     ~ kc_mid,
    TRUE             ~ kc_mid + (kc_end - kc_mid) * (frac - 0.85) / (late_end - 0.85)
  )
}

# ============================================================
# WSI (water stress index, 0-1, 1 = no stress)
#
# Day-by-day tipping-bucket water balance, ported from the manuscript
# repo's weather_preprocess.R::compute_wsi_daily() -- NOT the naive
# cumsum+rollmean version this file used to have. That version had three
# bugs the manuscript repo already found and fixed: (1) AW_max was ~10x too
# small because it never converted the SLLL/SDUL*root_depth*BD depth from
# cm to mm, so the "bucket" saturated or emptied on almost every rain event
# instead of buffering water over weeks; (2) a single running cumsum (fixed
# up only after the fact) doesn't behave the same as re-deriving the
# capped storage day-by-day whenever the raw balance would dip below 0 or
# exceed AW_max mid-season, which it does; (3) subtracting SLLL a second
# time in Theta_rel double-counted the offset and capped WSI below 1 even
# when the bucket was completely full. Needs df$DAT (integer days since
# planting, 0-indexed) whenever kc_fun is supplied.
# ============================================================
compute_wsi_daily <- function(df, root_depth_cm, SLLL, SDUL, BD, kc_fun = NULL) {
  denom <- (SDUL - SLLL)
  if (is.na(denom) || denom <= 0) stop("SDUL must be > SLLL to compute WSI.")
  if (is.na(root_depth_cm) || root_depth_cm <= 0) stop("root_depth_cm must be > 0.")
  if (is.na(BD) || BD <= 0) stop("Bulk density must be > 0.")

  # depth = theta_gravimetric * BD * Zr / rho_water (rho_water = 1 g/cm3 dropped);
  # *10 converts that depth from cm to mm so AW_max is comparable to water/ET (mm).
  AW_max <- (SDUL - SLLL) * root_depth_cm * BD * 10

  ETc <- if (is.null(kc_fun)) df$ET else df$ET * kc_fun(df$DAT / max(df$DAT, na.rm = TRUE))

  net <- df$water - ETc
  storage <- numeric(length(net))
  storage[1] <- min(AW_max, max(0, net[1]))
  for (t in seq_along(net)[-1]) {
    storage[t] <- min(AW_max, max(0, storage[t - 1] + net[t]))
  }

  df %>%
    mutate(
      water_balance_raw = storage,
      water_balance_smooth = storage,
      Available_water = storage,
      Theta_t = Available_water / (root_depth_cm * BD * 10),
      # Theta_t already IS the relative quantity (storage is "available water
      # above wilting point"), ranging 0..(SDUL-SLLL) -- dividing by
      # (SDUL-SLLL) is the correct normalization to [0, 1], no SLLL subtraction.
      Theta_rel = Theta_t / (SDUL - SLLL),
      Theta_rel = pmin(1, pmax(0, Theta_rel)),
      WSI_daily = case_when(
        is.na(Theta_rel) ~ 1,
        Theta_rel >= 1 ~ 1,
        TRUE ~ Theta_rel
      ),
      WSI_daily = pmin(1, pmax(0, WSI_daily)),
      WSI_20d = slider::slide_dbl(WSI_daily, .f = ~ mean(.x, na.rm = TRUE),
                                   .before = 10 - 1L, .complete = FALSE),
      WSI_20d = if_else(is.na(WSI_20d), WSI_daily, WSI_20d),
      WSI = WSI_20d
    )
}

# ============================================================
# Stress functions
# ============================================================
fw_trap <- function(WSI, w1, w2, f_min) {
  WSI <- pmin(1, pmax(0, WSI))
  fw <- ifelse(
    WSI <= w1, f_min,
    ifelse(
      WSI >= w2, 1,
      f_min + (1 - f_min) * (WSI - w1) / (w2 - w1)
    )
  )
  pmin(1, pmax(f_min, fw))
}

compute_GDD_eff <- function(df, x_var, WSI_var, w1, w2, f_min, use_fw_trapezoid = FALSE) {
  df <- df[order(df[[x_var]]), , drop = FALSE]

  dd <- df %>%
    dplyr::group_by(.data[[x_var]]) %>%
    dplyr::summarise(
      WSI = mean(.data[[WSI_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.data[[x_var]])

  x <- dd[[x_var]]
  w <- dd$WSI

  dx <- c(0, diff(x))
  dx[!is.finite(dx) | dx < 0] <- 0

  fw <- fw_trap(w, w1 = w1, w2 = w2, f_min = f_min)

  fw_use <- if (use_fw_trapezoid) {
    fw_prev <- c(fw[1], fw[-length(fw)])
    0.5 * (fw_prev + fw)
  } else fw

  dd$GDD_eff <- cumsum(dx * fw_use)

  out <- df
  out$x_join <- out[[x_var]]
  dd$x_join  <- dd[[x_var]]
  out <- dplyr::left_join(out, dd[, c("x_join", "GDD_eff")], by = "x_join")
  out$x_join <- NULL
  out
}

make_stressed_pred <- function(df_daily, x_col, wsi_col, deriv_fun, w1, w2, f_min, y0 = 0.05) {
  dd <- df_daily[stats::complete.cases(df_daily[, c(x_col, wsi_col)]), , drop = FALSE]
  dd <- dd[order(dd[[x_col]]), , drop = FALSE]

  dd <- dd %>%
    dplyr::group_by(.data[[x_col]]) %>%
    dplyr::summarise(WSI = mean(.data[[wsi_col]], na.rm = TRUE), .groups = "drop")

  x_daily <- dd[[x_col]]
  dx <- c(0, diff(x_daily))
  dx[!is.finite(dx) | dx < 0] <- 0

  fw <- fw_trap(dd$WSI, w1 = w1, w2 = w2, f_min = f_min)

  function(x_out, a, b, c) {
    dW_pot <- deriv_fun(x_daily, a, b, c) * dx
    dW_pot[!is.finite(dW_pot)] <- 0
    dW_pot[dW_pot < 0] <- 0

    W_act <- y0 + cumsum(dW_pot * fw)
    stats::approx(x = x_daily, y = W_act, xout = x_out, rule = 2)$y
  }
}
