# ============================================================
# Small formatting / table helpers shared across modules
# ============================================================

fmt_yyyymmdd <- function(x) format(as.Date(x), "%Y%m%d")

f_to_c <- function(x) (x - 32) * 5/9
c_to_f <- function(x) x * 9/5 + 32

t_ha_to_lb_ac <- function(x) x * 892.18
# 50-lb bushel: the sweetpotato industry's actual yield convention in the
# southeastern US (Villordon et al. 2009, HortTechnology).
t_ha_to_bu_ac <- function(x) t_ha_to_lb_ac(x) / 50
mm_to_in <- function(x) x / 25.4

fmt2 <- function(x) {
  if (is.numeric(x)) round(x, 2) else x
}

# Scales decimal places to the value's magnitude so a converted unit (e.g.
# lbs/acre, ~892x t/ha) doesn't inherit a fixed-decimal format that implies
# false precision.
format_precision <- function(x) {
  if (is.na(x)) return(NA)
  if (abs(x) > 100) round(x, 0)
  else if (abs(x) > 10) round(x, 1)
  else round(x, 2)
}

format_table_dates <- function(df) {
  df %>%
    mutate(date = as.Date(date)) %>%
    mutate(date = format(date, "%Y-%m-%d"))
}

kv_table <- function(named_list, digits = 2) {
  Metric <- names(named_list)
  Value  <- unname(named_list)

  Value_chr <- vapply(Value, function(v) {
    if (inherits(v, "Date")) return(format(as.Date(v), "%Y-%m-%d"))
    if (is.logical(v)) return(ifelse(v, "Yes", "No"))
    if (is.numeric(v)) return(as.character(round(v, digits)))
    if (length(v) == 0) return(NA_character_)
    if (length(v) == 1) return(as.character(v))
    paste(as.character(v), collapse = ", ")
  }, character(1))

  data.frame(Metric = Metric, Value = Value_chr, stringsAsFactors = FALSE)
}

# Expects raw Celsius tmax_c/tmin_c and raw-mm precip/irrigation/et (not
# pre-converted columns) so the unit conversion always reflects whatever
# temp_unit/water_unit is passed NOW, not whatever was in effect when the
# underlying weather data was last fetched.
format_dt <- function(df, temp_unit = "C", water_unit = "mm") {
  unit_lab <- if (identical(temp_unit, "F")) "F" else "C"
  conv <- if (identical(temp_unit, "F")) c_to_f else identity
  w_lab <- if (identical(water_unit, "in")) "in" else "mm"
  w_conv <- if (identical(water_unit, "in")) mm_to_in else identity
  df %>%
    mutate(date = as.Date(date)) %>%
    mutate(date = format(date, "%Y-%m-%d")) %>%
    mutate(
      tmax_c = conv(tmax_c), tmin_c = conv(tmin_c),
      precip_mm = w_conv(precip_mm), irrigation_mm = w_conv(irrigation_mm), et_mm = w_conv(et_mm)
    ) %>%
    rename(
      !!paste0("tmax_", unit_lab) := tmax_c,
      !!paste0("tmin_", unit_lab) := tmin_c,
      !!paste0("precip_", w_lab) := precip_mm,
      !!paste0("irrigation_", w_lab) := irrigation_mm,
      !!paste0("et_", w_lab) := et_mm
    ) %>%
    mutate(across(where(is.numeric), fmt2))
}

pick_pred_at_date <- function(df, target_date) {
  target_date <- as.Date(target_date)
  df <- df %>% mutate(date = as.Date(date))
  idx <- which.min(abs(df$date - target_date))
  if (length(idx) == 0) return(NA_real_)
  df$pred[idx]
}

dt_opts <- list(
  dom = "tip",
  paging = TRUE,
  pageLength = 25,
  lengthChange = FALSE,
  searching = FALSE,
  ordering = TRUE,
  info = TRUE,
  scrollX = TRUE
)
