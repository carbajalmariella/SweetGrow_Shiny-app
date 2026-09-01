# ============================================================
# Small formatting / table helpers shared across modules
# ============================================================

fmt_yyyymmdd <- function(x) format(as.Date(x), "%Y%m%d")

f_to_c <- function(x) (x - 32) * 5/9
c_to_f <- function(x) x * 9/5 + 32

fmt2 <- function(x) {
  if (is.numeric(x)) round(x, 2) else x
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

# Expects raw Celsius tmax_c/tmin_c (not a pre-converted column) so the
# unit conversion always reflects whatever temp_unit is passed NOW, not
# whatever was in effect when the underlying weather data was last fetched.
format_dt <- function(df, temp_unit = "C") {
  unit_lab <- if (identical(temp_unit, "F")) "F" else "C"
  conv <- if (identical(temp_unit, "F")) c_to_f else identity
  df %>%
    mutate(date = as.Date(date)) %>%
    mutate(date = format(date, "%Y-%m-%d")) %>%
    mutate(tmax_c = conv(tmax_c), tmin_c = conv(tmin_c)) %>%
    rename(
      !!paste0("tmax_", unit_lab) := tmax_c,
      !!paste0("tmin_", unit_lab) := tmin_c,
      precip = precip_mm,
      irrigation = irrigation_mm,
      et = et_mm
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
