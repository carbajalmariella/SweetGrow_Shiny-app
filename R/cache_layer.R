# ============================================================
# Disk cache for external API/data calls (NASA POWER, soil sources).
# Same (lat, lon, dates, source) inputs should not re-hit the network
# every time a stakeholder clicks "Run".
# ============================================================
app_cache <- cachem::cache_disk(
  dir = file.path("cache"),
  max_age = 60 * 60 * 24 * 30,   # 30 days: climate/soil history for a fixed window doesn't change
  max_size = 512 * 1024 * 1024
)

cached <- function(f) memoise::memoise(f, cache = app_cache)

# NOTE: memoise keys on function name + arguments, not on the function's
# implementation. If you change what a cached function (get_climate_power_api,
# get_soil_ssurgo_profile, get_soil_soilgrids_profile) computes or returns,
# old results for the same (lat, lon, dates) sitting in cache/ will keep
# being served with the OLD shape/values until that directory is cleared.
# `rm -rf cache/*` after any change to those functions.
