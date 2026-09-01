# ============================================================
# Rawls et al. (1982) pedotransfer function: estimates volumetric soil
# water content at a given capillary pressure from texture + organic
# matter, fit on the US Cooperative Soil Survey Database (n=5320).
# Coefficients as tabulated in Guber & Pachepsky (2010), USDA-ARS
# "Multimodeling with Pedotransfer Functions" manual, Table 4:
#   theta = a + b*sand_pct + c*silt_pct + d*clay_pct + e*OM_pct
# SoilGrids gives texture + bulk density directly (static, reliable COGs)
# but not water retention -- this is the standard way to bridge that gap
# without depending on ISRIC's own on-the-fly (and less reliable) wv0033/
# wv1500 REST layers. Declared explicitly as an approximation, not hidden:
# surfaced in the app as "SoilGrids v2.0 + Rawls PTF".
# ============================================================
.RAWLS_1982 <- list(
  "330"   = c(a = 0.2576, b = -0.0020, c = 0,      d = 0.0036, e = 0.0299),  # ~33 kPa, field capacity
  "15000" = c(a = 0.0260, b = 0,       c = 0,      d = 0.0050, e = 0.0158)   # ~1500 kPa, wilting point
)

# van Bemmelen factor: organic matter % = organic carbon % * 1.724
oc_pct_to_om_pct <- function(oc_pct) oc_pct * 1.724

rawls1982_theta <- function(sand_pct, silt_pct, clay_pct, om_pct, h_cm) {
  co <- .RAWLS_1982[[as.character(h_cm)]]
  if (is.null(co)) stop("rawls1982_theta: no coefficients tabulated for h_cm = ", h_cm)
  co[["a"]] + co[["b"]] * sand_pct + co[["c"]] * silt_pct + co[["d"]] * clay_pct + co[["e"]] * om_pct
}

# Field capacity (SDUL, ~330 cm / 33 kPa) and wilting point (SLLL, ~15000
# cm / 1500 kPa) from sand/silt/clay % and organic carbon %, clamped to a
# physically sane range.
soil_water_from_texture <- function(sand_pct, silt_pct, clay_pct, oc_pct) {
  om_pct <- oc_pct_to_om_pct(oc_pct)
  sdul <- rawls1982_theta(sand_pct, silt_pct, clay_pct, om_pct, "330")
  slll <- rawls1982_theta(sand_pct, silt_pct, clay_pct, om_pct, "15000")
  list(
    SDUL = pmin(0.6, pmax(0.01, sdul)),
    SLLL = pmin(0.6, pmax(0.01, slll))
  )
}
