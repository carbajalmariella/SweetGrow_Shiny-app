# ============================================================
# Dry weight (DW_roots, t/ha) -> estimated fresh/marketable weight.
#
# The fitted models predict DW_roots (storage-root DRY weight, t/ha) --
# see sigmoid_models_final.Rmd's intro: "storage-root dry weight (DW_roots,
# t ha^-1^)". That's not what a grower harvests or sells; fresh weight is.
#
# Dry-matter fractions (DW/FW), by cultivar, in order of preference:
#
# - Covington: 0.198 -- published, multi-year NC field trials (the SAME
#   growing region this manuscript's own trials are in): Yencho et al.
#   (2008) 'Covington' Sweetpotato, HortScience 43(6):1911-1914, reporting
#   19.7% (freshly harvested, averaged 2001-2006 NC yield trials) and 20.0%
#   (cured roots). This SUPERSEDES an earlier in-app estimate of 0.391
#   derived from this manuscript's own raw FW_below/DW_below plant samples
#   (Growth_data_all.xlsx, DAT >= 90, n=12) -- that number was roughly 2x
#   the published figure, and the per-sample ratios it was averaged from
#   ranged implausibly from 0.19 to 0.60 for the same cultivar/site/date,
#   suggesting a measurement or protocol issue in that raw sampling rather
#   than a real dry-matter difference. Kept as a cautionary note, not used.
#
# - Bayou Belle: 0.230 -- published: LaBonte, Villordon, Smith & Clark, US
#   Plant Patent PP23,785 P3 "Sweetpotato Plant Named '07-146'" (2013,
#   assignee LSU AgCenter), reporting 23% DM in freshly harvested roots
#   across LA/MS/AR trials 2009-2011, "similar for '07-146' and
#   'Beauregard'". Different growing region than this manuscript's NC
#   trials; no NC-specific figure found.
#
# - Bellevue: 0.254 -- this manuscript's own raw-sample derivation (DAT >=
#   90, n=12, Caswell 2021 only -- see caveats above; not cross-checked
#   against a directly comparable published fresh-tissue number). The one
#   published figure found, LaBonte et al., US Plant Patent PP26,735 P3 /
#   HortScience 50(6):930-931 (2015), reports 20.5% DM on BAKED roots
#   (stored 3 months, then baked), not raw fresh tissue, so it isn't
#   directly comparable and wasn't substituted in.
#
# - Monaco: 0.266 -- this manuscript's own raw-sample derivation (same
#   caveats as Bellevue). No published dry-matter figure was found for
#   Monaco in cultivar-release literature or variety trial reports as of
#   this writing.
#
# - Average: mean of the four cultivar values above (not a raw sheet
#   cultivar itself).
#
# General sanity check: orange-fleshed sweetpotato cultivars typically run
# ~19-31% dry matter (Mugisa et al. 2022, Frontiers in Plant Science
# 13:956936) -- Covington/Bayou Belle fall inside that band; Bellevue/Monaco
# are a bit above it but not implausibly so.
# ============================================================
DRY_MATTER_FRACTION <- c(
  "Bayou Belle" = 0.230,
  "Bellevue"    = 0.254,
  "Covington"   = 0.198,
  "Monaco"      = 0.266,
  "Average"     = 0.237
)

dw_to_fw <- function(dw_t_ha, cultivar) {
  frac <- unname(DRY_MATTER_FRACTION[cultivar])
  if (is.na(frac)) return(rep(NA_real_, length(dw_t_ha)))
  dw_t_ha / frac
}
