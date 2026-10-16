#' Surface and crown fire behavior for one fuel bed
#'
#' The full rothRmel calculation for one scan: Rothermel surface fire, Van
#' Wagner crown fire thresholds, Rothermel (1991) crown spread, the crown
#' fire type and the torching and crowning indices. Weather and moistures are
#' meant to be held constant across scans, so differences between scans come
#' from the fuels and canopy.
#'
#' @param load_tonsac Loads (tons/acre) named `d1`, `d10`, `d100`, `herb` and
#'   `woody`. Missing classes count as 0. See [brown_load()] for dead loads
#'   from time lag counts.
#' @param depth_ft Fuel bed depth (ft).
#' @param cbh_m Canopy base height (m); `NA` for no canopy.
#' @param cbd Canopy bulk density (kg/m^3); `NA` for no canopy.
#' @param wind_mph 20-ft wind speed (mi/h).
#' @param waf Wind adjustment factor (midflame / 20-ft wind).
#' @param slope_pct Slope (%).
#' @param m1,m10,m100 Dead fuel moistures (%).
#' @param m_herb,m_woody Live fuel moistures (%).
#' @param mx_dead Dead moisture of extinction (%).
#' @param fmc Foliar moisture content (%).
#' @param sav Surface-area-to-volume ratios (ft^2/ft^3) of the five classes.
#' @param dynamic `TRUE` to cure part of the live herbaceous load as in
#'   Scott and Burgan (2005) dynamic fuel models; see [herb_cured_fraction()].
#' @return A list: `ros_ch_hr`, `ros_m_min`, `fli_kw_m`, `flame_ft`,
#'   `flame_m`, `rxn_int` (BTU/ft^2/min), `hpa` (BTU/ft^2), `crown_Io`
#'   (kW/m), `crown_Ro` (m/min), `crown_ros_m` (m/min), `torching_idx` and
#'   `crowning_idx` (mi/h) and `fire_type` (0 surface, 1 passive, 2 active).
#'   Without a fuel bed depth every value is `NA`.
#' @examples
#' fire_behavior(
#'   load_tonsac = c(d1 = 0.24, d10 = 1.44, d100 = 0.81, herb = 0.1, woody = 0.7),
#'   depth_ft = 0.5, cbh_m = 6, cbd = 0.08, wind_mph = 10
#' )
#' @export
fire_behavior <- function(load_tonsac, depth_ft, cbh_m = NA_real_, cbd = NA_real_,
                          wind_mph = 10, waf = 0.3, slope_pct = 0,
                          m1 = 6, m10 = 7, m100 = 8, m_herb = 90, m_woody = 90,
                          mx_dead = 25, fmc = 100,
                          sav = FUEL_CONSTANTS$sav, dynamic = FALSE) {
  classes <- c("d1", "d10", "d100", "herb", "woody")
  load <- stats::setNames(rep(0, 5), classes)
  given <- intersect(names(load_tonsac), classes)
  load[given] <- load_tonsac[given]
  load[is.na(load)] <- 0

  na_out <- list(
    ros_ch_hr = NA_real_, ros_m_min = NA_real_, fli_kw_m = NA_real_, flame_ft = NA_real_,
    flame_m = NA_real_, rxn_int = NA_real_, hpa = NA_real_, crown_Io = NA_real_,
    crown_Ro = NA_real_, crown_ros_m = NA_real_, torching_idx = NA_real_,
    crowning_idx = NA_real_, fire_type = NA_real_
  )
  if (is.na(depth_ft) || depth_ft <= 0) return(na_out)

  mf5 <- c(m1, m10, m100, m_herb, m_woody) / 100
  # split the live herb into cured (dead) and live herb: a six-class bed
  cured <- if (isTRUE(dynamic)) herb_cured_fraction(m_herb) else 0
  load6 <- c(load[c("d1", "d10", "d100")], dherb = load[["herb"]] * cured,
             herb = load[["herb"]] * (1 - cured), woody = load[["woody"]])
  sav6 <- c(sav[1:3], dherb = sav[[4]], sav[4:5])
  mf6 <- c(mf5[1:3], mf5[1], mf5[4:5])
  load_lbft2 <- load6 * TONSAC_TO_LBFT2
  slope_frac <- slope_pct / 100
  no_dead_fuel <- sum(load_lbft2[1:4]) <= 0

  surf <- if (no_dead_fuel) {
    list(ros_ft_min = 0, I_R = 0, I_B_kw = 0, flame_ft = 0, hpa = 0)
  } else {
    rothermel_surface(load_lbft2, sav6, mf6, depth_ft, mx_dead / 100,
                      waf * wind_mph * MPH_TO_FTMIN, slope_frac)
  }

  i_o <- crown_initiation_intensity(cbh_m, fmc)
  r_o <- critical_crown_ros(cbd)
  cros <- crown_ros(wind_mph, mf5, slope_frac)

  list(
    ros_ch_hr = surf$ros_ft_min * FTMIN_TO_CHHR,
    ros_m_min = surf$ros_ft_min * FTMIN_TO_MMIN,
    fli_kw_m = surf$I_B_kw,
    flame_ft = surf$flame_ft,
    flame_m = surf$flame_ft * FT_TO_M,
    rxn_int = surf$I_R,
    hpa = surf$hpa,
    crown_Io = i_o,
    crown_Ro = r_o,
    crown_ros_m = cros,
    torching_idx = if (no_dead_fuel) NA_real_ else {
      torching_index(load_lbft2, sav6, mf6, depth_ft, mx_dead / 100, waf, slope_frac, i_o)
    },
    crowning_idx = crowning_index(mf5, slope_frac, r_o),
    fire_type = crown_fire_type(surf$I_B_kw, i_o, cros, r_o)
  )
}
