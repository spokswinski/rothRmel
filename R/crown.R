#' Crown fire initiation intensity (Van Wagner, 1977)
#'
#' The surface fireline intensity needed to ignite the canopy.
#'
#' @param cbh_m Canopy base height (m).
#' @param fmc Foliar moisture content (%).
#' @return Critical surface fireline intensity I0 (kW/m); `NA` without a
#'   positive canopy base height.
#' @references Van Wagner, C. E. (1977). Conditions for the start and spread
#'   of crown fire. Canadian Journal of Forest Research, 7(1), 23-34.
#' @examples
#' crown_initiation_intensity(cbh_m = 4, fmc = 100)
#' @export
crown_initiation_intensity <- function(cbh_m, fmc = 100) {
  ifelse(!is.na(cbh_m) & cbh_m > 0, (0.010 * cbh_m * (460 + 25.9 * fmc))^1.5, NA_real_)
}

#' Critical spread rate for active crown fire (Van Wagner, 1977)
#'
#' @param cbd Canopy bulk density (kg/m^3).
#' @return Critical crown fire spread rate R0 (m/min); `NA` without a
#'   positive canopy bulk density.
#' @inherit crown_initiation_intensity references
#' @examples
#' critical_crown_ros(cbd = 0.1)
#' @export
critical_crown_ros <- function(cbd) {
  ifelse(!is.na(cbd) & cbd > 0, 3.0 / cbd, NA_real_)
}

#' Crown fire spread rate (Rothermel, 1991)
#'
#' 3.34 times the surface spread rate of standard fuel model 10 ([FM10]) at
#' a midflame wind of 0.4 times the 20-ft wind.
#'
#' @param wind_mph 20-ft wind speed (mi/h).
#' @param mf Fuel moistures as fractions for the 1-, 10- and 100-hour, live
#'   herbaceous and live woody classes.
#' @param slope_frac Slope as rise over run.
#' @return Crown fire spread rate (m/min).
#' @references Rothermel, R. C. (1991). Predicting behavior and size of crown
#'   fires in the Northern Rocky Mountains. Res. Pap. INT-438. USDA Forest
#'   Service.
#' @examples
#' crown_ros(wind_mph = 20, mf = c(0.06, 0.07, 0.08, 0.9, 0.9))
#' @export
crown_ros <- function(wind_mph, mf, slope_frac = 0) {
  surf <- rothermel_surface(
    FM10$load_tonsac * TONSAC_TO_LBFT2, FUEL_CONSTANTS$sav, mf, FM10$depth_ft,
    FM10$mx_dead, 0.4 * wind_mph * MPH_TO_FTMIN, slope_frac
  )
  3.34 * surf$ros_ft_min * FTMIN_TO_MMIN
}

#' Crown fire type
#'
#' @param i_b_kw Surface fireline intensity (kW/m).
#' @param i_o Crown initiation intensity from [crown_initiation_intensity()].
#' @param crown_ros_m Crown fire spread rate from [crown_ros()] (m/min).
#' @param r_o Critical spread rate from [critical_crown_ros()] (m/min).
#' @return 0 for surface fire, 1 for passive crown fire (torching), 2 for
#'   active crown fire.
#' @examples
#' crown_fire_type(i_b_kw = 1500, i_o = 1000, crown_ros_m = 20, r_o = 30)
#' @export
crown_fire_type <- function(i_b_kw, i_o, crown_ros_m, r_o) {
  if (is.na(i_o) || i_b_kw < i_o) return(0)
  if (!is.na(r_o) && crown_ros_m >= r_o) 2 else 1
}

#' Find the 20-ft wind at which a threshold is reached
#'
#' @param f Function of the 20-ft wind (mi/h), increasing, that crosses zero
#'   at the threshold.
#' @param lo,hi Search range (mi/h).
#' @return The wind (mi/h); 0 when the threshold is met with no wind; `NA`
#'   when it isn't reached within the range.
#' @noRd
solve_wind <- function(f, lo = 0.01, hi = 100) {
  f_lo <- tryCatch(f(lo), error = function(e) NA_real_)
  f_hi <- tryCatch(f(hi), error = function(e) NA_real_)
  if (is.na(f_lo) || is.na(f_hi)) return(NA_real_)
  if (f_lo >= 0) return(0)
  if (f_hi < 0) return(NA_real_)
  tryCatch(stats::uniroot(f, lower = lo, upper = hi)$root, error = function(e) NA_real_)
}

#' Torching and crowning indices (Scott and Reinhardt, 2001)
#'
#' The torching index is the 20-ft wind at which the surface fireline
#' intensity reaches the crown initiation intensity; the crowning index is
#' the 20-ft wind at which the crown spread rate reaches the critical rate
#' for active crown fire. Both are found by a numerical search from 0 to 100
#' mi/h. Higher values mean a lower crown fire hazard.
#'
#' @param load_lbft2,sav,mf,depth_ft,mx_dead,slope_frac Surface fuel bed and
#'   terrain, as in [rothermel_surface()].
#' @param waf Wind adjustment factor (midflame / 20-ft wind).
#' @param i_o Crown initiation intensity (kW/m).
#' @param r_o Critical spread rate for active crown fire (m/min).
#' @param mf_fm10 Moistures for the 1-, 10- and 100-hour, live herbaceous and
#'   live woody classes of fuel model 10.
#' @return The index (mi/h); 0 when the threshold is met with no wind; `NA`
#'   when there is no canopy or it isn't reached by 100 mi/h.
#' @references Scott, J. H., & Reinhardt, E. D. (2001). Assessing crown fire
#'   potential by linking models of surface and crown fire behavior. Res. Pap.
#'   RMRS-RP-29. USDA Forest Service.
#' @name crown_indices
NULL

#' @rdname crown_indices
#' @export
torching_index <- function(load_lbft2, sav, mf, depth_ft, mx_dead, waf, slope_frac, i_o) {
  if (is.na(i_o) || sum(load_lbft2[seq_len(length(load_lbft2) - 2)], na.rm = TRUE) <= 0) {
    return(NA_real_)
  }
  solve_wind(function(w) {
    rothermel_surface(load_lbft2, sav, mf, depth_ft, mx_dead,
                      waf * w * MPH_TO_FTMIN, slope_frac)$I_B_kw - i_o
  })
}

#' @rdname crown_indices
#' @export
crowning_index <- function(mf_fm10, slope_frac, r_o) {
  if (is.na(r_o)) return(NA_real_)
  solve_wind(function(w) crown_ros(w, mf_fm10, slope_frac) - r_o)
}
