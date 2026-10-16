#' Rothermel (1972) surface fire spread
#'
#' The Rothermel surface fire spread model, implemented from the equations as
#' described by Andrews (2018), with fireline intensity, flame length and heat
#' per unit area after Byram (1959). All inputs and outputs are US customary.
#'
#' Fuel classes are given in this order: the dead classes (1-, 10- and
#' 100-hour, and optionally a cured dead herbaceous class after 100-hour),
#' then the two live classes (herbaceous and woody). The last two entries are
#' always treated as live.
#'
#' @param load_lbft2 Fuel loads (lb/ft^2), one per class.
#' @param sav Surface-area-to-volume ratios (ft^2/ft^3), one per class.
#' @param mf Fuel moistures as fractions (0.06 for 6%), one per class.
#' @param depth_ft Fuel bed depth (ft).
#' @param mx_dead Dead fuel moisture of extinction as a fraction.
#' @param midflame_ftmin Midflame wind speed (ft/min).
#' @param slope_frac Slope as rise over run (0.2 for 20%).
#' @param heat Low heat content (BTU/lb).
#' @param rho_p Oven-dry particle density (lb/ft^3).
#' @return A list with `ros_ft_min` (rate of spread, ft/min), `I_R`
#'   (reaction intensity, BTU/ft^2/min), `sigma` (characteristic
#'   surface-area-to-volume ratio), `I_B` (fireline intensity, BTU/ft/s),
#'   `I_B_kw` (kW/m), `flame_ft` (flame length, ft) and `hpa` (heat per unit
#'   area, BTU/ft^2). Every value is 0 when the bed has no depth or no fuel.
#' @references
#' Andrews, P. L. (2018). The Rothermel surface fire spread model and
#' associated developments: a comprehensive explanation. Gen. Tech. Rep.
#' RMRS-GTR-371. USDA Forest Service.
#'
#' Byram, G. M. (1959). Combustion of forest fuels. In K. P. Davis (Ed.),
#' Forest fire: control and use (pp. 61-89). McGraw-Hill.
#'
#' Rothermel, R. C. (1972). A mathematical model for predicting fire spread
#' in wildland fuels. Res. Pap. INT-115. USDA Forest Service.
#' @examples
#' # standard fuel model 10 at 6/7/8% dead and 90% live moisture, 5 mi/h
#' # midflame wind, flat ground
#' load <- FM10$load_tonsac * 2000 / 43560
#' rothermel_surface(load, FUEL_CONSTANTS$sav, c(0.06, 0.07, 0.08, 0.9, 0.9),
#'                   depth_ft = 1, mx_dead = 0.25, midflame_ftmin = 5 * 88,
#'                   slope_frac = 0)
#' @export
rothermel_surface <- function(load_lbft2, sav, mf, depth_ft, mx_dead,
                              midflame_ftmin, slope_frac,
                              heat = FUEL_CONSTANTS$heat,
                              rho_p = FUEL_CONSTANTS$rho_p) {
  zero <- list(ros_ft_min = 0, I_R = 0, sigma = 0, I_B = 0, I_B_kw = 0,
               flame_ft = 0, hpa = 0)
  if (is.na(depth_ft) || depth_ft <= 0) return(zero)
  load_lbft2[is.na(load_lbft2)] <- 0
  if (sum(load_lbft2) <= 0) return(zero)
  s_t <- FUEL_CONSTANTS$s_t
  s_e <- FUEL_CONSTANTS$s_e

  n_class <- length(load_lbft2)
  dead <- seq_len(n_class - 2)
  live <- (n_class - 1):n_class

  # surface-area weighting
  a_ij <- sav * load_lbft2 / rho_p
  a_dead <- sum(a_ij[dead])
  a_live <- sum(a_ij[live])
  a_total <- a_dead + a_live
  if (a_total <= 0) return(zero)
  f_dead <- a_dead / a_total
  f_live <- a_live / a_total
  f_ij <- numeric(n_class)
  if (a_dead > 0) f_ij[dead] <- a_ij[dead] / a_dead
  if (a_live > 0) f_ij[live] <- a_ij[live] / a_live

  # characteristic surface-area-to-volume ratio
  sigma <- f_dead * sum(f_ij[dead] * sav[dead]) + f_live * sum(f_ij[live] * sav[live])
  if (sigma <= 0) return(zero)

  # bulk density and packing ratios
  rho_b <- sum(load_lbft2) / depth_ft
  beta <- rho_b / rho_p
  beta_op <- 3.348 * sigma^(-0.8189)
  rpr <- beta / beta_op

  # reaction velocity
  gamma_max <- sigma^1.5 / (495 + 0.0594 * sigma^1.5)
  a_coef <- 133 * sigma^(-0.7913)
  gamma <- gamma_max * rpr^a_coef * exp(a_coef * (1 - rpr))

  # net loads and category moistures
  wn_dead <- sum(load_lbft2[dead]) * (1 - s_t)
  wn_live <- sum(load_lbft2[live]) * (1 - s_t)
  mf_dead <- if (a_dead > 0) sum(f_ij[dead] * mf[dead]) else 0
  mf_live <- if (a_live > 0) sum(f_ij[live] * mf[live]) else 0

  # live fuel moisture of extinction
  mx_live <- mx_dead
  if (a_live > 0 && sum(load_lbft2[live]) > 0) {
    w_d <- sum(load_lbft2[dead] * exp(-138 / sav[dead]))
    w_l <- sum(load_lbft2[live] * exp(-500 / sav[live]))
    ratio <- if (w_l > 0) w_d / w_l else 0
    mf_dead_fine <- if (w_d > 0) {
      sum(load_lbft2[dead] * exp(-138 / sav[dead]) * mf[dead]) / w_d
    } else {
      0
    }
    mx_live <- max(2.9 * ratio * (1 - mf_dead_fine / mx_dead) - 0.226, mx_dead)
  }

  # moisture and mineral damping
  eta_m <- function(mfc, mxc) {
    if (mxc <= 0) return(0)
    rm <- min(mfc / mxc, 1)
    max(0, 1 - 2.59 * rm + 5.11 * rm^2 - 3.52 * rm^3)
  }
  eta_s <- min(1, 0.174 * s_e^(-0.19))

  # reaction intensity (BTU/ft^2/min)
  i_r <- gamma * (wn_dead * heat * eta_m(mf_dead, mx_dead) * eta_s +
                    wn_live * heat * eta_m(mf_live, mx_live) * eta_s)

  # propagating flux ratio
  xi <- exp((0.792 + 0.681 * sqrt(sigma)) * (beta + 0.1)) / (192 + 0.2595 * sigma)

  # wind factor, with the original effective wind speed limit of 0.9 I_R
  c_w <- 7.47 * exp(-0.133 * sigma^0.55)
  b_w <- 0.02526 * sigma^0.54
  e_w <- 0.715 * exp(-3.59e-4 * sigma)
  u <- min(max(0, midflame_ftmin), 0.9 * i_r)
  phi_w <- if (u > 0) c_w * u^b_w * rpr^(-e_w) else 0

  # slope factor
  phi_s <- 5.275 * beta^(-0.3) * slope_frac^2

  # heat sink
  eps <- exp(-138 / sav)
  q_ig <- 250 + 1116 * mf
  heat_sink <- rho_b * (f_dead * sum(f_ij[dead] * eps[dead] * q_ig[dead]) +
                          f_live * sum(f_ij[live] * eps[live] * q_ig[live]))

  ros <- if (heat_sink > 0) i_r * xi * (1 + phi_w + phi_s) / heat_sink else 0

  # Byram intensity and flame length
  hpa <- i_r * 384 / sigma
  i_b <- hpa * ros / 60
  list(
    ros_ft_min = ros, I_R = i_r, sigma = sigma, I_B = i_b,
    I_B_kw = i_b * BTUFTS_TO_KWM,
    flame_ft = if (i_b > 0) 0.45 * i_b^0.46 else 0,
    hpa = hpa
  )
}

#' Cured share of the live herbaceous load
#'
#' Scott and Burgan (2005) dynamic fuel models move part of the live
#' herbaceous load into a dead herbaceous class as it dries: all of it at 30%
#' herbaceous moisture, none at 120%.
#'
#' @param m_herb_pct Live herbaceous moisture (%).
#' @return The share cured, from 0 to 1.
#' @references
#' Scott, J. H., & Burgan, R. E. (2005). Standard fire behavior fuel models: a
#' comprehensive set for use with Rothermel's surface fire spread model. Gen.
#' Tech. Rep. RMRS-GTR-153. USDA Forest Service.
#' @examples
#' herb_cured_fraction(c(30, 60, 90, 120))
#' @export
herb_cured_fraction <- function(m_herb_pct) {
  pmin(1, pmax(0, 1.333 - 1.11 * m_herb_pct / 100))
}
