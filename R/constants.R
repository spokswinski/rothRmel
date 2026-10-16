# Constants used by the fire behavior calculations. US customary units unless
# noted, as in Rothermel (1972) and Andrews (2018).

#' Fuel particle and fuel bed constants
#'
#' Values used for every fuel class by [rothermel_surface()].
#'
#' * `rho_p`: oven-dry particle density, 32 lb/ft^3
#' * `heat`: low heat content, 8,000 BTU/lb
#' * `s_t`: total mineral content, 0.0555
#' * `s_e`: effective (silica-free) mineral content, 0.010
#' * `sav`: standard surface-area-to-volume ratios (ft^2/ft^3) of the 1-, 10-
#'   and 100-hour, live herbaceous and live woody classes
#'
#' @format A named list.
#' @export
FUEL_CONSTANTS <- list(
  rho_p = 32,
  heat = 8000,
  s_t = 0.0555,
  s_e = 0.010,
  sav = c(d1 = 2000, d10 = 109, d100 = 30, herb = 1500, woody = 1500)
)

#' Standard fuel model 10 (Anderson, 1982)
#'
#' Timber (litter and understory), used by [crown_ros()] for the Rothermel
#' (1991) crown fire spread rate. Loads in tons/acre for the 1-, 10- and
#' 100-hour, live herbaceous and live woody classes; fuel bed depth in ft;
#' dead moisture of extinction as a fraction.
#'
#' @format A named list.
#' @export
FM10 <- list(
  load_tonsac = c(d1 = 3.01, d10 = 2.0, d100 = 5.01, herb = 0, woody = 2.0),
  depth_ft = 1.0,
  mx_dead = 0.25
)

#' Brown (1974) planar intersect constants
#'
#' For each time lag class: `d2`, the squared mean diameter (in^2); `s`, the
#' specific gravity; and `a`, the non-horizontal correction. Used by
#' [brown_load()].
#'
#' @format A named list of lists for the `onehr`, `tenhr`, `hunhr` and
#'   `thohr` (1000-hour sound) classes.
#' @export
BROWN_CLASSES <- list(
  onehr = list(d2 = 0.0151, s = 0.48, a = 1.13),
  tenhr = list(d2 = 0.289, s = 0.48, a = 1.13),
  hunhr = list(d2 = 2.76, s = 0.40, a = 1.13),
  thohr = list(d2 = 22.30, s = 0.40, a = 1.00)
)

# Unit conversions
TONSAC_TO_LBFT2 <- 2000 / 43560 # tons/acre -> lb/ft^2
CM_TO_FT <- 0.0328084
M_TO_FT <- 3.280839895
MPH_TO_FTMIN <- 88
FTMIN_TO_MMIN <- 0.3048
FTMIN_TO_CHHR <- 60 / 66 # 1 chain = 66 ft
BTUFTS_TO_KWM <- 3.46414
FT_TO_M <- 0.3048
