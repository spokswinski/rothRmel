#' Downed woody fuel load from a planar intersect count (Brown, 1974)
#'
#' W = 11.64 * n * d^2 * s * a * c / (N * L), with the class constants in
#' [BROWN_CLASSES], no slope correction (c = 1) and L the transect length in
#' feet.
#'
#' @param count Number of pieces of the class crossing the transect.
#' @param cls One of [BROWN_CLASSES], or the name of one ("onehr", "tenhr",
#'   "hunhr", "thohr").
#' @param n_planes Number of sampling planes (N).
#' @param transect_m Length of each plane (m).
#' @return Load (tons/acre); `NA` for a missing or negative count.
#' @references Brown, J. K. (1974). Handbook for inventorying downed woody
#'   material. Gen. Tech. Rep. INT-16. USDA Forest Service.
#' @examples
#' brown_load(64, "onehr")
#' brown_load(7.3, "hunhr")
#' @export
brown_load <- function(count, cls, n_planes = 1, transect_m = 40) {
  if (is.character(cls)) cls <- BROWN_CLASSES[[cls]]
  count <- suppressWarnings(as.numeric(count))
  out <- (11.64 * count * cls$d2 * cls$s * cls$a) / (n_planes * transect_m * M_TO_FT)
  out[is.na(count) | count < 0] <- NA_real_
  out
}
