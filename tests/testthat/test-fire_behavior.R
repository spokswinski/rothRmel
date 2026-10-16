# Reference values from the rothRmel tab of the IntELiMon Decision Support Tool
# (app/logic/fire_behavior.R), run on the same inputs. The package must give
# the same numbers as the app.

bed <- function(d1, d10, d100, herb, woody, depth_cm, cbh, cbd) {
  fire_behavior(c(d1 = d1, d10 = d10, d100 = d100, herb = herb, woody = woody),
                depth_cm * 0.0328084, cbh, cbd, wind_mph = 12, waf = 0.3, slope_pct = 20)
}

test_that("a mixed bed matches the app: surface fire, no crowning", {
  r <- bed(0.24, 1.44, 0.81, 0.1, 0.7, 15, 6, 0.08)
  expect_equal(r$ros_ch_hr, 5.004397, tolerance = 1e-6)
  expect_equal(r$fli_kw_m, 342.6056, tolerance = 1e-6)
  expect_equal(r$flame_ft, 3.723952, tolerance = 1e-6)
  expect_equal(r$rxn_int, 4189.617, tolerance = 1e-6)
  expect_equal(r$hpa, 1077.968, tolerance = 1e-6)
  expect_equal(r$crown_Io, 2475.578, tolerance = 1e-6)
  expect_equal(r$crown_Ro, 37.5)
  expect_equal(r$torching_idx, 62.60528, tolerance = 1e-5)
  expect_equal(r$crowning_idx, 16.98702, tolerance = 1e-5)
  expect_equal(r$fire_type, 0)
})

test_that("a heavy bed under a low canopy matches the app: active crown fire", {
  r <- bed(0.5, 2, 3, 0, 0, 40, 2, 0.2)
  expect_equal(r$ros_ch_hr, 50.55963, tolerance = 1e-6)
  expect_equal(r$fli_kw_m, 5680.073, tolerance = 1e-6)
  expect_equal(r$torching_idx, 0)
  expect_equal(r$crowning_idx, 7.862201, tolerance = 1e-5)
  expect_equal(r$fire_type, 2)
})

test_that("a bed without dead fuel doesn't spread", {
  r <- bed(0, 0, 0, 0.5, 0, 10, 5, 0.1)
  expect_equal(r$ros_ch_hr, 0)
  expect_equal(r$fli_kw_m, 0)
  expect_true(is.na(r$torching_idx))
  expect_equal(r$crowning_idx, 14.24561, tolerance = 1e-5)
  expect_equal(r$fire_type, 0)
})

test_that("no fuel bed depth gives no result, and no canopy no crown metrics", {
  expect_true(all(is.na(unlist(fire_behavior(c(d1 = 1), depth_ft = 0)))))
  r <- fire_behavior(c(d1 = 1, d10 = 1), depth_ft = 0.5)
  expect_true(is.na(r$crown_Io))
  expect_true(is.na(r$torching_idx))
  expect_equal(r$fire_type, 0)
})

test_that("Van Wagner thresholds follow their formulas", {
  expect_equal(crown_initiation_intensity(4, 100), (0.010 * 4 * (460 + 2590))^1.5)
  expect_equal(critical_crown_ros(0.1), 30)
  expect_true(is.na(crown_initiation_intensity(0)))
  expect_true(is.na(critical_crown_ros(NA)))
})

test_that("crown fire type follows the thresholds", {
  expect_equal(crown_fire_type(500, 1000, 50, 30), 0)
  expect_equal(crown_fire_type(1500, 1000, 20, 30), 1)
  expect_equal(crown_fire_type(1500, 1000, 40, 30), 2)
  expect_equal(crown_fire_type(1500, NA, 40, 30), 0)
})

test_that("Brown loads follow the planar intersect equation", {
  expect_equal(brown_load(64.1, "onehr"),
               11.64 * 64.1 * 0.0151 * 0.48 * 1.13 / (40 * 3.280839895))
  expect_equal(brown_load(7.3, "hunhr"), 0.808, tolerance = 1e-3)
  expect_true(is.na(brown_load(-1, "onehr")))
  expect_true(is.na(brown_load(NA, "tenhr")))
})

test_that("herbaceous curing runs from 30% to 120% moisture", {
  expect_equal(herb_cured_fraction(30), 1)
  expect_equal(herb_cured_fraction(150), 0)
  expect_equal(herb_cured_fraction(75), 1.333 - 1.11 * 0.75)
})
