# rothRmel

The fire behavior calculations behind the **rothRmel** tab of the
[IntELiMon](https://intelimon.xyz) Decision Support Tool, as plain R functions,
so you can see and reuse exactly how the tool estimates surface and crown fire
behavior from fuels measured with terrestrial laser scanning (TLS).

> **Use for change tracking and treatment success only.** These models have
> not been verified for use with custom fuel beds generated from TLS, and the
> Rothermel-based calculations are not validated for planning. Compare
> potential fire behavior through time at the same plots under the same
> weather, and confirm important decisions with field observations and
> validated tools such as BehavePlus.

## Install

```r
# install.packages("remotes")
remotes::install_github("spokswinski/rothRmel")
```

The package uses base R only.

## Example

```r
library(rothRmel)

# dead loads from planar intersect counts per 40 m of transect (Brown, 1974)
loads <- c(
  d1   = brown_load(64.1, "onehr"),
  d10  = brown_load(103, "tenhr"),
  d100 = brown_load(7.3, "hunhr"),
  herb = 0.1, woody = 0.7          # live loads, tons/acre
)

fire_behavior(
  load_tonsac = loads,
  depth_ft = 15 * 0.0328084,       # 15 cm fuel bed
  cbh_m = 6, cbd = 0.08,           # canopy base height (m), bulk density (kg/m^3)
  wind_mph = 12, waf = 0.3, slope_pct = 20,
  m1 = 6, m10 = 7, m100 = 8, m_herb = 90, m_woody = 90,
  mx_dead = 25, fmc = 100
)
```

`fire_behavior()` returns surface rate of spread (ch/hr and m/min), fireline
intensity (kW/m), flame length (ft and m), reaction intensity, heat per unit
area, the crown initiation intensity I₀ and critical crown spread rate R₀, the
crown spread rate, the torching and crowning indices (mi/h) and the crown fire
type (0 surface, 1 passive, 2 active).

Each step is also its own function:

| Function | Calculation |
|---|---|
| `rothermel_surface()` | Rothermel (1972) surface spread, reaction intensity, Byram (1959) fireline intensity and flame length |
| `crown_initiation_intensity()` | Van Wagner (1977): I₀ = (0.010 · CBH · (460 + 25.9 · FMC))^1.5 |
| `critical_crown_ros()` | Van Wagner (1977): R₀ = 3.0 / CBD |
| `crown_ros()` | Rothermel (1991): 3.34 × fuel model 10 spread at 0.4 × the 20-ft wind |
| `crown_fire_type()` | Surface, passive or active crown fire from I₀ and R₀ |
| `torching_index()`, `crowning_index()` | Scott and Reinhardt (2001): the 20-ft wind at which each threshold is reached |
| `brown_load()` | Brown (1974) planar intersect loads |
| `herb_cured_fraction()` | Scott and Burgan (2005) dynamic fuel model curing |

The constants are exported too: `FUEL_CONSTANTS`, `FM10` and `BROWN_CLASSES`.

## How the IntELiMon tool uses it

For each scan, the tool builds a fuel bed from the site's custom models: the
1-, 10- and 100-hour loads from the predicted time lag counts (`brown_load()`)
and the predicted fuel bed depth. Canopy base height comes from the scan and
canopy bulk density from LANDFIRE. Weather, moistures and live loads are held
the same for every scan, so differences between scans and time steps come
from the measured fuels and canopy. The tests in `tests/testthat` check that
this package returns the same numbers as the tool.

## Known differences from BehavePlus

The model is implemented from the equations in Andrews (2018) and has not
been tested against a reference implementation. Known differences:

- Midflame wind is capped at the original Rothermel (1972) limit of 0.9 × I_R,
  not the later limit recommended by Andrews (2018), so spread at high winds
  can be lower than in BehavePlus.
- Net fuel loads use w₀(1 − S_T) summed within the dead and live categories,
  without BehavePlus's size subclass weighting.
- Crown fire spread always uses fuel model 10's own dead moisture of
  extinction (25%).

## References

- Anderson, H. E. (1982). Aids to determining fuel models for estimating fire
  behavior. Gen. Tech. Rep. INT-122. USDA Forest Service.
- Andrews, P. L. (2018). The Rothermel surface fire spread model and
  associated developments: a comprehensive explanation. Gen. Tech. Rep.
  RMRS-GTR-371. USDA Forest Service.
- Brown, J. K. (1974). Handbook for inventorying downed woody material. Gen.
  Tech. Rep. INT-16. USDA Forest Service.
- Byram, G. M. (1959). Combustion of forest fuels. In K. P. Davis (Ed.),
  *Forest fire: control and use* (pp. 61-89). McGraw-Hill.
- Pokswinski, S., Nelson, K., Maxwell, A., et al. (2026). IntELiMon: an
  automated framework for standardized terrestrial lidar processing in
  multi-disciplinary ecosystem monitoring. *Forestry*, 99(5), cpag083.
  https://doi.org/10.1093/forestry/cpag083
- Rothermel, R. C. (1972). A mathematical model for predicting fire spread in
  wildland fuels. Res. Pap. INT-115. USDA Forest Service.
- Rothermel, R. C. (1991). Predicting behavior and size of crown fires in the
  Northern Rocky Mountains. Res. Pap. INT-438. USDA Forest Service.
- Scott, J. H., & Burgan, R. E. (2005). Standard fire behavior fuel models: a
  comprehensive set for use with Rothermel's surface fire spread model. Gen.
  Tech. Rep. RMRS-GTR-153. USDA Forest Service.
- Scott, J. H., & Reinhardt, E. D. (2001). Assessing crown fire potential by
  linking models of surface and crown fire behavior. Res. Pap. RMRS-RP-29.
  USDA Forest Service.
- Van Wagner, C. E. (1977). Conditions for the start and spread of crown fire.
  *Canadian Journal of Forest Research*, 7(1), 23-34.

## License

CC0 1.0 Universal (public domain dedication); see `LICENSE`.
