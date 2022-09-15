
<!-- README.md is generated from README.Rmd. Please edit that file -->

# actmobilityr

The goal of `actmobilityr` is to provide functions to estimate the
economic value of the health effects associated to a shift from passive
to active mobility in Italian municipalities. The economic valuation
considers effects on mortality only but future versions of this package
will allow an estimation of effects on morbidity too. A shift from
passive to active mobility implies an increase in physical activity in
new adopters, which can, in turn, trigger positive effects on health.
Since most commuting is carried out in urban areas, however, such a
shift can have also negative effects on health because of higher air
pollutant intake and increased risk injuries. These are taken into
account in the estimation. The package allows to estimate unit benefit
factors associated to a mobility shift for any of the Italian
municipalities using - when available - municipality-specific air
pollutant concentration data, and province-specific mortality rate and
life expectancy data. The algorithm used to carry out the estimation
takes into account pre-existing physical activity levels among the
population (data refer to Italy as a whole but are age-specific) since
health benefits due to increased physical activity are stronger for
inactive people and are capped at a certain threshold of weekly physical
activity, above which no additional benefits are yielded. Active
mobility transportation modes considered are walking, bikes and e-bikes.
More information on the methodology used can be found in a [paper by
Mela and Girardi](https://doi.org/10.1016/j.jth.2022.101487).

## Installation

You can install the development version of actmobilityr from
[GitHub](https://github.com/) with:

``` r

install.packages("devtools")
devtools::install_github("giuliomela/actmobilityr")
```

``` r

library(actmobilityr)
```

## Examples

The package currently has two functions available for the user. One,
`scenario_builder`, is designed to define a mobility change scenario of
which the health impacts need to be estimated. The other,
`active_mob_impact`, quantifies the health benefits in economic terms.

The function `scenario_builder` can be used to create a modal shift
scenario for any of the Italian municipalities. The function takes
several parameters and returns a data frame containing the number of
individuals shifting from a given passive mobility mode to an active
one. The user can select a city of interest amongst all Italian
municipalities. The full documentation can be displayed typing
`?scenario_builder` in the console. One of trickiest parameters (and one
of few with no default values) the user must specify is
`mode_change_distance` which is a vector of length `4`. Each element of
`mode_change_distance` represents the share of total commuters shifting
from passive to active mobility for commuting travels shorter than 3 km
(one way), between 3 and 10 km, between 10 and 15 km and the longer than
15 km respectively (commuting distances are grouped in four classes and
not on a linear scale because the starting dataset - the Italian
Commuting Matrix - provides the data in this forma only). The user can
adjust these shares for different age groups with the `mode_change_age`
parameter (the default option is that shares are the same across age
groups). The output of `scenario_builder` is a data frame that is also
the main input for the `active_mob_impact` function.

``` r
library(actmobilityr)

### defining a mobility-change scenario for the city of Florence

# share of total commuters shifting to active mobility (less than 3 km, 3-10 km, 10-15 km, more than 15 km)
mode_change_shares <- c(0.2, 0.15, 0.1, 0.05) # for example, 0.2 means that 20% of total commuters travelling with a give passive mode of transportation shift to an active mode.

scenario <- scenario_builder(city = "Palermo", # city of interest
                 mode_from = "private_car_driver", # passive transportation mode
                 mode_to = "bike", # active transportation mode
                 mode_change_distance = mode_change_shares, # share of new adopters on total commuters
                 mode_change_age = rep(0, 9),
                 comm_days = 4, # weekly commuting days
                 max_km = 10) # maximum distance above which commuters don't change their transportation mode

head(scenario, 10)
#>      age  pop_share mode_change_age individuals daily_km mode_change_distance
#> 1  20-24 0.05795269               0    5691.740      5.5                 0.20
#> 2  20-24 0.05795269               0    5935.642     16.5                 0.15
#> 3  25-29 0.06173659               0    5691.740      5.5                 0.20
#> 4  25-29 0.06173659               0    5935.642     16.5                 0.15
#> 5  30-34 0.06083746               0    5691.740      5.5                 0.20
#> 6  30-34 0.06083746               0    5935.642     16.5                 0.15
#> 7  35-39 0.06470618               0    5691.740      5.5                 0.20
#> 8  35-39 0.06470618               0    5935.642     16.5                 0.15
#> 9  40-44 0.07157844               0    5691.740      5.5                 0.20
#> 10 40-44 0.07157844               0    5935.642     16.5                 0.15
#>             mode_from mode_to avg_speed met weekly_travel_time    city year
#> 1  private_car_driver    bike        15 6.8           1.466667 Palermo 2020
#> 2  private_car_driver    bike        15 6.8           4.400000 Palermo 2020
#> 3  private_car_driver    bike        15 6.8           1.466667 Palermo 2020
#> 4  private_car_driver    bike        15 6.8           4.400000 Palermo 2020
#> 5  private_car_driver    bike        15 6.8           1.466667 Palermo 2020
#> 6  private_car_driver    bike        15 6.8           4.400000 Palermo 2020
#> 7  private_car_driver    bike        15 6.8           1.466667 Palermo 2020
#> 8  private_car_driver    bike        15 6.8           4.400000 Palermo 2020
#> 9  private_car_driver    bike        15 6.8           1.466667 Palermo 2020
#> 10 private_car_driver    bike        15 6.8           4.400000 Palermo 2020
#>    weekly_comm_days
#> 1                 4
#> 2                 4
#> 3                 4
#> 4                 4
#> 5                 4
#> 6                 4
#> 7                 4
#> 8                 4
#> 9                 4
#> 10                4
```

Function `active_mob_impact` performs the economic estimation of the
benefits associated with the modal shift. The main input of this
function is the scenario built with `scenario_builder`, all the other
parameters have defaults but can indeed be changed by the user. See
`active_mob_impact` for more details.

``` r

active_mob_impact(scenario)
#> # A tibble: 1 × 11
#>   city    mode_…¹ mode_to avoid…² km_tot avoid…³ avoid…⁴ avoid…⁵ avoid…⁶ air_p…⁷
#>   <chr>   <chr>   <chr>     <dbl>  <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <lgl>  
#> 1 Palermo privat… bike       219. 2.39e9  4.23e8  8.10e8   0.177   0.338 TRUE   
#> # … with 1 more variable: injuries <lgl>, and abbreviated variable names
#> #   ¹​mode_from, ²​avoided_deaths, ³​avoided_voly, ⁴​avoided_vsl, ⁵​avoided_voly_km,
#> #   ⁶​avoided_vsl_km, ⁷​air_pollution
```

Function `active_mob_impact` returns a tibble with the outcome of the
economic evaluation: avoided deaths and their economic value estimated
with both the VOLY and the VSL. Values per km travelled are provided if
the `detail` parameters is set to `FALSE`
