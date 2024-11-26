# ExtremeEvents

Compute growing season and extreme event indices (drought and late spring frosts) from daily meteorogical data extracted using BioSIMR

## Installation

``` r
remotes::install_github("metafor-ulaval/ExtremEvents")
```

## Exemple

``` r
file = system.file("extdata", "dayclim_GS.txt", package="ExtremeEvents")
data <- read.table(file)
growing_season_and_drought_yearly(data)
```

