# ExtremeEvents

Calcul de la saison de croissance et de la sécheresse à partir des données BioSIM

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

