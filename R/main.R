#' Calcul de la saison de croissance et de la sécheresse à partir des données BioSIM
#'
#' Calcul de la saison de croissance et de la sécheresse à partir des données BioSIM.
#' growing_season_and_drought_yearly est la fonction principale qui fait tout.
#'
#' @param data data.frame BioSIM data
#' @param min_growing_day  jour julien, Default 15 avril
#' @param max_growing_day jour julien, Default 15 août
#' @param threshold_growth seuil de température pour la croissance
#' @param threshold_dormancy seuil de température pour la dormance
#' @export
#' @rdname extreme_events
#' @examples
#' file = system.file("extdata", "dayclim_GS.txt", package="ExtremeEvents")
#' data <- read.table(file)
#'
#' growing_season_and_drought_yearly(data)
#'
#' GS = growing_season(data)
#' GS
#' data = drought(data, GS)
#' data
growing_season_and_drought_yearly = function(data, min_growing_day = 115, max_growing_day = 227, threshold_growth = 4, threshold_dormancy = 0, threshold_dd = 4, threshold_prec = 0)
{
 Year <- KeyID <- GSgdd <- GSprec <- drought_dd <- droughtevent_dd <- NULL

  data$yday <- lubridate::yday(as.Date(paste(data$Year, data$Month, data$Day, sep = "-")))

  data_GS = growing_season(data, min_growing_day, max_growing_day, threshold_growth, threshold_dormancy)
  data <- dplyr::left_join(data, data_GS)
  data <- drought(data, data_GS, threshold_dd, threshold_prec)

  #on peut ensuite ramener ça à l'échelle de l'année pour les analyses
  data_yearly <- data |>
    dplyr::group_by(KeyID, Year) |>
    dplyr::summarise(GSgdd = max(GSgdd),
                     GSprec = max(GSprec),
                     drought_dd = max(drought_dd),
                     droughtevent_dd = max(droughtevent_dd))

  data_yearly
}

#' @rdname extreme_events
#' @export
growing_season = function(data, min_growing_day = 115, max_growing_day = 227, threshold_growth = 4, threshold_dormancy = 0)
{
  Tmax <- Tmin <- yday <- days_above4 <- days_below0 <- sum_above4 <- sum_below0 <- KeyID <- Year <- GSstart <- GSend <- NULL

  data$yday <- lubridate::yday(as.Date(paste(data$Year, data$Month, data$Day, sep = "-")))

  data_GS <- data |>
    dplyr::mutate(days_above4 = ifelse(Tmax >= threshold_growth & yday > min_growing_day, 1, 0), #minimum pour le début de la saison de croissance le 15 avril
           days_below0 = ifelse(Tmin < threshold_dormancy & yday > max_growing_day, 1, 0),
           sum_above4 = cumsum_reset(days_above4, reset = 0),
           sum_below0 = cumsum_reset(days_below0, reset = 0),
           GSstart = ifelse(sum_above4 == 5, yday, NA),
           GSend = ifelse(sum_below0 == 5, yday, NA)) |>
    dplyr::group_by(KeyID, Year) |>
    dplyr::summarise(GSstart = min(GSstart, na.rm = TRUE), GSend = min(GSend, na.rm = TRUE))

  data_GS
}


#' @param data_GS data.frame produit par \link{growing_season}
#' @param threshold_dd seuil de temmpérature au dessus duquel les degrés-jours s'accumulent
#' @param threshold_prec seuil de précipitation journalière pour les sécheresses
#' @export
#' @rdname extreme_events
drought = function(data, data_GS, threshold_dd = 4, threshold_prec = 0)
{
  KeyID <- Year <- yday <- GSstart <- GSend <- GS_cond <- Tair <- growing_dd <- Prcp <- NULL

  data$yday <- lubridate::yday(as.Date(paste(data$Year, data$Month, data$Day, sep = "-")))

  data <- dplyr::left_join(data, data_GS)

  data <- data |>
    dplyr::group_by(KeyID, Year) |>
    dplyr::mutate(GS_cond = yday > GSstart & yday < GSend,
           growing_dd = ifelse(GS_cond & Tair > threshold_dd,  Tair - 4, 0),
           GSgdd = cumsum(ifelse(GS_cond & Tair > threshold_dd, growing_dd, 0)),
           GSprec = cumsum(ifelse(GS_cond, Prcp, 0)),
           drought_dd = cumsum(ifelse(GS_cond & Tair > threshold_dd & Prcp <= threshold_prec, growing_dd, 0)), #somme des degrés-jours pour toutes les sécheresses de la saison
           droughtevent_dd = cumsum_reset(ifelse(GS_cond & Tair > threshold_dd & Prcp <= threshold_prec, growing_dd, 0), reset =  0)) #somme des degrés-jour par événement de sécheresse

  data
}


