#' Growing season climate
#'
#' Compute growing season start and end dates, growing degree-day (Celsius degrees), and precipitation (mm), based on the output of the \code{generateWeather()} function using the model "Climatic_Daily"of the \code{BioSIM} package.
#'
#' The onset of the growing season is defined as the fifth consecutive day during which the mean daily temperature reaches a certain temperature threshold. The end of the growing season is identified as the fifth consecutive day during which the minimum temperature reaches a dormancy threshold.
#'
#'
#' @param data Data frame created from the generateWeather() function of BioSIM generated using the "Climatic_Daily" model
#' @param min_growing_day  Minimum starting date for the growing season (year day format). This minimum date is used to avoid winter thaws from inducing the start of the growing season.
#' @param min_ending_day Minimum ending date for the growing season (year day format). This minimum data is used to avoid late spring forest from inducing the end of the growing season.
#' @param threshold_growth Temperature threshold (Celsius degrees) for inducing growth. This threshold is used to induce the start of the growing season (after the 5th day during which the mean daily temperature reaches this threshold), and for calculating growing season growing degree-days, which are computed based on this threshold.
#' @param threshold_dormancy Temperature threshold (Celsius degrees) for inducing dormancy This threshold is used to induce the end of the growing season (after the 5th day during which the minimum daily temperature falls below this threshold).
#' @param threshold_rain Quantity of rain in mm to count the day as a day of rain during the growing season.
#' @export
#' @return A data.frame with the following columns: KeyID, Year, GSstart (first day of the growing season),
#' GSend (last day of the growing season), GSgdd (??), GSprec (total precipitation during the growing season),
#' GSRad (total radiation during the growing season), GSDaysOfRain
#' (number of days of rain with a precipitation above the input threshold during the growing season).
#' @examples
#' file = system.file("extdata", "dayclim_GS.txt", package="ExtremeEvents")
#' data <- read.table(file)
#'
#' GS = growing_season(data)
#' GS
growing_season = function(data, min_growing_day = 115, min_ending_day = 227, threshold_growth = 4, threshold_dormancy = 0, threshold_rain = 2)
{
  Tmax <- Tmin <- yday <- days_above4 <- days_below0 <- sum_above4 <- sum_below0 <- KeyID <- Year <- GSstart <- GSend <- GS_cond <- Tair <- growing_dd <- Prcp <- GSgdd <- GSprec <- NULL

  data$yday <- lubridate::yday(as.Date(paste(data$Year, data$Month, data$Day, sep = "-")))

  data_GS <- data |>
    dplyr::mutate(days_above4 = ifelse(Tair >= threshold_growth & yday > min_growing_day, 1, 0), #minimum pour le début de la saison de croissance le 15 avril
           days_below0 = ifelse(Tmin < threshold_dormancy & yday > min_ending_day, 1, 0),
           sum_above4 = cumsum_reset(days_above4, reset = 0),
           sum_below0 = cumsum_reset(days_below0, reset = 0),
           GSstart = ifelse(sum_above4 == 5, yday, NA),
           GSend = ifelse(sum_below0 == 5, yday, NA)) |>
    dplyr::group_by(KeyID, Year) |>
    dplyr::summarise(GSstart = min(GSstart, na.rm = TRUE), GSend = min(GSend, na.rm = TRUE))

  data <- dplyr::left_join(data, data_GS)

  data <- data |>
    dplyr::group_by(KeyID, Year) |>
    dplyr::mutate(GS_cond = yday > GSstart & yday < GSend,
                  growing_dd = ifelse(GS_cond & Tair > threshold_growth,  Tair - 4, 0),
                  GSgdd = cumsum(ifelse(GS_cond & Tair > threshold_growth, growing_dd, 0)),
                  GSprec = cumsum(ifelse(GS_cond, Prcp, 0)),
                  GSSRad = cumsum(ifelse(GS_cond, SRad, 0)),
                  GSDaysOfRain = cumsum(ifelse(GS_cond & Prcp >= threshold_rain, 1, 0)))

  data <- data |> dplyr::group_by(KeyID, Year) |>
    dplyr::summarise(GSstart = min(GSstart, na.rm = TRUE), GSend = min(GSend, na.rm = TRUE), GSgdd = max(GSgdd, na.rm = TRUE), GSprec = max(GSprec, na.rm = TRUE),
                     GSSRad = max(GSSRad), GSDaysOfRain = max(GSDaysOfRain))

  data
}

#' Compute drought indices based on daily climate
#'
#' This function identifies drought events occurring during the growing season and computes drought indices based on a precipitation threshold. The drought event starts whenever the daily precipitation is below that specified by the \code{threshold_prec} argument and ends when the daily precipitation is above this threshold.
#' The function cumulates degree-days (above the threshold indicated by \code{threshold_growth}) during the drought events. When \code{by_year = TRUE}, the function returns a data frame containing the maximum GDD for a single event (column \code{droughtevent_dd}) and the total amount of GDD cumulated during all the drought events that occurred in the entire growing season  (column \code{droughtGS_dd}).
#' The function also returns the maximum number of days for a single drought event \code{droughtevent_days}) and the total number of drought days over the entire growing season \code{droughtGS_days}).
#' When \code{by_year = FALSE}, the function returns a data frame containing daily values. This may be used to characterize each drought event.
#'
#' @param data Data frame created from the generateWeather() function of BioSIM generated using the "Climatic_Daily" model
#' @param data_GS data.frame produced by \link{growing_season}
#' @param threshold_growth Temperature threshold (Celsius degrees) for calculating growing degree-days during the drought event, which are computed based on this threshold.
#' @param threshold_prec Daily precipitation (mm) threshold for inducing a drought event. A drought event will start when the daily precipitation is below this threshold, and will continue until the daily precipitation is higher than this threshold.
#' @param by_year boolean. Summarise by year. If false, a daily output is generated.
#' @export
#' @examples
#' file = system.file("extdata", "dayclim_GS.txt", package="ExtremeEvents")
#' data <- read.table(file)
#'
#' GS = growing_season(data)
#' GS
#'
#' data = drought(data, GS)
#' data
#'
drought = function(data, data_GS, threshold_growth = 4, threshold_prec = 0, by_year = TRUE)
{
  KeyID <- Year <- yday <- GSstart <- GSend <- GS_cond <- Tair <- growing_dd <- Prcp <- droughtGS_dd <- droughtevent_dd <- drought_dd <- droughtGS_days <- droughtevent_days <- NULL

  data$yday <- lubridate::yday(as.Date(paste(data$Year, data$Month, data$Day, sep = "-")))

  data <- dplyr::left_join(data, data_GS)

  data <- dplyr::mutate(data,
                        GS_cond = yday > GSstart & yday < GSend,
                        growing_dd = ifelse(GS_cond & Tair > threshold_growth,  Tair - threshold_growth, 0),
                        drought_dd = ifelse(GS_cond & Tair > threshold_growth& Prcp <= threshold_prec, growing_dd, 0))

  data <- data |>
    dplyr::group_by(KeyID, Year)  |>
    dplyr::mutate(droughtGS_dd = ifelse(GS_cond & Tair > threshold_growth & Prcp <= threshold_prec, cumsum(drought_dd), 0), #somme des degrés-jours pour toutes les sécheresses de la saison
           droughtevent_dd = cumsum_reset(ifelse(GS_cond & Tair > threshold_growth & Prcp <= threshold_prec, drought_dd, 0), reset =  0),
           droughtGS_days =  ifelse(GS_cond & Tair > threshold_growth & Prcp <= threshold_prec, cumsum(drought_dd !=0), 0),
           droughtevent_days = cumsum_reset(ifelse(GS_cond & Tair > threshold_growth & Prcp <= threshold_prec, 1, 0), reset =  0))

  if (by_year)
  {
    data = dplyr::group_by(data, KeyID, Year) |>
      dplyr::summarise(droughtGS_dd = max(droughtGS_dd, na.rm = TRUE),
                       droughtevent_dd = max(droughtevent_dd, na.rm = TRUE),
                       droughtGS_days = max(droughtGS_days, na.rm = TRUE),
                       droughtevent_firstday = yday[which.max(droughtevent_days)] - max(droughtevent_days, na.rm = TRUE),
                       droughtevent_days = max(droughtevent_days, na.rm = TRUE))
  }

  data
}



#' Compute late spring frost indices based on daily climate
#'
#' This function identifies late spring frost (LSF) events occurring during the growing season. It computes a LSF severity index, which we defined as the number of growing degree-days cumulated before the frost events.
#' The temperature threshold (Celsius degrees) for identifying a LSF event is defined by the \code{threshold_frost} argument. This threshold should represent a temperature below which damage to the tree are expected.
#' The growing degree-days cumulate from the start of the growing season until a LSF occurs. The temperature above which to cumulate growing degree-days can be specified by the \code{threshold_growth} argument.
#' When \code{by_year = TRUE}, the function returns a data frame containing the maximum GDD for a single event (column \code{lsfevent_dd}) and the total amount of GDD cumulated before the last frost event of the growing season (column \code{lsfGS_dd}).
#' Similarly, the function returns a columns for the number of days with mean temperature abov the growth threshold before a the worst LSF event (column \code{lsfevent_days}, and the total number of days before the last LSF event of the growing season (column \code{lsfGS_days}).
#' When \code{by_year = FALSE}, the function returns as data frame containing daily values of \code{lsfevent_dd} and \code{lsfGS_dd}. This may be used to characterize each LSF event.
#'
#' @param data Data frame created from the generateWeather() function of BioSIM generated using the "Climatic_Daily" model
#' @param data_GS data.frame produced by \link{growing_season}
#' @param threshold_frost Daily precipitation (mm) threshold for inducing a drought event. A drought event will start when the daily precipitation is below this threshold, and will continue until the daily precipitation is higher than this threshold.
#' @param threshold_growth Temperature threshold (Celsius degrees) for cumulating growing degree-days before the frost event.
#' @param threshold_lsf Maximum date (day of year format) for considering late spring frost. This prevent identifying the end of the growing season as a LSF.
#' @param by_year boolean. Summarise by year. If false, a daily output is generated.
#' @export

lsf = function(data, data_GS, threshold_frost = -4, threshold_growth = 4, threshold_lsf = 196, by_year = TRUE)
{
  KeyID <- Year <- yday <- GSstart <- GSend <- GS_cond <- Tair <- Tmin <- growing_dd <- Prcp <- lsfGS_dd <- lsfGS_days <- lsfevent_days <- lsfevent_dd <- lsf_day_max <- lsf_day <-NULL

  data$yday <- lubridate::yday(as.Date(paste(data$Year, data$Month, data$Day, sep = "-")))

  data <- dplyr::left_join(data, data_GS)

  data <- dplyr::mutate(data,
                        GS_cond = yday > GSstart & yday < GSend,
                        growing_dd = ifelse(GS_cond & Tair > threshold_growth,  Tair - threshold_growth, 0))

  data <- dplyr::mutate(data,  lsf_day = ifelse(GS_cond & Tmin <= threshold_frost & yday <= threshold_lsf, yday, 0)) #marks days during which the temperature drop below the frost threshold

  data_lsf <- data |> dplyr::group_by(KeyID, Year) |>
    dplyr::summarise(lsf_day_max = max(lsf_day, na.rm = TRUE))

  data <- dplyr::left_join(data, data_lsf)

  data <- data |> dplyr::group_by(KeyID,Year)|>
    dplyr::mutate(lsfevent_dd = cumsum_reset(ifelse(GS_cond & yday <= lsf_day_max & Tmin >= threshold_frost, growing_dd, 0), reset = 0), #somme des degrés-jours pour toutes les sécheresses de la saison
                  lsfGS_dd = ifelse(GS_cond & yday <= lsf_day_max & Tmin >= threshold_frost, cumsum_reset(growing_dd, reset = GS_cond), 0),
                  lsfGS_days =  ifelse(GS_cond & yday <= lsf_day_max & Tmin >= threshold_frost, cumsum(lsfevent_dd !=0), 0),
                  lsfevent_days = cumsum_reset(ifelse(GS_cond & yday <= lsf_day_max & Tmin >= threshold_frost, 1, 0), reset =  0))

  if (by_year)
  {
    data = dplyr::group_by(data, KeyID, Year) |>
      dplyr::summarise(lsfGS_dd = max(lsfGS_dd, na.rm = TRUE),
                       lsfevent_dd = max(lsfevent_dd, na.rm = TRUE),
                       lsfGS_days = max(lsfGS_days, na.rm = TRUE),
                       lsfevent_firstday = yday[which.max(lsfevent_days)]- max(lsfevent_days, na.rm = TRUE),
                       lsfevent_days = max(lsfevent_days, na.rm = TRUE))
  }

  data
}


