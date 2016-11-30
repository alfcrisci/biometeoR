#' mph2kmh
#' 
#' Conversion speed from mile per hour in kilometer per second.
#'
#' @param numeric  mph Speed in mile per hour.
#' @return 
#'
#'
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  mph2kmh
#' 
#' @export
#'
#'
#'
#'
mph2kmh<-function(mph)
{
  return (mph * 1.60934);
}
