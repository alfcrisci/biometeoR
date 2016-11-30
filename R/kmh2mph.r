#' kmh2mph
#'
#' Conversion kilometer per hour to mile per hour.
#'
#' @param numeric  kmh Speed in Kilometer per hour.
#' @return 
#'
#'
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  kmh2mph 
#' 
#' @export
#'
#'
#'
#'

kmh2mph<-function(kmh)
{
  return (kmh * 0.621371);
}