#' kmh2knots
#'
#' Conversion from kilometer per hour to knot per second.
#'
#' @param numeric  kmh Speed in kilometer per hour.
#' @return 
#'
#'
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  kmh2knots 
#' 
#' @export
#'
#'
#'
#'

kmh2knots<-function(kmh)
{
 return(kmh * 0.539957);
}