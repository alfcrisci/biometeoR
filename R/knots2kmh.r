#' knots2kmh
#' 
#' Conversion speed from knots in kilometer per hour.
#'
#' @param numeric  knots Speed in knots.
#' @return 
#'
#'
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  knots2mph
#' 
#' @export
#'
#'
#'
#'
knots2kmh<-function(knots)
{
  return(knots * 1.852);
}