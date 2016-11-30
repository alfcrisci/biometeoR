#' knots2ms
#' 
#' Conversion speed from knots in meter per second.
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
knots2ms<-function(knots)
{
  return(knots * 0.514444);
}