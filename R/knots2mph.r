#' knots2mph
#'
#' 
#' Conversion speed from knots per hour to mile per hour.
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

knots2mph<-function(knots)
{
  return (knots * 1.15078);
}
