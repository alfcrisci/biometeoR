#' ms2knots
#'
#' Conversion speed meter per second in knot per second.
#'
#' @param numeric   ms Speed in meter per second [m/s].
#' @return 
#'
#'
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  ms2knots 
#' 
#' @export
#'
#'
#'
#'

ms2knots<-function(ms)
{
  return(ms * 1.94384);
}