#' ms2mph
#' 
#' Conversion speed from meter per second to knots.
#'
#' @param numeric  ms Speed in meter per second.
#' @return 
#'
#'
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  ms2mph
#' 
#' @export
#'
#'
#'
#'

ms2mph<-function(ms)
{
  return (ms * 2.23694);
}