#' mph2ms
#' 
#' Conversion speed from mile per hour in meter per second.
#'
#' @param numeric  mph Speed in mile per hour.
#' @return 
#'
#'
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  mph2ms
#' 
#' @export
#'
#'
#'
#'

mph2ms<-function(mph)
{
  return (mph * 0.44704);
}