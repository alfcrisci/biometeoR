#' mph2knots
#' 
#' Conversion speed from mile per hour in knots.
#'
#' @param numeric  mph Speed in mile per hour.
#' @return 
#'
#'
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  mph2knots
#' 
#' @export
#'
#'
#'
#'
mph2knots<-function(mph)
{
  return(mph * 0.868976);
}