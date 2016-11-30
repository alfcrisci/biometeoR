#' C2F
#'
#' Convert temperature from Celsius degree to Fahrenheit degree.
#'
#' @param numeric C Temperature in Celsius degrees.
#' @return numeric  Temperature in Fahrenheit degrees.
#'
#'
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  Temperature 
#' 
#' @export
#'
#'
#'
#'

C2F<-function(t)
{
  return (t * 1.8+ 32.0);
}
