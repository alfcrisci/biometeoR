#' F2C
#'
#' Convert temperature from Fahrenheit degree to  Celsius degree.
#'
#' @param numeric F Temperature in Fahrenheit degrees
#' @return umeric  Temperature in Celsius degrees.
#'
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  F2C 
#' 
#' @export
#'
#'
#'
#'

F2C<-function(tF)
{
  return (tF * 0.555556- 17.7778);
}
