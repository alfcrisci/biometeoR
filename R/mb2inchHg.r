#' mb2inchHG
#'
#' Conversion from Millibar [hPa] to inches HG.
#'
#' @param numeric   mb Air pressure in Millibar [hPa] 
#' @return 
#'
#'
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  mbtoinchHG 
#' 
#' @export
#'
#'
#'
#'

mb2inchHG<-function(mb)
{
  return (mb/ 33.8638864);
}