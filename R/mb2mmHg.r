#' mb2mmHG
#'
#' Conversion from Millibar [hPa] to mmHG.
#'
#' @param numeric mb Air pressure in Millibar [hPa] 
#' @return 
#'
#'
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  mbtommHG 
#' 
#' @export
#'
#'
#'
#'
mb2mmHg<-function(mb)
{
  return (mb* 0.750062);
}

