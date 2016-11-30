#' mmHg2mb
#'
#' Conversion mmHg2mb to millibar [hPa].
#'
#' @param numeric  mmHg millimeter of mercures.
#' @return 
#'
#'
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  mmHg2mb
#' 
#' @export
#'
#'
#'
#'

mmHg2mb<-function(mmHg)
{
  return (mmHg * 1.33322);
}