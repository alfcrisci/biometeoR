#' mmHG2inches
#'
#' @description Return inches from millimeter of mercure.
#' 
#' @param mm millimeter
#' @references 
#' Istituto di Biometeorologia Firenze Italy
#' @author  Alfonso crisci \email{a.crisci@@ibimet.cnr.it} Marco Morabito \email{m.morabito@@unifi.it} 
#' @examples
#' 
#' 
#' @export

mmHG2inches<-function(mm)
{
  return( mm * 0.0393701);
}