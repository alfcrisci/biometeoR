#' UTCI
#'
#' Calculate Universal Thermal Climate Index (UTCI) index.
#'
#' @param numeric t Air temperature in Celsius degrees.
#' @param numeric rh Air Relative humidity in percentage.
#' @param numeric wind Wind speed in meter per second.
#' @param numeric tr Mean radiant temperature in Celsius Degrees
#' @return UTCI
#'
#'
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  UTCI 
#' @references Bröde P,Jendritzky G,Fiala D and Havenith G, (2011),
#' The Universal Thermal Climate Index UTCI in Operational Use,  International Journal of Biometeorology
#' \url{http://www.utci.org/isb/documents/windsor_vers05.pdf}
#' @export
#'
#'
#'
#'
utci <- function(t, rh, wind, tr) {
     tmrt=tr;
    .Call('biometeoR_utci', PACKAGE = 'biometeoR', t, rh, wind, tmrt)
}
