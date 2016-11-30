#' heat_index
#'
#' Calculate the heat index following NOAA scheme.
#'
#' @param numeric t Air temperature in Celsius Degrees.
#' @param numeric rh Air Relative humidity .
#' @return 
#'
#'
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords  NOAA heat index 
#' @references NOAA index from George Winterling based on SteadMan's work.
#'
#' @export


heat_index <- function(t, rh) {
    .Call('biometeoR_heatindex', PACKAGE = 'biometeoR', t, rh)
}
