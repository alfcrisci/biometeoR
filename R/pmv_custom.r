#' PMV_custom
#'
#'  Calculate Predicted Mean Vote (PMV) following ISO 7730 customizing some individual features.
#'
#' @param numeric t Air temperature in Celsius degrees.
#' @param numeric rh Air Relative humidity in %.
#' @param numeric wind Windspeed in meter per second [m/s].
#' @param numeric tr Air temperature in Celsius degrees.
#' @param numeric M Metabolic rate of subject [W/mq].
#' @param numeric clo Clothing insulation level in clo.
#' @param numeric age Age in years.
#' @param numeric mbody Body Mass in kg. 
#' @param numeric ht Heigth of subject in meters.
#' @return 
#'
#'
#' @author  Istituto di Biometeorologia Firenze Italy  Alfonso Crisci \email{a.crisci@@ibimet.cnr.it}
#' @keywords PMV customized ISO 7730. 
#' 
#' @export
#'
#'
#'
#'

PMV_custom <- function(t, rh, wind, tr, M, age, mbody,  ht,clo) {
    mtrad=tr;
    iclo=clo;
    .Call('biometeoR_pmv_custom', PACKAGE = 'biometeoR', t, rh, wind, mtrad, met, age, ht, mbody, iclo)
}
