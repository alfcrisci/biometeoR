ta_comfort <- function(rh, iclo, wind, M, H) {
    .Call('biometeoR_ta_comfort', PACKAGE = 'biometeoR', rh, iclo, wind, M, H)
}
