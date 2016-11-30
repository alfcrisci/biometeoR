steadman_outdoor_index <- function(t, rh, wind, rshort, sunelev) {
    .Call('biometeoR_steadman_outdoor_sun', PACKAGE = 'biometeoR', t, rh, wind, rshort, sunelev)
}
