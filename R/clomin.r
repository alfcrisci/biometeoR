clomin <- function(t, rh, wind, trad) {
    .Call('biometeoR_clomin', PACKAGE = 'biometeoR', t, rh, wind, trad)
}

