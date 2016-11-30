clomax <- function(t, rh, wind, trad) {
    .Call('biometeoR_clomax', PACKAGE = 'biometeoR', t, rh, wind, trad)
}
