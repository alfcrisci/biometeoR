ssi <- function(t, rh) {
    .Call('biometeoR_ssi', PACKAGE = 'biometeoR', t, rh)
}
