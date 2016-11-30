wbgt <- function(t, rh, wind) {
    .Call('biometeoR_wbgt', PACKAGE = 'biometeoR', t, rh, wind)
}
