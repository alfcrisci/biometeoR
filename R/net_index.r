net <- function(t, rh, wind) {
    .Call('biometeoR_net', PACKAGE = 'biometeoR', t, rh, wind)
}
