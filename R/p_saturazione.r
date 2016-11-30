p_saturazione <- function(t) {
    .Call('biometeoR_p_saturazione', PACKAGE = 'biometeoR', t)
}
