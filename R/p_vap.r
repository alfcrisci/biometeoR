p_vap <- function(t, rh) {
    .Call('biometeoR_p_vap', PACKAGE = 'biometeoR', t, rh)
}

