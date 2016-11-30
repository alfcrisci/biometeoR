thom <- function(t, p_hPa) {
    .Call('biometeoR_thom', PACKAGE = 'biometeoR', t, p_hPa)
}
