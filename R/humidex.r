humidex <- function(t, rh) {
    .Call('biometeoR_humidex', PACKAGE = 'biometeoR', t, rh)
}
