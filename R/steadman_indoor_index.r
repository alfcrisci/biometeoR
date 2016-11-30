steadman_indoor_index <- function(t, rh) {
    .Call('biometeoR_steadman_indoor', PACKAGE = 'biometeoR', t, rh)
}
