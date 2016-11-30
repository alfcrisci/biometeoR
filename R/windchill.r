windchill <- function(t, wind) {
    .Call('biometeoR_new_windchill', PACKAGE = 'biometeoR', t, wind)
}
