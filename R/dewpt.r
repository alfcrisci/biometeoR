dewpt <- function(t, kpa) {
    .Call('biometeoR_dewpt', PACKAGE = 'biometeoR', t, kpa)
}
