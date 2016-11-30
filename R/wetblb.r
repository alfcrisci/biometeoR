wetblb <- function(tstar, w, hh, pa) {
    .Call('biometeoR_wetblb', PACKAGE = 'biometeoR', tstar, w, hh, pa)
}
