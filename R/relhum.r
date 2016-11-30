relhum <- function(mu, pws, pa) {
    .Call('biometeoR_relhum', PACKAGE = 'biometeoR', mu, pws, pa)
}
