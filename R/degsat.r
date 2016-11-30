degsat <- function(t, rh, pa) {
    .Call('biometeoR_degsat', PACKAGE = 'biometeoR', t, rh, pa)
}

