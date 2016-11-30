spvol <- function(tc, w, pa) {
    .Call('biometeoR_spvol', PACKAGE = 'biometeoR', tc, w, pa)
}
