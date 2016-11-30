pmv_holmer <- function(Ta, rh, v, Tr, w, Icl) {
    .Call('biometeoR_pmv_holmer', PACKAGE = 'biometeoR', Ta, rh, v, Tr, w, Icl)
}
