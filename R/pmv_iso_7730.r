pmv_iso_7730 <- function(ta, rh, vel, tr, met, wme, clo) {
    .Call('biometeoR_pmv_iso_7730', PACKAGE = 'biometeoR', ta, rh, vel, tr, met, wme, clo)
}
