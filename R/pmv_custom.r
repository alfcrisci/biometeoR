pmv_custom <- function(t, rh, wind, mtrad, met, age, ht, mbody, iclo) {
    .Call('biometeoR_pmv_custom', PACKAGE = 'biometeoR', t, rh, wind, mtrad, met, age, ht, mbody, iclo)
}
