pmv_hoppe_iso <- function(t, rh, wind, tr, clo) {
     mtrad=tr;
     iclo=clo;  
    .Call('biometeoR_pmv_hoppe_iso', PACKAGE = 'biometeoR', t, rh, wind, mtrad, iclo)
}
