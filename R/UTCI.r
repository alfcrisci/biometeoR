utci <- function(t, rh, wind, tmrt) {
    .Call('biometeoR_utci', PACKAGE = 'biometeoR', t, rh, wind, tmrt)
}
