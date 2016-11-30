temprad <- function(t, rh, rshort, rdiffuse, sunelev, albedo) {
    .Call('biometeoR_temprad', PACKAGE = 'biometeoR', t, rh, rshort, rdiffuse, sunelev, albedo)
}
