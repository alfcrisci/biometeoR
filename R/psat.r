psat <- function(tdb) {
    .Call('biometeoR_psat', PACKAGE = 'biometeoR', tdb)
}
