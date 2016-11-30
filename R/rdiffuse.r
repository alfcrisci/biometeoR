rdiffuse <- function(radteoric, rshort) {
    .Call('biometeoR_rdiffuse', PACKAGE = 'biometeoR', radteoric, rshort)
}
