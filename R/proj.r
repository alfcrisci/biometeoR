proj <- function(sunelev) {
    .Call('biometeoR_proj', PACKAGE = 'biometeoR', sunelev)
}

