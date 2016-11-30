p_local <- function(press, topo, temp) {
    .Call('biometeoR_p_local', PACKAGE = 'biometeoR', press, topo, temp)
}
