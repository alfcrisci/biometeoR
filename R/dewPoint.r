dewPoint <- function(celsius, humidity) {
    .Call('biometeoR_dewPoint', PACKAGE = 'biometeoR', celsius, humidity)
}

