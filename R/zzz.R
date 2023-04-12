.onAttach <- function(libname, pkgname) {

  packageStartupMessage(
    "Useful information sources:
    + help(package = \"hydrotoolbox\")
    + vignette(package = \"hydrotoolbox\")
    + https://gitlab.com/ezetoum27/hydrotoolbox
    *******************************************
    Don't forget to cite:
    + citation(package = \"hydrotoolbox\")"
    )

}
