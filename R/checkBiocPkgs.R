#' @title Ensure Required Bioconductor Packages Are Installed
#'
#' @description
#' This function checks if the specified Bioconductor packages are installed.
#'
#' @param pkgs A character vector of package names to check.
#'
#' @return Throws an error if any of the specified packages are not installed.
checkBiocPkgs <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(
        "Package '",
        pkg,
        "' is required but not installed.\n",
        "Please install it (e.g. via BiocManager::install(\"",
        pkg,
        "\")).",
        call. = FALSE
      )
    }
  }
}
