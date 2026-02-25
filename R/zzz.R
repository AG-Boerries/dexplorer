.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\n\n",
    "########################################################################################\n",
    "################################ Welcome to 'DExploreR' ################################\n",
    "################ Visualize and interactively explore bulk RNA-seq data. ################\n",
    "########################################################################################\n",
    "\n"
  )
}

# TODO: check if all bioconductor packages are installed:
# checkBiocPkgs <- function(pkgs) {
#   for (pkg in pkgs) {
#     if (!requireNamespace(pkg, quietly = TRUE)) {
#       stop(
#         "Package '",
#         pkg,
#         "' is required but not installed.\n",
#         "Please install it (e.g. via BiocManager::install(\"",
#         pkg,
#         "\")).",
#         call. = FALSE
#       )
#     }
#   }
# }
