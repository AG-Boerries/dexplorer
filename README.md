
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dexpreprocessr

<!-- badges: start -->

<!-- badges: end -->

The goal of dexpreprocessr is to …

## Installation

You can install the development version of dexpreprocessr like so:

``` r
# currently also requires auth_token = {GITHUB_PAT}
BiocManager::install("AG-Boerries/dexplorer", , ask=FALSE, update=TRUE, build_vignettes = TRUE, dependencies = TRUE)


# devtools::install_git(
#   url = "https://gitlab.ibsm.uniklinik-freiburg.de/tobias.hundertmark/dexpreprocessr.git",
#   git = "external",
#   build_vignettes = TRUE
# )
```

- You need to have configured `git` to commit/push/pull/clone/… from our
  GitLab. [Here is the guide on how to set up the connection between
  `git` and
  GitLab.](https://ibsm.atlassian.net/wiki/spaces/TSKB/pages/1770139/Repository+-+AG+B+rries?atl_p=eyJpIjoiZDAxNTViZTctZTY3NS1iYzlmLWE4MDEtNDU5M2E5NjkwZTE4IiwidCI6ImZvbGxvd2luZ0ZlZWRSZWFkTW9yZSIsInNvdXJjZSI6ImVtYWlsIiwiZSI6ImNjLW5vdGlmaWNhdGlvbnNfZm9sbG93aW5nX2ZlZWQiLCJmYyI6IkZPTExPV0lORyIsImZyIjoxfQ)

Also make sure required BioConductor packages are installed:

``` r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install(c(
    "AnnotationDbi",
    "org.Mm.eg.db",
    "org.Hs.eg.db",
    "edgeR",
    "limma",
    "gage",
    "msigdbr"
))
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# library(dexpreprocessr)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
