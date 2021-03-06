
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NUPEX

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/BrunoVilela/NUPEX.svg?branch=master)](https://travis-ci.com/BrunoVilela/NUPEX)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/BrunoVilela/NUPEX?branch=master&svg=true)](https://ci.appveyor.com/project/BrunoVilela/NUPEX)
[![Codecov test
coverage](https://codecov.io/gh/BrunoVilela/NUPEX/branch/master/graph/badge.svg)](https://codecov.io/gh/BrunoVilela/NUPEX?branch=master)
<!-- badges: end -->

The goal of NUPEX package is to help with analysis of academic output
from the Brazilian lattes platform and from other administrative
sources.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("BrunoVilela/NUPEX")
```

## Example

This is a basic example of how to obtain all data from a lattes XML
file:

``` r
library(NUPEX)
# Path to the XML file
path_lattes <- paste0(system.file("lattes", 
                                  package = "NUPEX"),
                      "/lattes1.xml")
# Run the main function to obtain all lattes data into a list of tables
lattes_data <- get_lattes(path_lattes)
```

This is an example to obtain lattes data from multiple XMLs:

``` r
# Path to the XMLs files
path_lattes_folder <- system.file("lattes", package = "NUPEX")
# Run the main function to obtain all lattes data into a list of tables
lattes_data_multiple <- get_lattes_folder(path_lattes_folder)
#>   |                                                                              |                                                                      |   0%  |                                                                              |==================                                                    |  25%  |                                                                              |===================================                                   |  50%  |                                                                              |====================================================                  |  75%  |                                                                              |======================================================================| 100%
```
