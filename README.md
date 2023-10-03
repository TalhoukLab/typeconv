
<!-- README.md is generated from README.Rmd. Please edit that file -->

# typeconv

<!-- badges: start -->
<!-- badges: end -->

The goal of typeconv is to provide a consistent API for converting
clinical data package variables back to their native types
(e.g. character, factor, numeric, Date). Our deidentified, processed,
clinical data are stored as data objects in R packages, where the class
of each variable is the name of the R package. The main function of
typeconv is `to_native_type()`, and allows for the conversion of
variables back to native types.

## Installation

You can install the development version of typeconv like so:

``` r
remotes::install_github("TalhoukLab/typeconv")
```

## Example

This is a basic example of type conversion for the `caEndometrial`
package:

``` r
library(typeconv)
data("emdb.ca", package = "caEndometrial")
dat <- to_native_type(emdb.ca)
```
