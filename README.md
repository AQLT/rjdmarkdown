
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rjdmarkdown

[![Travis Build
Status](https://img.shields.io/travis/AQLT/rjdmarkdown.svg?logo=travis)](https://travis-ci.org/AQLT/rjdmarkdown)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/rjdmarkdown)](https://cran.r-project.org/package=rjdmarkdown)

## Overview

rjdmarkdown gives a set of functions to print nicely your models created
by [RJDemetra](https://github.com/nbbrd/rjdemetra).

## Installation

rjdmarkdown relies on RJDemetra that requires Java SE 8 or later
version.

``` r
# Install development version from GitHub
# install.packages("devtools")
devtools::install_github("AQLT/rjdmarkdown")
```

## Usage

For the moment the only function developped is `print_latex()` for
regarima models. You can try it creating a new rmarkdown file with the
option:

``` eval
header-includes:
   - \usepackage{booktabs}
   - \usepackage{float}
```

And the following example with the chunk option `results='asis'`:

``` r
library(RJDemetra)
library(rjdmarkdown)
myreg <- regarima_x13(ipi_c_eu[, "FR"])
print_latex(myreg)
```
