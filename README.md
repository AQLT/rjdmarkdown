
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

For the moment the only functions developped are:

  - `print_preprocessing()` for the pre-processing model;  
  - `print_decomposition()` for the decomposition.

The available outputs are
[LaTeX](https://aqlt.github.io/rjdmarkdown/articles/rjdmarkdown-pdf.pdf)
(`format = "latex"`, the package booktabs and float must be used) and
[HTML](https://aqlt.github.io/rjdmarkdown/articles/rjdmarkdown-html.html)
outputs (`format = "html"`).

The option `results='asis'` must be used in the chunk:

``` r
library(RJDemetra)
library(rjdmarkdown)
mysa <- x13(ipi_c_eu[, "FR"])
print_preprocessing(mysa, format = "latex")
print_decomposition(mysa, format = "latex")
```
