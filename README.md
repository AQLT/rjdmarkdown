
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rjdmarkdown

[![Travis Build
Status](https://img.shields.io/travis/AQLT/rjdmarkdown.svg?logo=travis)](https://travis-ci.org/AQLT/rjdmarkdown)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/rjdmarkdown)](https://cran.r-project.org/package=rjdmarkdown)
[![CRAN last
release](http://www.r-pkg.org/badges/last-release/rjdmarkdown)](https://cran.r-project.org/package=rjdmarkdown)
[![CRAN monthly
downloads](http://cranlogs.r-pkg.org/badges/rjdmarkdown?color=lightgrey)](https://cran.r-project.org/package=rjdmarkdown)
[![CRAN
downloads](http://cranlogs.r-pkg.org/badges/grand-total/rjdmarkdown?color=lightgrey)](https://cran.r-project.org/package=rjdmarkdown)

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

The functions developped are:

  - `print_preprocessing()` for the pre-processing model;  
  - `print_decomposition()` for the decomposition;  
  - `print_diagnostics()` to print diagnostics tests on the quality of
    the seasonal adjustment.

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
print_diagnostics(mysa, format = "latex")
```

[<img src="https://user-images.githubusercontent.com/24825189/85861799-2ecd5080-b7c1-11ea-9ea0-70ffea5248b3.png" alt="LaTeX output" width="400" align="left" />](https://aqlt.github.io/rjdmarkdown/articles/rjdmarkdown-pdf.pdf)
[<img src="https://user-images.githubusercontent.com/24825189/85861811-312faa80-b7c1-11ea-9105-e25d71c7df3e.png" alt="HTML output" width="400"  align="right"  />](https://aqlt.github.io/rjdmarkdown/articles/rjdmarkdown-html.html)
