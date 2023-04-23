
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rjdmarkdown

[![R-CMD-check](https://github.com/AQLT/rjdmarkdown/workflows/R-CMD-check/badge.svg)](https://github.com/AQLT/rjdmarkdown/actions)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/rjdmarkdown)](https://cran.r-project.org/package=rjdmarkdown)
[![CRAN last
release](http://www.r-pkg.org/badges/last-release/rjdmarkdown)](https://cran.r-project.org/package=rjdmarkdown)
[![CRAN monthly
downloads](http://cranlogs.r-pkg.org/badges/rjdmarkdown?color=lightgrey)](https://cran.r-project.org/package=rjdmarkdown)
[![CRAN
downloads](http://cranlogs.r-pkg.org/badges/grand-total/rjdmarkdown?color=lightgrey)](https://cran.r-project.org/package=rjdmarkdown)

## Overview

rjdmarkdown gives a set of functions to print nicely your models created
by [RJDemetra](https://github.com/jdemetra/rjdemetra).

## Installation

rjdmarkdown relies on RJDemetra that requires Java SE 8 or later
version.

``` r
# Install release version from CRAN
install.packages("rjdmarkdown")

# Install development version from GitHub
# install.packages("devtools")
devtools::install_github("AQLT/rjdmarkdown")
```

## Usage

The functions developped are:

- `print_preprocessing()` for the pre-processing model;  
- `print_decomposition()` for the decomposition;  
- `print_diagnostics()` to print diagnostics tests on the quality of the
  seasonal adjustment.

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

A R Markdown can also directly be created and render with the
`create_rmd` function. It can take as argument a `SA`, `jSA`, `sa_item`,
`multiprocessing` (all the models of the `multiprocessing` are printed)
or workspace object (all the models of all the `multiprocessing` of the
`workspace` are printed). For example:

``` r
sa_ts <- tramoseats(ipi_c_eu[, "FR"])
wk <- new_workspace()
new_multiprocessing(wk, "sa1")
add_sa_item(wk, "sa1", mysa, "X13")
add_sa_item(wk, "sa1", sa_ts, "TramoSeats")
# It's important to compute the workspace to be able to import the models
compute(wk)

output_file <- tempfile(fileext = ".Rmd")
create_rmd(wk, output_file, 
           output_format = c("pdf_document", "html_document"),
           output_options = list(toc = TRUE,
                                 number_sections = TRUE))
# To open the file:
browseURL(sub(".Rmd",".pdf", output_file, fixed = TRUE))
browseURL(sub(".Rmd",".html", output_file, fixed = TRUE))
```

### LaTeX output

[<img src="https://user-images.githubusercontent.com/24825189/85861799-2ecd5080-b7c1-11ea-9ea0-70ffea5248b3.png" alt="LaTeX output" width="500" />](https://aqlt.github.io/rjdmarkdown/articles/rjdmarkdown-pdf.pdf)

### HTML output

[<img src="https://user-images.githubusercontent.com/24825189/85861811-312faa80-b7c1-11ea-9105-e25d71c7df3e.png" alt="HTML output" width="500" />](https://aqlt.github.io/rjdmarkdown/articles/rjdmarkdown-html.html)
