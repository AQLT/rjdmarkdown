# Changelog

## rjdmarkdown 0.2.4

## rjdmarkdown 0.2.3

CRAN release: 2026-10-13

- correction of
  [`print_preprocessing()`](../reference/print_preprocessing.md)
  (`"FALSE"` replaced by `FALSE`,
  [\#4](https://github.com/AQLT/rjdmarkdown/issues/4)).

## rjdmarkdown 0.2.2

CRAN release: 2023-10-04

- correction in the create file of
  [`create_rmd()`](../reference/create_rmd.md).

- functions used in [`create_rmd()`](../reference/create_rmd.md) can be
  removed using a `NULL` value.

## rjdmarkdown 0.2.1

CRAN release: 2023-04-25

- `rjdmarkdown` now depends on `RJDemetra`.

- SystemRequirements update for CRAN policies.

## rjdmarkdown 0.2.0

CRAN release: 2020-10-01

### New function

- New function `create_rmd` to create and render a R Markdown from a
  specific object: `SA`, `jSA`, `sa_item`, `multiprocessing` (all the
  models of the `multiprocessing` are printed) or workspace object (all
  the models of all the `multiprocessing` of the `workspace` are
  printed).
