# rjdmarkdown 0.2.2

* correction in the create file of `create_rmd()`.

* functions used in `create_rmd()` can be removed using a `NULL` value.


# rjdmarkdown 0.2.1

* `rjdmarkdown` now depends on `RJDemetra`.

* SystemRequirements update for CRAN policies.

# rjdmarkdown 0.2.0

## New function

- New function `create_rmd` to create and render a R Markdown from a specific object: `SA`, `jSA`, `sa_item`, `multiprocessing` (all the models of the `multiprocessing` are printed) or workspace object (all the models of all the `multiprocessing` of the `workspace` are printed).