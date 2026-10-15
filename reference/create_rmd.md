# Create and render 'rmarkdown' file

Function to create a 'rmarkdown' file with all the output and render it

## Usage

``` r
create_rmd(
  x,
  output_file,
  output_format = "pdf_document",
  preprocessing_fun = print_preprocessing,
  decomposition_fun = print_decomposition,
  diagnostics_fun = print_diagnostics,
  title = "Seasonal adjustment summary",
  knitr_chunk_opts = list(fig.pos = "h", echo = FALSE, results = "asis", fig.cap =
    "S-I Ratio"),
  ...
)

# S3 method for class 'SA'
create_rmd(
  x,
  output_file,
  output_format = "pdf_document",
  preprocessing_fun = print_preprocessing,
  decomposition_fun = print_decomposition,
  diagnostics_fun = print_diagnostics,
  title = "Seasonal adjustment summary",
  knitr_chunk_opts = list(fig.pos = "h", echo = FALSE, results = "asis", fig.cap =
    "S-I Ratio"),
  ...
)

# S3 method for class 'jSA'
create_rmd(
  x,
  output_file,
  output_format = "pdf_document",
  preprocessing_fun = print_preprocessing,
  decomposition_fun = print_decomposition,
  diagnostics_fun = print_diagnostics,
  title = "Seasonal adjustment summary",
  knitr_chunk_opts = list(fig.pos = "h", echo = FALSE, results = "asis", fig.cap =
    "S-I Ratio"),
  ...
)

# S3 method for class 'workspace'
create_rmd(
  x,
  output_file,
  output_format = "pdf_document",
  preprocessing_fun = print_preprocessing,
  decomposition_fun = print_decomposition,
  diagnostics_fun = print_diagnostics,
  title = "Seasonal adjustment summary",
  knitr_chunk_opts = list(fig.pos = "h", echo = FALSE, results = "asis", fig.cap =
    "S-I Ratio"),
  ...
)

# S3 method for class 'multiprocessing'
create_rmd(
  x,
  output_file,
  output_format = "pdf_document",
  preprocessing_fun = print_preprocessing,
  decomposition_fun = print_decomposition,
  diagnostics_fun = print_diagnostics,
  title = "Seasonal adjustment summary",
  knitr_chunk_opts = list(fig.pos = "h", echo = FALSE, results = "asis", fig.cap =
    "S-I Ratio"),
  ...,
  workspace
)

# S3 method for class 'sa_item'
create_rmd(
  x,
  output_file,
  output_format = "pdf_document",
  preprocessing_fun = print_preprocessing,
  decomposition_fun = print_decomposition,
  diagnostics_fun = print_diagnostics,
  title = "Seasonal adjustment summary",
  knitr_chunk_opts = list(fig.pos = "h", echo = FALSE, results = "asis", fig.cap =
    "S-I Ratio"),
  ...,
  workspace
)
```

## Arguments

- x:

  the object to render: it can be a `"SA"`, `"jSA"`, `"sa_item"`,
  `"multiprocessing"` or `"workspace"` object

- output_file:

  the name of the output \`rmarkdown\` file.

- output_format:

  the R Markdown output format to convert to: `"pdf_document"` for a pdf
  output, `"html_document"` for a HTML output. See
  [render](https://pkgs.rstudio.com/rmarkdown/reference/render.html) for
  more details.

- preprocessing_fun:

  the function used to print the preprocessing.
  [print_preprocessing](print_preprocessing.md) by default. If
  `preprocessing_fun = NULL` the function is not used.

- decomposition_fun:

  the function used to print the decomposition
  [print_decomposition](print_decomposition.md) by default. If
  `decomposition_fun = NULL` the function is not used.

- diagnostics_fun:

  the function used to print the diagnostics
  [print_diagnostics](print_diagnostics.md) by default. If
  `diagnostics_fun = NULL` the function is not used.

- title:

  the title of the R Markdown document.

- knitr_chunk_opts:

  options for R code chunks. See
  [opts_chunk](https://rdrr.io/pkg/knitr/man/opts_chunk.html) for more
  details.

- ...:

  other arguments to pass to
  [render](https://pkgs.rstudio.com/rmarkdown/reference/render.html).

- workspace:

  the workspace. Only used when `x` is a `"sa_item"` or
  `"multiprocessing"`.

## Examples

``` r
# \donttest{
ipi <- RJDemetra::ipi_c_eu[, "FR"]
jsa_x13 <- RJDemetra::jx13(ipi)

output_file <- tempfile(fileext = ".Rmd")
create_rmd(jsa_x13, output_file, output_format = "pdf_document")
#> 
#> 
#> processing file: file1d682244eea9.Rmd
#> 1/5                  
#> 2/5 [setup]          
#> 3/5                  
#> 4/5 [unnamed-chunk-1]
#> Error: object 'jsa_x13' not found
#> Error: object 'jsa_x13' not found
#> Error: object 'jsa_x13' not found
#> 5/5                  
#> output file: file1d682244eea9.knit.md
#> /opt/hostedtoolcache/pandoc/3.8.3/x64/pandoc +RTS -K512m -RTS file1d682244eea9.knit.md --to latex --from markdown+autolink_bare_uris+tex_math_single_backslash --output file1d682244eea9.tex --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --embed-resources --standalone --syntax-highlighting tango --pdf-engine pdflatex --variable graphics --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/extract-data-uri.lua --include-in-header /tmp/Rtmpb6qGeJ/rmarkdown-str1d6812a6544b.html --variable 'geometry:margin=1in' 
#> ! LaTeX Error: Unicode character ^^[ (U+001B)
#>                not set up for use with LaTeX.
#> Try other LaTeX engines instead (e.g., xelatex) if you are using pdflatex. See https://bookdown.org/yihui/rmarkdown-cookbook/latex-unicode.html
#> Error: LaTeX failed to compile file1d682244eea9.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See file1d682244eea9.log for more info.
# To directly open the pdf:
browseURL(sub(".Rmd",".pdf", output_file, fixed = TRUE))


# To create a pdf from a workspace:
jsa_ts <- jtramoseats(ipi)
wk <- new_workspace()
mp <- new_multiprocessing(wk, "sa1")
add_sa_item(wk, "sa1", jsa_x13, "X13")
add_sa_item(wk, "sa1", jsa_ts, "TramoSeats")

# It's important to compute the workspace to be able
# to import the models
compute(wk)

output_file <- tempfile(fileext = ".Rmd")
create_rmd(wk, output_file, 
           output_format = c("pdf_document", "html_document"),
           output_options = list(toc = TRUE,
                                 number_sections = TRUE))
#> 
#> 
#> processing file: file1d683f624b84.Rmd
#> 1/17                  
#> 2/17 [setup]          
#> 3/17                  
#> 4/17 [unnamed-chunk-1]
#> Error: object 'wk' not found
#> 5/17                  
#> 6/17 [unnamed-chunk-2]
#> Error: object 'jmultip_' not found
#> 7/17                  
#> 8/17 [unnamed-chunk-3]
#> Error: object 'jsaitem_' not found
#> 9/17                  
#> 10/17 [unnamed-chunk-4]
#> Error: object 'jmod_' not found
#> Error: object 'jmod_' not found
#> Error: object 'jmod_' not found
#> 11/17                  
#> 12/17 [unnamed-chunk-5]
#> Error: object 'jmultip_' not found
#> 13/17                  
#> 14/17 [unnamed-chunk-6]
#> Error: object 'jsaitem_' not found
#> 15/17                  
#> 16/17 [unnamed-chunk-7]
#> Error: object 'jmod_' not found
#> Error: object 'jmod_' not found
#> Error: object 'jmod_' not found
#> 17/17                  
#> output file: file1d683f624b84.knit.md
#> /opt/hostedtoolcache/pandoc/3.8.3/x64/pandoc +RTS -K512m -RTS file1d683f624b84.knit.md --to latex --from markdown+autolink_bare_uris+tex_math_single_backslash --output file1d683f624b84.tex --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --embed-resources --standalone --table-of-contents --toc-depth 2 --number-sections --syntax-highlighting tango --pdf-engine pdflatex --variable graphics --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/extract-data-uri.lua --include-in-header /tmp/Rtmpb6qGeJ/rmarkdown-str1d684e3afa34.html --variable 'geometry:margin=1in' 
#> ! LaTeX Error: Unicode character ^^[ (U+001B)
#>                not set up for use with LaTeX.
#> Try other LaTeX engines instead (e.g., xelatex) if you are using pdflatex. See https://bookdown.org/yihui/rmarkdown-cookbook/latex-unicode.html
#> Error: LaTeX failed to compile file1d683f624b84.tex. See https://yihui.org/tinytex/r/#debugging for debugging tips. See file1d683f624b84.log for more info.
# To open the file:
browseURL(sub(".Rmd",".pdf", output_file, fixed = TRUE))
browseURL(sub(".Rmd",".html", output_file, fixed = TRUE))
# }
```
