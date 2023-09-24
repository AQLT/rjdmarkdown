init_rmd <- function(title, knitr_chunk_opts){
  yaml_obj <- c('---',
                sprintf('title: "%s"', title),
                'header-includes:', '  - \\usepackage{booktabs}',
                '  - \\usepackage{float}', '  - \\usepackage{array}',
                '  - \\usepackage{multirow}', '  - \\floatplacement{figure}{H}',
                '---',
                '')
  setup_obj <- c(
    '```{r setup, include = FALSE}',
    'library(RJDemetra)',
    'library(rjdmarkdown)',
    'do.call(knitr::opts_chunk$set,',
    knitr_chunk_opts,
    ')',
    '```',
    '')
  c(yaml_obj, setup_obj)
}
model_chunk <- function(model_name, preprocessing_fun = "print_preprocessing",
                        decomposition_fun = "print_decomposition",
                        diagnostics_fun = "print_diagnostics"){
  vect <- c()
  if (preprocessing_fun != "NULL")
    vect <- c(vect, preprocessing_fun)
  if (decomposition_fun != "NULL")
    vect <- c(vect, decomposition_fun)
  if (diagnostics_fun != "NULL")
    vect <- c(vect, diagnostics_fun)
  if (length(vect) > 0 ){
    c('', '```{r}',
      sprintf(paste0(vect, "(%s)"),
              model_name),
      '```', '')
  } else {
    ''
  }
}
sa_item_chunk <- function(model_name, workspace_name){
  c('', '```{r}',
    sprintf("jmod_ <- get_jmodel(%s, %s)", model_name, workspace_name),
    '```', '')
}
mp_chunk <- function(mp_name, i){
  c('', '```{r}',
    sprintf("jsaitem_ <- get_object(%s, %i)", mp_name, i),
    '```', '')
}
wk_chunk <- function(wk_name, i){
  c('', '```{r}',
    sprintf("jmultip_ <- get_object(%s, %i)", wk_name, i),
    '```', '')
}

#' Create and render 'rmarkdown' file 
#'
#' Function to create a 'rmarkdown' file with all the output and render it
#' @param x the object to render: it can be a \code{"SA"}, \code{"jSA"}, \code{"sa_item"},
#'  \code{"multiprocessing"} or \code{"workspace"} object
#' 
#' @param output_file the name of the output `rmarkdown` file.
#' @param output_format the R Markdown output format to convert to: \code{"pdf_document"} for
#' a pdf output, \code{"html_document"} for a HTML output. 
#' See \link[rmarkdown]{render} for more details.
#' @param preprocessing_fun the function used to print the preprocessing. 
#' \link{print_preprocessing} by default. 
#' If \code{preprocessing_fun = NULL} the function is not used.
#' @param decomposition_fun the function used to print the decomposition 
#' \link{print_decomposition} by default.
#' If \code{decomposition_fun = NULL} the function is not used.
#' @param diagnostics_fun the function used to print the diagnostics 
#' \link{print_diagnostics} by default.
#' If \code{diagnostics_fun = NULL} the function is not used.
#' @param title the title of the R Markdown document.
#' @param knitr_chunk_opts options for R code chunks.
#' See \link[knitr]{opts_chunk} for more details.
#' @param ... other arguments to pass to \link[rmarkdown]{render}.
#' @param workspace the workspace. Only used when \code{x} is a  \code{"sa_item"} or 
#' \code{"multiprocessing"}.
#'
#' @examples 
#' \donttest{
#' ipi <- RJDemetra::ipi_c_eu[, "FR"]
#' jsa_x13 <- RJDemetra::jx13(ipi)
#' 
#' output_file <- tempfile(fileext = ".Rmd")
#' create_rmd(jsa_x13, output_file, output_format = "pdf_document")
#' # To directly open the pdf:
#' browseURL(sub(".Rmd",".pdf", output_file, fixed = TRUE))
#' 
#' 
#' # To create a pdf from a workspace:
#' jsa_ts <- jtramoseats(ipi)
#' wk <- new_workspace()
#' mp <- new_multiprocessing(wk, "sa1")
#' add_sa_item(wk, "sa1", jsa_x13, "X13")
#' add_sa_item(wk, "sa1", jsa_ts, "TramoSeats")
#' 
#' # It's important to compute the workspace to be able
#' # to import the models
#' compute(wk)
#' 
#' output_file <- tempfile(fileext = ".Rmd")
#' create_rmd(wk, output_file, 
#'            output_format = c("pdf_document", "html_document"),
#'            output_options = list(toc = TRUE,
#'                                  number_sections = TRUE))
#' # To open the file:
#' browseURL(sub(".Rmd",".pdf", output_file, fixed = TRUE))
#' browseURL(sub(".Rmd",".html", output_file, fixed = TRUE))
#' }
#' @importFrom RJDemetra get_object get_name
#' @export
create_rmd <- function(x,
                       output_file,
                       output_format = "pdf_document", 
                       preprocessing_fun = print_preprocessing,
                       decomposition_fun = print_decomposition,
                       diagnostics_fun = print_diagnostics,
                       title = "Seasonal adjustment summary",
                       knitr_chunk_opts = list(fig.pos = "h", echo = FALSE, 
                                               results='asis', fig.cap = "S-I Ratio"), ...){
  UseMethod("create_rmd", x)
}
#' @name create_rmd
#' @rdname create_rmd
#' @export
create_rmd.SA <- function(x,
                          output_file,
                          output_format = "pdf_document", 
                          preprocessing_fun = print_preprocessing,
                          decomposition_fun = print_decomposition,
                          diagnostics_fun = print_diagnostics,
                          title = "Seasonal adjustment summary",
                          knitr_chunk_opts = list(fig.pos = "h", echo = FALSE, 
                                                  results='asis', fig.cap = "S-I Ratio"),...){
  init_rmd <- init_rmd(title = title,
                       knitr_chunk_opts = deparse(substitute(knitr_chunk_opts)))
  
  mod_obj <- model_chunk(model_name = deparse(substitute(x)),
                         preprocessing_fun = deparse(substitute(preprocessing_fun)),
                         decomposition_fun = deparse(substitute(decomposition_fun)),
                         diagnostics_fun = deparse(substitute(diagnostics_fun)))
  print(mod_obj)
  markobj <- c(init_rmd, mod_obj)
  
  writeLines(text = markobj,
             con = output_file)
  rmarkdown::render(input = output_file,
                    output_format = output_format,
                    ...)
}
#' @name create_rmd
#' @rdname create_rmd
#' @export
create_rmd.jSA <- function(x,
                           output_file,
                           output_format = "pdf_document", 
                           preprocessing_fun = print_preprocessing,
                           decomposition_fun = print_decomposition,
                           diagnostics_fun = print_diagnostics,
                           title = "Seasonal adjustment summary",
                           knitr_chunk_opts = list(fig.pos = "h", echo = FALSE, 
                                                   results='asis', fig.cap = "S-I Ratio"),
                          ...){
  init_rmd <- init_rmd(title = title,
                       knitr_chunk_opts = deparse(substitute(knitr_chunk_opts)))
  
  mod_obj <- model_chunk(model_name = deparse(substitute(x)),
                         preprocessing_fun = deparse(substitute(preprocessing_fun)),
                         decomposition_fun = deparse(substitute(decomposition_fun)),
                         diagnostics_fun = deparse(substitute(diagnostics_fun)))
  
  markobj <- c(init_rmd, mod_obj)
  
  writeLines(text = markobj,
             con = output_file)
  rmarkdown::render(input = output_file,
                    output_format = output_format,
                    ...)
}
#' @name create_rmd
#' @rdname create_rmd
#' @export
create_rmd.workspace <- function(x,
                                 output_file,
                                 output_format = "pdf_document", 
                                 preprocessing_fun = print_preprocessing,
                                 decomposition_fun = print_decomposition,
                                 diagnostics_fun = print_diagnostics,
                                 title = "Seasonal adjustment summary",
                                 knitr_chunk_opts = list(fig.pos = "h", echo = FALSE, 
                                                         results='asis', fig.cap = "S-I Ratio"),
                                 ...){
  init_rmd <- init_rmd(title = title,
                       knitr_chunk_opts = deparse(substitute(knitr_chunk_opts)))
  wk_name <- deparse(substitute(x))
  preprocessing_fun_ = deparse(substitute(preprocessing_fun))
  decomposition_fun_ = deparse(substitute(decomposition_fun))
  diagnostics_fun_ = deparse(substitute(diagnostics_fun))
  
  nb_mp <- RJDemetra::count(x)
  all_mp_obj <- lapply(seq_len(nb_mp),
                          function(i) {
                            extract_saitem <- wk_chunk(wk_name, i)
                            mp_tmp <- get_object(x, i)
                            mp_obj <- extract_mp_rmd(x = mp_tmp,
                                                     mp_name = "jmultip_",
                                                     section_level = "#",
                                                     workspace_name = wk_name,
                                                     preprocessing_fun = preprocessing_fun_,
                                                     decomposition_fun = decomposition_fun_,
                                                     diagnostics_fun = diagnostics_fun_)
                            c(extract_saitem, '',
                              mp_obj, '')
                          })
  all_mp_obj <- unlist(all_mp_obj)

  markobj <- c(init_rmd,
               all_mp_obj)
  writeLines(text = markobj,
             con = output_file)
  rmarkdown::render(input = output_file,
                    output_format = output_format,
                    ...)
}
#' @name create_rmd
#' @rdname create_rmd
#' @export
create_rmd.multiprocessing <- function(x,
                                       output_file,
                                       output_format = "pdf_document", 
                                       preprocessing_fun = print_preprocessing,
                                       decomposition_fun = print_decomposition,
                                       diagnostics_fun = print_diagnostics,
                                       title = "Seasonal adjustment summary",
                                       knitr_chunk_opts = list(fig.pos = "h", echo = FALSE, 
                                                               results='asis', fig.cap = "S-I Ratio"),
                                       ...,
                                       workspace){
  init_rmd <- init_rmd(title = title,
                       knitr_chunk_opts = deparse(substitute(knitr_chunk_opts)))
  # mp_name <- deparse(substitute(x))
  
  # nb_sa_objects <- RJDemetra::count(x)
  # all_sa_obj <- lapply(seq_len(nb_sa_objects),
  #                         function(i) {
  #                           saitem_name <- get_name(get_object(x, i))
  #                           extract_sa_obj <- mp_chunk(mp_name, i)
  #                           sa_item_obj <- sa_item_chunk(model_name = "jsaitem_",
  #                                                        workspace_name = deparse(substitute(workspace)))
  #                           mod_obj <- model_chunk(model_name = "jmod_",
  #                                                  preprocessing_fun = deparse(substitute(preprocessing_fun)),
  #                                                  decomposition_fun = deparse(substitute(decomposition_fun)),
  #                                                  diagnostics_fun = deparse(substitute(diagnostics_fun)))
  #                           c(sprintf("## SaItem '%s'", saitem_name),'',
  #                             extract_sa_obj,
  #                             sa_item_obj,
  #                             mod_obj,''
  #                           )
  #                         })
  # all_sa_obj <- unlist(all_sa_obj)
  
  mp_obj <- extract_mp_rmd(x = x,
                           section_level = "#",
                           mp_name = deparse(substitute(x)),
                           workspace_name = deparse(substitute(workspace)),
                           preprocessing_fun = deparse(substitute(preprocessing_fun)),
                           decomposition_fun = deparse(substitute(decomposition_fun)),
                           diagnostics_fun = deparse(substitute(diagnostics_fun)))
  markobj <- c(init_rmd,
               mp_obj)
  writeLines(text = markobj,
             con = output_file)
  rmarkdown::render(input = output_file,
                    output_format = output_format,
                    ...)
}
#' @name create_rmd
#' @rdname create_rmd
#' @export
create_rmd.sa_item <- function(x,
                               output_file,
                               output_format = "pdf_document", 
                               preprocessing_fun = print_preprocessing,
                               decomposition_fun = print_decomposition,
                               diagnostics_fun = print_diagnostics,
                               title = "Seasonal adjustment summary",
                               knitr_chunk_opts = list(fig.pos = "h", echo = FALSE, 
                                                       results='asis', fig.cap = "S-I Ratio"),
                              ...,
                               workspace){
  init_rmd <- init_rmd(title = title,
                       knitr_chunk_opts = deparse(substitute(knitr_chunk_opts)))
  saitem_obj <- extract_saitem_rmd(sa_name = get_name(x), 
                     section_level = "#",
                     model_name = deparse(substitute(x)),
                     workspace_name = deparse(substitute(workspace)),
                     preprocessing_fun = deparse(substitute(preprocessing_fun)),
                     decomposition_fun = deparse(substitute(decomposition_fun)),
                     diagnostics_fun = deparse(substitute(diagnostics_fun)))
  markobj <- c(init_rmd,
               saitem_obj)
  
  writeLines(text = markobj,
             con = output_file)
  rmarkdown::render(input = output_file,
                    output_format = output_format,
                    ...)
}

extract_saitem_rmd <- function(sa_name, section_level = "#",
                               model_name,
                               workspace_name,
                               preprocessing_fun,
                               decomposition_fun,
                               diagnostics_fun){
  sa_item_obj <- sa_item_chunk(model_name = model_name,
                               workspace_name = workspace_name)
  
  mod_obj <- model_chunk(model_name = "jmod_",
                         preprocessing_fun = preprocessing_fun,
                         decomposition_fun = decomposition_fun,
                         diagnostics_fun = diagnostics_fun)
  c(sprintf("%s Model '%s'",section_level, sa_name),
    '',
    sa_item_obj, 
    mod_obj,'')
}
extract_mp_rmd <- function(x, mp_name,
                           section_level = "#",
                           workspace_name,
                           preprocessing_fun,
                           decomposition_fun,
                           diagnostics_fun){
  nb_sa_objects <- RJDemetra::count(x)
  all_sa_obj <- lapply(seq_len(nb_sa_objects),
                       extract_mp_rmd_i,
                       x = x,mp_name = mp_name,
                       section_level = section_level,
                       workspace_name = workspace_name,
                       preprocessing_fun = preprocessing_fun,
                       decomposition_fun = decomposition_fun,
                       diagnostics_fun = diagnostics_fun)
  all_sa_obj <- unlist(all_sa_obj)
  c(sprintf("%s Multiprocessing '%s'",section_level, get_name(x)), '',
    all_sa_obj, '')
}
extract_mp_rmd_i <- function(i,x, mp_name,
                             section_level = "#",
                             workspace_name,
                             preprocessing_fun,
                             decomposition_fun,
                             diagnostics_fun){
  
  saitem_name <- get_name(get_object(x, i))
  extract_sa_obj <- mp_chunk(mp_name, i)
  
  saitem_obj <- extract_saitem_rmd(sa_name = saitem_name, 
                                   section_level = paste0("#", section_level),
                                   model_name = "jsaitem_",
                                   workspace_name = workspace_name,
                                   preprocessing_fun = preprocessing_fun,
                                   decomposition_fun = decomposition_fun,
                                   diagnostics_fun = diagnostics_fun)
  c(extract_sa_obj, saitem_obj)
}

