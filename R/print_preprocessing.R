#' Print the pre-processing model
#' 
#' Function to print the pre-processing model
#' 
#' @param x the object to print.
#' @param format output format: \code{"latex"} or \code{"html"}.
#' @param signif.stars logical; if \code{TRUE}, p-values are additionally encoded visually as ‘significance stars’ in order to help scanning of long coefficient tables
#' @param digits number of digits after the decimal point.
#' @param decimal.mark the character to be used to indicate the numeric decimal point.
#' @param booktabs boolean indicating whether to use or not the booktabs package (when \code{format = "latex"}).
#' @param summary boolean indicating whether to use or not the summary section.
#' @param likelihood boolean indicating whether to use or not the likelihood section.
#' @param arima boolean indicating whether to use or not the arima section.
#' @param regression boolean indicating whether to use or not the regression section.
#' @param ... unused.
#' @examples 
#' ipi <- RJDemetra::ipi_c_eu[, "FR"]
#' 
#' sa_x13 <- RJDemetra::jx13(ipi)
#' print_preprocessing(sa_x13, format = "latex")
#' 
#' \donttest{
#' sa_ts <- RJDemetra::tramoseats(ipi)
#' print_preprocessing(sa_ts, format = "html")
#' }
#' 
#' @export
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling footnote
#' @importFrom magrittr %>%
#' @importFrom stats time printCoefmat
#' @importFrom graphics plot
print_preprocessing <- function(x, format = knitr::opts_knit$get('rmarkdown.pandoc.to'),
                                signif.stars = TRUE,
                           digits = 3, decimal.mark = getOption("OutDec"),
                           booktabs = TRUE,
                           summary = TRUE,
                           likelihood = TRUE,
                           arima = TRUE,
                           regression = TRUE, ...){
  UseMethod("print_preprocessing", x)
}
#' @export
print_preprocessing.SA <- function(x, format = knitr::opts_knit$get('rmarkdown.pandoc.to'),
                                   signif.stars = TRUE,
                              digits = 3, decimal.mark = getOption("OutDec"),
                              booktabs = TRUE,
                              summary = TRUE,
                              likelihood = TRUE,
                              arima = TRUE,
                              regression = TRUE, ...){
  if(is.null(format))
    format <- "latex"
  # if(identical(format, "latex")){
  #   print_preprocessing_latex(x$regarima, signif.stars = signif.stars,
  #                             digits = digits, decimal.mark = decimal.mark,
  #                             booktabs = booktabs, summary = summary,
  #                             likelihood = likelihood,
  #                             arima = arima, regression = regression, ...)
  # }
  # if(identical(format, "html")){
  #   print_preprocessing_html(x$regarima, signif.stars = signif.stars,
  #                            digits = digits, decimal.mark = decimal.mark,
  #                            summary = summary,
  #                            likelihood = likelihood,
  #                            arima = arima, regression = regression, ...)
  # }
  print_preprocessing(x$regarima, format = format, signif.stars = signif.stars,
                      digits = digits, decimal.mark = decimal.mark,
                      summary = summary,
                      likelihood = likelihood,
                      arima = arima, regression = regression, ...)
}
#' @export
print_preprocessing.jSA <- function(x, format = knitr::opts_knit$get('rmarkdown.pandoc.to'),
                                    signif.stars = TRUE,
                                    digits = 3, decimal.mark = getOption("OutDec"),
                                    booktabs = TRUE,
                                    summary = TRUE,
                                    likelihood = TRUE,
                                    arima = TRUE,
                                    regression = TRUE, ...){
  if(is.null(format))
    format <- "latex"
  x <- RJDemetra::jSA2R(x)
  print_preprocessing(x, format = format, signif.stars = signif.stars,
                      digits = digits, decimal.mark = decimal.mark,
                      summary = summary,
                      likelihood = likelihood,
                      arima = arima, regression = regression, ...)
}
#' @export
print_preprocessing.regarima <- function(x, format = knitr::opts_knit$get('rmarkdown.pandoc.to'),
                                         signif.stars = TRUE,
                                    digits = 3, decimal.mark = getOption("OutDec"),
                                    booktabs = TRUE,
                                    summary = TRUE,
                                    likelihood = TRUE,
                                    arima = TRUE,
                                    regression = TRUE, ...){
  if(is.null(format))
    format <- "latex"
  if(identical(format, "latex")){
    print_preprocessing_latex(x, signif.stars = signif.stars,
                              digits = digits, decimal.mark = decimal.mark,
                         booktabs = booktabs, summary = summary,
                         likelihood = likelihood,
                         arima = arima, regression = regression, ...)
  }
  if(identical(format, "html")){
    print_preprocessing_html(x, signif.stars = signif.stars,
                             digits = digits, decimal.mark = decimal.mark,
                        summary = summary,
                        likelihood = likelihood,
                        arima = arima, regression = regression, ...)
  }
}

print_preprocessing_latex <- function(x, signif.stars = TRUE,
                                      digits = 3, decimal.mark = getOption("OutDec"),
                            booktabs = TRUE,
                            summary = TRUE,
                            likelihood = TRUE,
                            arima = TRUE,
                            regression = TRUE, ...){
  knitr.kable.NA <- getOption("knitr.kable.NA")
  options(knitr.kable.NA = '')
  on.exit(options(knitr.kable.NA = knitr.kable.NA))
  
  summary_x <- summary(x)
  model <- ifelse(inherits(x, "TRAMO_SEATS"), "Tramo", "RegArima")
  cat(sprintf("\\underline{\\textbf{Pre-processing (%s)}}",model))
  cat("\n\n")
  
  ##################
  ## summary bloc ##
  if(summary){
    cat(title("Summary", format = "latex"))
    cat("\n\n")
    est_span <- summary_x$results_spec[1,"T.span"]
    cat("\n\n")
    
    nobs <- length(time(x$model$effects))
    cat(nobs, "observations")
    cat("\n\n")
    
    if(summary_x$results_spec[,"Log transformation"]){
      cat("Series has been log-transformed\n\n")
    }
    
    nb_td_var <- summary_x$results_spec[1,"Trading days"]
    if(nb_td_var != 0) {
      cat(sprintf("Trading days effect (%s variable%s)",nb_td_var,
                  ifelse(nb_td_var > 1,"s","")))
      cat("\n\n")
    }
    
    if(summary_x$results_spec[1,"Easter"]){
      cat(grep("^Easter \\[\\d+\\]$",rownames(x$regression.coefficients),
               value = TRUE)[1],
          "detected")
      cat("\n\n")
    }
    
    n_pre_out <- ifelse(identical(x$specification$regression$userdef$outliers,NA),
                        0,
                        nrow(x$specification$regression$userdef$outliers))
    if(n_pre_out > 0){
      cat(sprintf("%i pre-specified outlier%s\n\n",n_pre_out,
                  ifelse(n_pre_out > 1,"s","")))
    }
    
    n_out <- summary_x$results_spec[,"Outliers"] - n_pre_out
    if(n_out > 0){
      cat(sprintf("%i detected outlier%s\n\n", n_out,
                  ifelse(n_out > 1,"s","")))
    }
  }
  
  ## End of summary bloc ##
  #########################
  
  #####################
  ## Likelihood bloc ##
  if(likelihood){
    cat(title("Likelihood statistics", format = "latex"))
    cat("\n\n")
    
    cat(sprintf("Number of effective observations = %s\n\n", x$loglik["neffectiveobs",]))
    cat(sprintf("Number of estimated parameters = %s\n\n", x$loglik["np",]))
    
    format_numbers <- formatC(c(x$loglik[c("logvalue", "aicc", "bicc"),],
                                summary_x$residuals_st_err),
                              digits = digits,
                              decimal.mark = decimal.mark,
                              format = "f")
    cat(sprintf("Loglikelihood = %s, AICc = %s, BICc = %s\n\n",
                format_numbers[1],
                format_numbers[2],
                format_numbers[3]))
    cat(sprintf("Standard error of the regression (ML estimate) = %s\n\n",
                format_numbers[4]))
  }
  
  
  ## End of Likelihood bloc ##
  ############################
  
  ################
  ## ARIMA bloc ##
  
  if(arima){
    cat(title("ARIMA model", format = "latex"))
    cat("\n\n")
    arima_model <- sprintf("ARIMA (%s)(%s)",
                           paste(summary_x$arma_orders[c("p", "d", "q")],collapse = ","),
                           paste(summary_x$arma_orders[c("bp", "bd", "bq")],collapse = ","))
    arima_coef <- format_table_coefficient(summary_x$coefficients$arima,
                                           format = "latex")
    if(!is.null(arima_coef)){
      if (signif.stars) {
        arima_coef <- add_stars(arima_coef)
        arima_model <- c(footnote_stars(format = "latex"),
                         arima_model)
      }
      table <- kable(arima_coef, format = "latex", digits = digits,
                     escape = FALSE,
                     caption = "ARIMA coefficients",
                     format.args = list(decimal.mark = decimal.mark),
                     booktabs = booktabs,
                     align = "c") %>% 
        kable_styling(latex_options = "HOLD_position") %>% 
        footnote(general = arima_model, general_title = "",
                 escape = FALSE)
      cat(table)
    }else{
      cat(arima_coef)
    }
    cat("\n\n")
  }
  
  ## End ARIMA bloc ##
  ####################

  #####################
  ## Regression bloc ##
  if(regression){
    if(!all(sapply(summary_x$coefficients[c("regression", "fixed_out", "fixed_var")], 
                   is.null))){
      cat(title("Regression model", format = "latex"))
      cat("\n\n")

      regression_table <- rbind(format_table_coefficient(summary_x$coefficients$regression, format = "latex"),
                                format_table_coefficient(summary_x$coefficients$fixed_out, format = "latex"),
                                format_table_coefficient(summary_x$coefficients$fixed_var, format = "latex")
      )
      if(!is.null(regression_table)){
        if(signif.stars){
          regression_table <- add_stars(regression_table)
        }
        table <- kable(regression_table, format = "latex", digits = digits,
                       escape = "FALSE",
                       caption = "Regression coefficientss",
                       format.args = list(decimal.mark = decimal.mark),
                       booktabs = booktabs,
                       align = "c") %>% 
          kable_styling(latex_options = "HOLD_position")
        if (signif.stars) {
          table <- table %>% 
            footnote(general = footnote_stars(format = "latex"),
                     general_title = "",
                     escape = FALSE)
        }
        cat(table)
      }
    }
  }
  cat("\n\n")
  return(invisible(x))
}
print_preprocessing_html <- function(x, signif.stars = TRUE,
                                     digits = 3, decimal.mark = getOption("OutDec"),
                                 booktabs = TRUE,
                                 summary = TRUE,
                                 likelihood = TRUE,
                                 arima = TRUE,
                                 regression = TRUE, ...){
  knitr.kable.NA <- getOption("knitr.kable.NA")
  options(knitr.kable.NA = '')
  on.exit(options(knitr.kable.NA = knitr.kable.NA))
  
  summary_x <- summary(x)
  model <- ifelse(inherits(x, "TRAMO_SEATS"), "Tramo", "RegArima")
  cat(sprintf("<u><b>Pre-processing (%s)</b> </u>", model))
  cat("\n\n")
  
  ##################
  ## summary bloc ##
  if(summary){
    cat(title("Summary", format = "html"))
    cat("\n\n")
    est_span <- summary_x$results_spec[1,"T.span"]
    cat("\n\n")
    
    nobs <- length(time(x$model$effects))
    cat(nobs, "observations")
    cat("\n\n")
    
    if(summary_x$results_spec[,"Log transformation"]){
      cat("Series has been log-transformed\n\n")
    }
    
    nb_td_var <- summary_x$results_spec[1,"Trading days"]
    if(nb_td_var != 0) {
      cat(sprintf("Trading days effect (%s variable%s)",nb_td_var,
                  ifelse(nb_td_var > 1,"s","")))
      cat("\n\n")
    }
    
    if(summary_x$results_spec[1,"Easter"]){
      cat(grep("^Easter \\[\\d+\\]$",rownames(x$regression.coefficients),
               value = TRUE)[1],
          "detected")
      cat("\n\n")
    }
    
    n_pre_out <- ifelse(identical(x$specification$regression$userdef$outliers,NA),
                        0,
                        nrow(x$specification$regression$userdef$outliers))
    if(n_pre_out > 0){
      cat(sprintf("%i pre-specified outlier%s\n\n",n_pre_out,
                  ifelse(n_pre_out > 1,"s","")))
    }
    
    n_out <- summary_x$results_spec[,"Outliers"] - n_pre_out
    if(n_out > 0){
      cat(sprintf("%i detected outlier%s\n\n", n_out,
                  ifelse(n_out > 1,"s","")))
    }
  }
  
  ## End of summary bloc ##
  #########################
  
  #####################
  ## Likelihood bloc ##
  if(likelihood){
    cat(title("Likelihood statistics", format = "html"))
    cat("\n\n")
    
    cat(sprintf("Number of effective observations = %s\n\n", x$loglik["neffectiveobs",]))
    cat(sprintf("Number of estimated parameters = %s\n\n", x$loglik["np",]))
    
    format_numbers <- formatC(c(x$loglik[c("logvalue", "aicc", "bicc"),],
                                summary_x$residuals_st_err),
                              digits = digits,
                              decimal.mark = decimal.mark,
                              format = "f")
    cat(sprintf("Loglikelihood = %s, AICc = %s, BICc = %s\n\n",
                format_numbers[1],
                format_numbers[2],
                format_numbers[3]))
    cat(sprintf("Standard error of the regression (ML estimate) = %s\n\n",
                format_numbers[4]))
  }
  
  
  ## End of Likelihood bloc ##
  ############################
  
  ################
  ## ARIMA bloc ##
  
  if(arima){
    cat(title("ARIMA model", format = "html"))
    cat("\n\n")
    arima_model <- sprintf("ARIMA (%s)(%s)",
                           paste(summary_x$arma_orders[c("p", "d", "q")],collapse = ","),
                           paste(summary_x$arma_orders[c("bp", "bd", "bq")],collapse = ","))
    arima_coef <- format_table_coefficient(summary_x$coefficients$arima,
                                           format = "html")
    if(!is.null(arima_coef)){
      if (signif.stars) {
        arima_coef <- add_stars(arima_coef)
        arima_model <- c(footnote_stars(format = "html"),
                         arima_model)
      }
      table <- kable(arima_coef, format = "html", digits = digits,
                     escape = FALSE,
                     caption = "ARIMA coefficients",
                     format.args = list(decimal.mark = decimal.mark),
                     align = "c") %>% 
        kable_styling() %>% 
        footnote(general = arima_model, general_title = "",
                 escape = FALSE)
      cat(table)
    }else{
      cat(arima_coef)
    }
    cat("\n\n")
  }
  
  ## End ARIMA bloc ##
  ####################
  
  #####################
  ## Regression bloc ##
  if(regression){
    if(!all(sapply(summary_x$coefficients[c("regression", "fixed_out", "fixed_var")], 
                   is.null))){
      cat(title("Regression model", format = "html"))
      cat("\n\n")
      
      regression_table <- rbind(format_table_coefficient(summary_x$coefficients$regression, format = "html"),
                                format_table_coefficient(summary_x$coefficients$fixed_out, format = "html"),
                                format_table_coefficient(summary_x$coefficients$fixed_var, format = "html")
      )
      if(!is.null(regression_table)){
        if(signif.stars){
          regression_table <- add_stars(regression_table)
        }
        table <- kable(regression_table, format = "html", digits = digits,
                       escape = FALSE,
                       caption = "Regression coefficients",
                       format.args = list(decimal.mark = decimal.mark),
                       align = "c") %>% 
          kable_styling()
        if (signif.stars) {
          table <- table %>% 
            footnote(general = footnote_stars(format = "html"),
                     general_title = "",
                     escape = FALSE)
        }
        cat(table)
      }
    }
  }
  cat("\n\n")
  return(invisible(x))
}
format_table_coefficient <- function(x, format = "latex"){
  if(is.null(x))
    return(x)
  if(ncol(x) == 2){
    x$new_col1 <- x$new_col2 <- NA
    x <- x[,c(1,3,4,2)]
  }
  if(format == "latex"){
    colnames(x) <- c("Coefficients", "Std. Error",
                     "T-stat", "$\\mathbb P (> \\lvert t \\rvert)$")
  }
  if(format == "html"){
    colnames(x) <- c("Coefficients", "Std. Error",
                     "T-stat", "P (> | t|)")
  }
  return(invisible(x))
}
