#' @export
print_latex <- function(x, ...){
  UseMethod("print_latex", x)
}
#' @export
print_latex.SA <- function(x, ...){
  NextMethod("print_latex", x$regarima)
}
#' @export
print_latex.regarima <- function(x, ...){
  NextMethod("print_latex", x)
}
#' @export
print_latex.X13 <- function(x, digits = 3, decimal.mark = getOption("OutDec"),
                            booktabs = TRUE, ...){
  summary_x <- summary(x)
  
  ##################
  ## summary bloc ##
  cat("\\underline{\\textbf{Summary}}")
  cat("\n\n")
  est_span <- summary_x$results_spec[1,"T.span"]
  cat("\n\n")
  
  nobs <- length(time(x$model$effects))
  cat(nobs, "observations")
  cat("\n\n")
  
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

  
  ## End of summary bloc ##
  #########################
  
  #####################
  ## Likelihood bloc ##
  cat("\\underline{\\textbf{Likelihood statistics}}\n\n")
  cat(sprintf("Number of effective observations = %s\n\n", x$loglik["neffectiveobs",]))
  cat(sprintf("Number of estimated parameters = %s\n\n", x$loglik["np",]))
  
  cat(sprintf("Loglikelihood = %s\n\n",
              formatC(x$loglik["logvalue",],
                      digits = digits,
                      decimal.mark = decimal.mark,
                      format = "f")))
  cat(sprintf("AICc = %s\n\n",
              formatC(x$loglik["aicc",],
                      digits = digits,
                      decimal.mark = decimal.mark,
                      format = "f")))
  cat(sprintf("BICc = %s\n\n",
              formatC(x$loglik["bicc",],
                      digits = digits,
                      decimal.mark = decimal.mark,
                      format = "f")))
  cat(sprintf("Standard error of the regression (ML estimate) = %s\n\n",
              formatC(summary_x$residuals_st_err,
                      digits = digits,
                      decimal.mark = decimal.mark,
                      format = "f")))
  
  ## End of Likelihood bloc ##
  ############################
  
  ################
  ## ARIMA bloc ##
  options(knitr.kable.NA = '')
  cat("\\underline{\\textbf{ARIMA model}}\n\n")
  cat(sprintf("ARIMA (%s)(%s)\n",
              paste(summary_x$arma_orders[c("p", "d", "q")],collapse = ","),
              paste(summary_x$arma_orders[c("bp", "bd", "bq")],collapse = ","))
  )
  arima_coef <- format_table_coefficient(summary_x$coefficients$arima,
                                         format = "latex")
  if(!is.null(arima_coef)){
    cat(kableExtra::kable_styling(knitr::kable(arima_coef, format = "latex", digits = digits,
              escape = "FALSE",
              caption = "ARIMA coefficients",
              format.args = list(decimal.mark = decimal.mark),
              booktabs = booktabs,
              align = "c"),
              latex_options = "HOLD_position")
        )
  }
  cat("\n\n")
  ## End ARIMA bloc ##
  ####################

  #####################
  ## Regression bloc ##
  if(!all(sapply(summary_x$coefficients[c("regression", "fixed_out", "fixed_var")], 
                is.null))){
    cat("\\underline{\\textbf{Regression model}}\n\n")
    
    regression_table <- rbind(format_table_coefficient(summary_x$coefficients$regression),
                              format_table_coefficient(summary_x$coefficients$fixed_out),
                              format_table_coefficient(summary_x$coefficients$fixed_var)
                              )
    if(!is.null(regression_table)){
      
      cat(kableExtra::kable_styling(knitr::kable(regression_table, format = "latex", digits = digits,
                escape = "FALSE",
                caption = "ARIMA coefficients",
                format.args = list(decimal.mark = decimal.mark),
                booktab = TRUE,
                align = "c"),
                        latex_options = "HOLD_position")
          )
    }
    cat("\n")
    
  }
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
  
  return(invisible(x))
}
# 
# cat("y = regression model + arima ",gsub("c","",deparse(as.numeric(x$arma_orders))),sep="")
# cat("\n\n")
# cat("Model:",x$results_spec[1,"Model"],sep=" ")
# cat("\n")
# cat("Estimation span:",x$results_spec[1,"T.span"],sep=" ")
# cat("\n")
# cat("Log-transformation:",if(x$results_spec[1,"Log transformation"]) {"yes"} else {"no"},sep=" ")
# cat("\n")
# cat("Regression model:",if(x$results_spec[1,"Mean"]) {"mean"} else {"no mean"},sep=" ")
# }
# cat(if(x$results_spec[1,"Leap year"]) {", leap year effect"} else {", no leap year effect"},sep="")
# cat(if(x$results_spec[1,"Easter"]) {", Easter effect"} else {", no Easter effect"},sep="")
# if(x$results_spec[1,"Outliers"]==0) {cat(", no outliers")} else {cat(", outliers(",x$results_spec[1,"Outliers"],")",sep="")}
# cat("\n\n")
# cat("Coefficients:")
# 
# 
# if (!is.null(x$coefficients$arima)){
#   cat("\n")
#   cat("ARIMA:","\n")
#   printCoefmat(x$coefficients$arima, digits = digits, signif.stars = signif.stars,
#                na.print = "NA", ...)
# }
# if (!is.null(x$coefficients$regression)){
#   cat("\n")
#   cat("Regression model:","\n")
#   
#   printCoefmat(x$coefficients$regression, digits = digits, signif.stars = signif.stars,
#                na.print = "NA", ...)
# }
# if (!is.null(x$coefficients$fixed_out)){
#   printCoefmat(x$coefficients$fixed_out[, -ncol(x$coefficients$fixed_out), drop = FALSE],
#                digits = digits, P.values= FALSE, na.print = "NA")
# }
# if (!is.null(x$coefficients$fixed_var)){
#   cat("\n")
#   cat("Fixed other regression effects:","\n")
#   printCoefmat(x$coefficients$fixed_var[,-ncol(x$coefficients$fixed_var), drop = FALSE],
#                digits = digits, P.values= FALSE, na.print = "NA", ...)
# }
# 
# loglik <- x$loglik
# class(result) <- "summary.regarima"
# cat("\n\n")
# cat("Residual standard error:",
#     ,
#     "on",
#     loglik["np",], "degrees of freedom", sep = " ")
# cat("\n")
# cat("Log likelihood = ", formatC(loglik["logvalue",], digits = digits),
#     ", aic = ",formatC(loglik["aic", ], digits = digits),
#     ", aicc = ", formatC(loglik["aicc", ], digits = digits),
#     ", bic(corrected for length) = ", formatC(loglik["bicc", ],digits = digits),
#     sep = "")
# cat("\n\n")
# invisible(x)
# 
# digits <- 4
# decimal.mark = getOption("OutDec")
# myseries <- ipi_c_eu[, "FR"]
# 
# mysa <- x13(myseries, spec = x13_spec(usrdef.outliersEnabled = TRUE,
#                                       usrdef.outliersType = c("LS","AO"),
#                                       usrdef.outliersDate = c("2008-10-01", "2002-01-01")
#                                       ,
#                                       usrdef.outliersCoef = c(36, 14),
#                                       transform.function = "None", automdl.enabled =FALSE,
#                                       arima.coefEnabled = TRUE,
#                                       arima.p = 1, arima.q = 1, arima.bp = 0, arima.bq = 1,
#                                       arima.coef = c(-0.8, -0.6, 0),
#                                       arima.coefType = c(rep("Fixed", 2), "Undefined")))
# x <- mysa$regarima
# summary_x <- summary(x)
# 
# class(mysa)
# summary.regarima(x)
