#' Print the diagnostics
#' 
#' Function to print diagnostics tests on the quality of the pre-processing and the decomposition
#' 
#' @inheritParams print_preprocessing
#' @param tests characters containing the names of the tests to print.
#' @param ... unused arguments.
#' 
#' @examples 
#' ipi <- RJDemetra::ipi_c_eu[, "FR"]
#' 
#' jsa_x13 <- RJDemetra::jx13(ipi)
#' print_diagnostics(jsa_x13, format = "latex")
#' 
#' \donttest{
#' sa_ts <- RJDemetra::tramoseats(ipi)
#' print_diagnostics(sa_ts, format = "html")
#' }
#' @export
print_diagnostics <- function(x, format = knitr::opts_knit$get('rmarkdown.pandoc.to'),
                              signif.stars = TRUE,
                              tests = c("mean", "skewness", "kurtosis",
                                        "ljung box",
                                        "ljung box (residuals at seasonal lags)", 
                                        "ljung box (squared residuals)",
                                        "qs test on sa", "qs test on i",
                                        "f-test on sa (seasonal dummies)", 
                                        "f-test on i (seasonal dummies)",
                                        "Residual seasonality (entire series)", 
                                        "Residual seasonality (last 3 years)",
                                        "f-test on sa (td)", "f-test on i (td)"),
                              digits = 3, decimal.mark = getOption("OutDec"),
                              booktabs = TRUE, ...){
  UseMethod("print_diagnostics", x)
}
#' @export
print_diagnostics.jSA <- function(x, format = knitr::opts_knit$get('rmarkdown.pandoc.to'),
                                 signif.stars = TRUE,
                                 tests = c("mean", "skewness", "kurtosis",
                                           "ljung box",
                                           "ljung box (residuals at seasonal lags)", 
                                           "ljung box (squared residuals)",
                                           "qs test on sa", "qs test on i",
                                           "f-test on sa (seasonal dummies)", 
                                           "f-test on i (seasonal dummies)",
                                           "Residual seasonality (entire series)", 
                                           "Residual seasonality (last 3 years)",
                                           "f-test on sa (td)", "f-test on i (td)"),
                                 digits = 3, decimal.mark = getOption("OutDec"),
                                 booktabs = TRUE, ...){
  if(is.null(format))
    format <- "latex"
  x <- RJDemetra::jSA2R(x)
  print_diagnostics(x, format = format,
                    signif.stars = signif.stars,
                    tests = tests, digits = digits, decimal.mark = decimal.mark,
                    booktabs = booktabs)
}
#' @export
print_diagnostics.regarima <- function(x, format = knitr::opts_knit$get('rmarkdown.pandoc.to'),
                                  signif.stars = TRUE,
                                  tests = c("mean", "skewness", "kurtosis",
                                            "ljung box",
                                            "ljung box (residuals at seasonal lags)", 
                                            "ljung box (squared residuals)",
                                            "qs test on sa", "qs test on i",
                                            "f-test on sa (seasonal dummies)", 
                                            "f-test on i (seasonal dummies)",
                                            "Residual seasonality (entire series)", 
                                            "Residual seasonality (last 3 years)",
                                            "f-test on sa (td)", "f-test on i (td)"),
                                  digits = 3, decimal.mark = getOption("OutDec"),
                                  booktabs = TRUE, ...){
  if(is.null(format))
    format <- "latex"
  print_diagnostics.SA(x, format = format,
                       signif.stars = signif.stars,
                       tests = tests, digits = digits, decimal.mark = decimal.mark,
                       booktabs = booktabs)
}
#' @export
print_diagnostics.SA <- function(x, format = knitr::opts_knit$get('rmarkdown.pandoc.to'),
                                 signif.stars = TRUE,
                                 tests = c("mean", "skewness", "kurtosis",
                                           "ljung box",
                                           "ljung box (residuals at seasonal lags)", 
                                           "ljung box (squared residuals)",
                                           "qs test on sa", "qs test on i",
                                           "f-test on sa (seasonal dummies)", 
                                           "f-test on i (seasonal dummies)",
                                           "Residual seasonality (entire series)", 
                                           "Residual seasonality (last 3 years)",
                                           "f-test on sa (td)", "f-test on i (td)"),
                                 digits = 3, decimal.mark = getOption("OutDec"),
                                 booktabs = TRUE, ...){
  if(is.null(format))
    format <- "latex"
  tests_table <- get_tests(x, tests = tests)
  if(nrow(tests_table) == 0)
    return(NULL)
  if (signif.stars) {
    tests_table <- add_stars(tests_table)
  }
  table <- tests_table %>% 
    format_table_tests(format = format) %>% 
    kable(format = format, digits = digits,
          escape = FALSE,
          caption = "Diagnostics tests",
          format.args = list(decimal.mark = decimal.mark),
          align = "c") %>% 
    kable_styling(latex_options = "HOLD_position")
  if (signif.stars) {
    table <- table %>% 
      footnote(general = footnote_stars(format = format),
               general_title = "",
               escape = FALSE)
  }
  cat(table)
}
get_tests <- function(x,
                      tests = c("mean", "skewness", "kurtosis",
                                "ljung box",
                                "ljung box (residuals at seasonal lags)", 
                                "ljung box (squared residuals)",
                                "qs test on sa", "qs test on i",
                                "f-test on sa (seasonal dummies)", 
                                "f-test on i (seasonal dummies)",
                                "Residual seasonality (entire series)", 
                                "Residual seasonality (last 3 years)",
                                "f-test on sa (td)", "f-test on i (td)")
){
  tests <- match.arg(tests, several.ok = TRUE)
  if (inherits(x, "SA")) {
    tests_table <- data.frame(rbind(x$regarima$residuals.stat$tests, 
                                    x$diagnostics$residuals_test))
  } else {
    tests_table <- data.frame(x$residuals.stat$tests)
  }
  as.matrix(tests_table[rownames(tests_table) %in% tests, 2, drop = FALSE])
}
format_table_tests <- function(x, format = "latex"){
  if(is.null(x))
    return(x)
  if(format == "latex"){
    colnames(x)[1] <- "$\\mathbb P (> \\lvert t \\rvert)$"
  }
  if(format == "html"){
    colnames(x)[1] <- "P (> | t|)"
  }
  return(invisible(x))
}
