#' Print the decomposition
#'
#' @inheritParams print_preprocessing
#' 
#' @importFrom kableExtra column_spec
#' @export
print_decomposition <- function(x, format = "latex",
                                plot = TRUE,
                                digits = 3, decimal.mark = getOption("OutDec"),
                                booktabs = TRUE, ...){
  UseMethod("print_decomposition", x)
}
#' @export
print_decomposition.SA <- function(x, format = "latex",
                                   plot = TRUE,
                                   digits = 3, decimal.mark = getOption("OutDec"),
                                   booktabs = TRUE, ...){
  print_decomposition(x$decomposition, format = format,
                      plot = plot,
                      digits = digits, decimal.mark = decimal.mark,
                      booktabs = booktabs, ...)
}
#' @export
print_decomposition.decomposition_X11 <- function(x, format = "latex",
                                                  plot = TRUE,
                                         digits = 3, decimal.mark = getOption("OutDec"),
                                         booktabs = TRUE, ...){

  legend_mstats <- data.frame(Description = c("The relative contribution of the irregular over three months span", 
                                              "The relative contribution of the irregular component to the stationary portion of the variance", 
                                              "The amount of period to period change in the irregular component as compared to the amount of period to period change in the trend", 
                                              "The amount of autocorrelation in the irregular as described by the average duration of run", 
                                              "The number of periods it takes the change in the trend to surpass the amount of change in the irregular", 
                                              "The amount of year to year change in the irregular as compared to the amount of year to year change in the seasonal", 
                                              "The amount of moving seasonality present relative to the amount of stable seasonality", 
                                              "The size of the fluctuations in the seasonal component throughout the whole series", 
                                              "The average linear movement in the seasonal component throughout the whole series", 
                                              "The size of the fluctuations in the seasonal component in the recent years", 
                                              "The average linear movement in the seasonal component in the recent years", "", ""
  ), `Mstat` = c("M(1)", "M(2)", "M(3)", "M(4)", "M(5)", "M(6)", 
                  "M(7)", "M(8)", "M(9)", "M(10)", "M(11)", "Q", "Q-M2"),
  stringsAsFactors = FALSE)
  m_stats <- data.frame(`Mstat` = rownames(x$mstats),
                        Value = as.numeric(x$mstats),
                        stringsAsFactors = FALSE)
  m_stats <- merge(m_stats, legend_mstats, sort = FALSE)
  rownames(m_stats) <- gsub(")", "",
                            gsub("(", "-", m_stats[,1], fixed = TRUE),
                            fixed = TRUE)
  m_stats <- m_stats[,-1]

  cat(title("Decomposition (X-11)", format = format))
  cat("\n\n")
  cat(sprintf("Mode: %s", tolower(x$mode)))
  cat("\n\n")
  
  if (plot) {
    plot(x, ...)
    cat("\n\n")
  }
  # if (plot) {
  #   cat(sprintf("plot(x, main = %s)",plot.title))
  #   cat("\n\n")
  # }
  
  if (identical(format, "latex")) {
    filters <- sprintf("\\\\textbf{Final filters}: M%s, Henderson-%s terms",
                       x$s_filter, gsub("\\D","",x$t_filter))
    table <- kable(m_stats, format = "latex", digits = digits,
                   escape = FALSE,
                   caption = "M-statistics",
                   format.args = list(decimal.mark = decimal.mark),
                   booktabs = booktabs,
                   align = c("c", "l")) %>% 
      kable_styling(latex_options = "HOLD_position") %>%
      column_spec(3, width = "0.7\\\\textwidth") %>% 
      footnote(general = filters, general_title = "", escape = FALSE)
    cat(table)
  }
  if(identical(format, "html")){
    filters <- sprintf("<b>Final filters</b>: M%s, Henderson-%s terms",
                       x$s_filter, gsub("\\D","",x$t_filter))
    table <- kable(m_stats, format = "html", digits = digits,
                   escape = FALSE,
                   caption = "M-statistics",
                   format.args = list(decimal.mark = decimal.mark),
                   align = c("c", "l")) %>% 
      column_spec(1, width_min = "1cm") %>% 
      kable_styling() %>% 
      footnote(general = filters, general_title = "", escape = FALSE)
    cat(table)
  }
}
#' @export
print_decomposition.decomposition_SEATS <- function(x, format = "latex",
                                                    plot = TRUE, 
                                                  digits = 2,
                                                  decimal.mark = getOption("OutDec"),
                                                  booktabs = TRUE, ...){
  
  
  cat(title("Decomposition (SEATS)", format = format))
  cat("\n\n")
  cat(sprintf("Mode: %s", tolower(x$mode)))
  cat("\n\n")
  
  if (plot) {
    plot(x, ...)
    cat("\n\n")
  }
  
  var <- x$model
  var_names <- bold(c("Model","SA","Trend","Seasonal","Transitory","Irregular"),
                    format = format)
  
  for (ii in 1:length(var_names)){
    if (!all(sapply(var[[ii]],is.null))){
      cat(var_names[ii],"\n\n", sep="")
      
      print_formula(x = var[[ii]][1,-1],"AR",
                    digits = digits,
                    decimal.mark = decimal.mark)
      cat("\n\n")
      print_formula(x = var[[ii]][2,-1],"D",
                    digits = digits,
                    decimal.mark = decimal.mark)
      cat("\n\n")
      print_formula(x = var[[ii]][3,-1],"MA",
                    digits = digits,
                    decimal.mark = decimal.mark)
      cat("\n\n")
      
      if (var[[ii]][4,1]==1) {
        cat("\n\n")
      }else{
        cat("Innovation variance: ",
            formatC(var[[ii]][4,1],
                    digits = digits,
                    decimal.mark = decimal.mark,
                    format = "f"), "\n\n")
      }
    }
  }
  
}
print_formula <- function(x, var, digits = 2, decimal.mark = getOption("OutDec")){
  non_0 <- which(x != 0 )
  if(length(non_0) == 0)
    return(NULL)
  coefs <- formatC(x,
          digits = digits,
          decimal.mark = decimal.mark,
          format = "f")
  polynome_degre <- sprintf("B^{%s}", non_0)
  polynome_degre[non_0 == 1] <- "B"
  
  polynome_coef <- coefs[non_0]
  # polynome_coef <- gsub("-","- ", polynome_coef)
  # polynome_coef <- gsub("\\+","+ ", polynome_coef)
  polynome_coef[polynome_coef > 0] <- sprintf("+%s", polynome_coef[polynome_coef > 0])
  polynome_coef[x[non_0] == -1] <- "-"
  polynome_coef[x[non_0] == 1] <- "+"
  
  polynome_formula <- paste0(polynome_coef, polynome_degre, collapse = "") 
  polynome_formula <- paste0("$1", polynome_formula,"$")
  cat(var,": ",polynome_formula,sep = "")
}
