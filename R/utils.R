title <- function(x, format){
  underline(bold(x, format = format), format = format)
}
bold <- function(x, format){
  if(identical(format, "latex")){
    res <- sprintf("\\textbf{%s}", x)
  }else if(identical(format, "html")){
    res <- sprintf("<b>%s</b>", x)
  }else{
    res <- x
  }
  res
}
underline <- function(x, format){
  if(identical(format, "latex")){
    res <- sprintf("\\underline{%s}", x)
  }else if(identical(format, "html")){
    res <- sprintf("<u>%s</u>", x)
  }else{
    res <- x
  }
  res
}

add_stars <- function(x, p_value = ncol(x),
                      decimal.mark = getOption("OutDec"),
                      digits = 3){
  stars <- cut(x[, p_value],
               breaks = c(0, 0.001, 0.01, 0.05, 0.1, 1),
               include.lowest = TRUE,
               labels = c("***","**","*",".",""))
  table <- cbind(formatC(x, digits = digits, decimal.mark = decimal.mark,
                         format = "f"),
                 as.character(stars))
  colnames(table) <- c(colnames(x), "")
  table
}
footnote_stars <- function(format = "latex"){
  if (format == "latex"){
    paste0("\\",
           bold("Signif. codes: ", format = "latex"),
           "0 `***' 0.001 `**' 0.01 `*' 0.05 `.' 0.1 ` ' 1")
  } else {
    paste(bold("Signif. codes:", format = "html"),
          "0 '\\*\\*\\*' 0.001 '\\*\\*' 0.01 '\\*' 0.05 '.' 0.1 ' ' 1")
  }
}
