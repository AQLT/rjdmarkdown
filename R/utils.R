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