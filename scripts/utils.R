# Capitalize the first letter of a string
capitalize <- function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

# Like tryCatch, only useful.
tryFail <- function(expr, default) {
  tryCatch(expr, warning=function(w) { default }, error=function(e) { default })
}
