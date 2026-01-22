print.a_div <- function(x, digits = 5, ...){
  method <- ifelse(x$method == "simpson", "Simpson",
                   "Shannon-Weiner")
  cat(method, " diversity:", "\n", sep = "")
  rq <- structure(x$div,  names = x$rn)
  print(rq, digits = digits)
  invisible(x)
}
