alpha.div <- function(x, method = "simpson"){
  if(is.data.frame(x)) rn <- rownames(x) else {
    if(ncol(as.matrix(x) == 1)) rn = noquote("") else rn = 1:nrow(as.matrix(x))
  }
  indices <- c("simpson", "shannon"); method <- match.arg(method, indices)
  x <- as.matrix(x)
  prop <- function(x){
    if(ncol(x) == 1) out <- x/sum(x)
    else
      out <- apply(x, 1, function(x) x/sum(x))
    out
  }
  p.i <- prop(x)
    simp <- function(x, p.i){
    if(ncol(x) == 1) D <- 1 - sum(p.i^2)
    else
      D <- 1 - apply(p.i^2, 2, sum)
    D
  }
  shan <- function(x, p.i){
    if(ncol(x) == 1) H <- -sum(p.i[p.i > 0] * log(p.i[p.i > 0]))
    else
      H <- apply(p.i, 2, function(x)-sum(x[x != 0] * log(x[x != 0])))
    H
  }
  div <- switch(method,
                simpson = simp(x, p.i),
                shannon = shan(x, p.i))
  out <- list(p.i = p.i, rn = rn, method = method, div = div)
  class(out) <- "a_div"; invisible(out)
}
