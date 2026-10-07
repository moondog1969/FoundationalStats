#!/bin/env Rscript

arg <- commandArgs(trailingOnly = TRUE)

if (length(arg) == 0) {
  stop("Please provide base sequence, e.g., AGTCC...", call. = FALSE)
}

base.summary <- function(bases){
  ss <- unlist(strsplit(bases, NULL))
  sf <- factor(as.factor(ss), levels = c("A","C","G","T"))
  bs <- data.frame(tapply(ss, sf, length))
  bs[,1] <- ifelse(is.na(bs[,1]), 0, bs[,1])
  names(bs) <- "Percentage"
  round(bs/sum(bs) * 100, 1)
}

base.summary(arg)
