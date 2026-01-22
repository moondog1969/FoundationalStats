plot.a_div <- function(x, plot.RAC = FALSE, ...){
  margin_theme <- function(){
    theme(axis.title.x = element_text(vjust=-5),
          axis.title.y = element_text(vjust=5),
          plot.margin = margin(t = 7.5, r = 7.5,
                               b = 20, l = 15))
  }
  p.i <- Rank <- Site <- div <- NULL
  ptype1 <- function(){
    spi <- apply(x$p.i, 2, function(x)
    {sort(x, decreasing = TRUE)})
    sspi <- data.frame(p.i = stack(as.data.frame(spi))[,1])
    sspi$Rank <- rep(1:nrow(x$p.i), ncol(x$p.i))
    sspi$Site <- rep(x$rn,  each = nrow(x$p.i))
    ggplot(sspi, aes(y = p.i, x = Rank, group = Site)) +
      geom_line(aes(y = p.i, x = Rank, colour = Site),
                alpha = .4) +
      ylab(expression(italic(p[i]))) +
      theme_classic() + margin_theme()
  }

  ptype2 <- function(){
    diversity <- data.frame(div =  x$div,
                            Site = factor(x$rn))
    method <- ifelse(x$method == "simpson",
                     "Simpson diveristy",
                     "Shannon-Weiner diveristy")
    ggplot(diversity) +
      geom_bar(aes(y = div, x = Site, fill = div),
               show.legend = FALSE, stat = "identity") +
      theme_classic() +
      margin_theme() +
      ylab(method) + xlab("Site")
  }
  if(plot.RAC)  ptype1() else ptype2()
}
