## Fig.5.3
samp.dist.snap(parent = expression(rexp(s.size)), stat = mean, 
s.size = c(1, 3, 7, 10, 20, 50),  R = 10000, func = NULL, xlab = expression(italic(bar(x))), 
show.SE = TRUE, fits = NULL, show.fits = TRUE, xlim = NULL, ylim = NULL, cex.lab=1.2, cex.axis = 1.2)

## Fig.5.4
s.sq <- function(s.dist, s.dist2, s.size, s.size2, sigma.sq = 1)((s.size - 1) * s.dist)/sigma.sq
ft <- function(s.size, s.size2)curve(dchisq(x, s.size - 1),from = 0, to = 40, add = TRUE, lwd = 2, col = gray(.3))

samp.dist.snap(parent = expression(rnorm(s.size)), stat = var, 
s.size = c(3, 7, 10, 20),  R = 10000, func = s.sq, xlab = expression((italic(n) - 1)*italic(S)^2/sigma^2), 
show.SE = TRUE, fits = ft, show.fits = TRUE, xlim = c(0,40), ylim = NULL, cex.lab=1.1, cex.axis = 1.1)

## Fig.5.5
t.star <- function(s.dist, s.dist3, s.size, s.size2)s.dist/sqrt(s.dist3/s.size)
ft <- function(s.size, s.size2){curve(dnorm(x),from = -10, to = 10, add = TRUE, lty = 2, lwd = 2, col = gray(.3)); curve(dt(x, s.size-1),from = -10,to = 10, add = TRUE, lty = 1, lwd = 2, col = gray(.6))}

samp.dist.snap(parent = expression(rnorm(s.size)), stat = mean, stat3 = var, 
s.size = c(3, 7, 10, 20),  R = 10000, func = t.star, xlab = expression(paste(italic(t),"*")), 
show.SE = TRUE, fits = ft, show.fits = TRUE, xlim = c(-5,5), ylim = c(0,0.5), cex.lab=1.1, cex.axis = 1.1)


## Fig.5.6
F.star <- function(s.dist,s.dist2,s.size, s.size2) s.dist/s.dist2

samp.dist(parent = expression(rnorm(s.size)), parent2 = expression(rnorm(s.size)), stat = var, stat2 = var, 
R = 2000, func = F.star, xlab = expression(paste(italic(F),"*")), s.size =8, s.size2 = 10,
show.SE = TRUE, cex.lab=1.3, cex.axis = 1.3, col.anim="gray")

curve(df(x, 10, 8), from = 0, to = 20, add = TRUE, lwd = 2, col = gray(.4))

## Fig 5.7
anm.ci(parent=expression(rnorm(n)), par.val = 0, conf = 0.95, sigma = NULL, par.type = "mu", n.est = 100, n = 50, interval = 0.1, err.col = gray(.7), par.col = "gray", cex.lab=1.2,cex.axis=1.2) 

## Fig 5.8
par(mar=c(5,4.5,2,2))
shade.norm(from=12.64,to=16.56,sigma=1,mu=14.6,tail="middle",legend.cex=1.2,cex.lab=1.3,cex.axis=1.3)

## Fig 5.9
par(cex = 1.5, mar=c(2,5,2,2))
bplot(y = pika[,2], x = rep(1, 21), int = "CI", conf = .95, names.arg = "", xlab="", ylab = "On haypile %N - Off haypile %N", names = "")    

Fig.5.10 <- function(){
theta <- seq(0.4, 0.6, by = .001)#support

pdfFonts(stdsym=Type1Font("standardsymbol",
            c("Helvetica.afm",
              "s050000l.afm",
              "Helvetica-Oblique.afm",
              "Helvetica-Bold.afm",
              "Symbol.afm"),
            encoding="CP1253"))

tri.pr <- function(x){
prior <- matrix(ncol =1 ,nrow = length(x))
for(i in 1: length(x)){
if(x[i] < 0.4) prior[i] = 0
if(x[i] > 0.6) prior[i] = 0
if(x[i] >= 0.4 & x[i] < 0.6) prior[i] = 5 * x[i] - 2
if(x[i] <= 0.6 & x[i] > 0.5) prior[i] = -5 * x[i] + 3
}
prior
}

prior <- tri.pr(theta)
lk <- theta^512*(1-theta)^(1000-512)
post <- prior * lk

windows(height=3.5)
par(mar=c(5, 4.5, 2, 1), mfrow = c(1, 2))
#plot a
plot(0:1, ylim = c(0, 0.55), xlim = c(.3, .7), type = "n", ylab = expression(paste(italic(f),"(" ,theta, ")")), xlab = "", cex.lab =1.2)
mtext(side=1, "\u03B8", font = 3, family = "stdsym", line =3, cex = 1.2)
segments(c(.5,  .5, .3, .6), c(.5, .5, 0, 0), c(.6, .4, .4, .7), c(0, 0, 0, 0))

#plot b
plot(theta, post, type="h", xlim = c(0.3, 0.7), ylab = expression(paste(""%prop%"", italic(f),"(" ,theta, " | data )")), xlab = "", cex.lab =1.2)
mtext(side=1, "\u03B8", font = 3, family = "stdsym", line = 3, cex = 1.2)
mtext("(a)", 3, at = -.47, line = .7, cex = 1.6)
mtext("(b)", 3,  at = 0.16, line = .7, cex = 1.6)
}


#Fig.5.11

anm.mc.bvn(start = c(-4, -4), mu = c(0, 0), sigma = matrix(2, 2, data = c(1, 0,
 0, 1)), length = 1000, sim = "G", jump.kernel = 0.2, xlim = c(-4, 4),
 ylim = c(-4, 4), interval = 0.01, show.leg = TRUE, cex.lab=1.3,cex.axis=1.3,cex.main=1.4,cex.leg=1.2)