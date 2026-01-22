# Fig. 11.2
par(cex=1.1, mar=c(5,4.3,1,1))
ci.p(c(rep(1, 18),rep(0,32)), method = "LR")

# Fig. 11.4
par(cex=1.1, mar=c(5,4.3,1,1))
data(whickham)
paik(survival ~ smoke + age, counts = count, data = whickham, xlab = "Smoke", leg.title = "Age", ylab = "Proportion surviving")
