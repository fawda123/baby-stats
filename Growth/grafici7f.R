#************************
#**Stature-for-age charts, 2 to 20 years, LMS parameters and selected smoothed stature percentiles in centimeters, by sex and age 
#************************

#	Load data
#	F
ccF <- get(load(here("Growth/data/mydata7f.rda")))

#	create dataframe for F with numeric
mydataF <- ccF
cols = c(1:ncol(mydataF))
mydataF[,cols] = apply(mydataF[,cols], 2, function(x) as.numeric(as.character(x)))

#***********************
#	Grafico Femmine
#***********************

attach(mydataF)
par(mgp = c(3, 1, 0))
plot(lowess(Agemos, P3, f=.01), type="l", ylim=c(79, 191), xlim=c(24, 240), lwd=2, axes=F, xlab = "", ylab = "")
title(main="Stature-for-age charts: \n selected smoothed stature percentiles in centimeters", ylab="Stature (cm)", xlab="Stature (cm)")
mtext("2 to 20 years, Girls", cex=.95, font=3, col="red")

# y axis left
axis(side=2, at=seq(79, 191, 1), labels=FALSE, col.ticks="red")
axis(side=2, at=seq(80, 190, 5), labels=seq(80, 190, 5), lwd.ticks=2)

# y axis right
axis(side=4, at=seq(79, 191, 1), labels=FALSE, col.ticks="red")
axis(side=4, at=seq(80, 190, 5), labels=seq(80, 190, 5), lwd.ticks=2)

# x axis down
axis(side=1, at=seq(24, 240, 6), labels=FALSE, col.ticks="red")
axis(side=1, at=seq(24, 240, 12), labels=seq(2, 20, 1), lwd.ticks=2)

lines(lowess(Agemos, P5,  f=.01))
lines(lowess(Agemos, P10, f=.01), lwd=2)
lines(lowess(Agemos, P25, f=.01))
lines(lowess(Agemos, P50, f=.01), lwd=2)
lines(lowess(Agemos, P75, f=.01))
lines(lowess(Agemos, P90, f=.01), lwd=2)
lines(lowess(Agemos, P95, f=.01))
lines(lowess(Agemos, P97, f=.01), lwd=2)

abline(v=seq(24, 240, 12), col="#FF000050")
abline(h=seq(79, 191, 1),  col="#FF000050")

text(max(Agemos)+3, max(P3),  "3rd",  adj=0.3, cex=.55)
text(max(Agemos)+3, max(P5),  "5th",  adj=0.3, cex=.55)
text(max(Agemos)+3, max(P10), "10th", adj=0.3, cex=.55)
text(max(Agemos)+3, max(P25), "25th", adj=0.3, cex=.55)
text(max(Agemos)+3, max(P50), "50th", adj=0.3, cex=.55)
text(max(Agemos)+3, max(P75), "75th", adj=0.3, cex=.55)
text(max(Agemos)+3, max(P90), "90th", adj=0.3, cex=.55)
text(max(Agemos)+3, max(P95), "95th", adj=0.3, cex=.55)
text(max(Agemos)+3, max(P97), "97th", adj=0.3, cex=.55)

detach(mydataF)
