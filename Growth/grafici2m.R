#************************
#**Length-for-age charts, birth to 36 months, LMS parameters and selected smoothed recumbent length percentiles in centimeters, by sex and age 
#************************

#	load data
#	M
ccM <- get(load("./Growth/data/mydata2m.rda"))

#	create dataframe for M with numeric
mydataM <- ccM
cols = c(1:ncol(mydataM))
mydataM[,cols] = apply(mydataM[,cols], 2, function(x) as.numeric(as.character(x)))

#***********************
#	Grafico Maschi
#***********************

attach(mydataM)
par(mgp = c(3, 1, 0))
plot(lowess(Agemos, P3, f=.01), type="l", ylim=c(40, 105), lwd=2, axes=F, xlab = "", ylab = "")
title(main="Length-for-age charts: \n smoothed recumbent length percentiles in centimeters", xlab="Age (Months)", ylab="Length (cm)")
mtext("Birth to 36 months, Boys", cex=.95, font=3, col="red")

# y axis left
axis(side=2, at=seq(40, 105, 5), labels=seq(40, 105, 5), lwd.ticks=2)
axis(side=2, at=setdiff(seq(40, 105, 1), seq(40, 105, 5)), labels=FALSE, col.ticks="blue")

# y axis right
axis(side=4, at=seq(40, 105, 5), labels=seq(40, 105, 5), lwd.ticks=2)
axis(side=4, at=setdiff(seq(40, 105, 1), seq(40, 105, 5)), labels=FALSE, col.ticks="blue")

# x axis down
axis(side=1, at=seq(0, 36, 0.5), labels=FALSE, col.ticks="blue")
axis(side=1, at=seq(0, 36, 1), labels=c(0:36), lwd.ticks=2)

lines(lowess(Agemos, P5,  f=.01))
lines(lowess(Agemos, P10, f=.01), lwd=2)
lines(lowess(Agemos, P25, f=.01))
lines(lowess(Agemos, P50, f=.01), lwd=2)
lines(lowess(Agemos, P75, f=.01))
lines(lowess(Agemos, P90, f=.01), lwd=2)
lines(lowess(Agemos, P95, f=.01))
lines(lowess(Agemos, P97, f=.01), lwd=2)

abline(v=seq(0, 36, 1),   col="#0000FF50")
abline(h=seq(40, 105, 1), col="#0000FF50")

text(max(Agemos)+.7, max(P3),  "3rd",  adj=0.3, cex=.55)
text(max(Agemos)+.7, max(P5),  "5th",  adj=0.3, cex=.55)
text(max(Agemos)+.7, max(P10), "10th", adj=0.3, cex=.55)
text(max(Agemos)+.7, max(P25), "25th", adj=0.3, cex=.55)
text(max(Agemos)+.7, max(P50), "50th", adj=0.3, cex=.55)
text(max(Agemos)+.7, max(P75), "75th", adj=0.3, cex=.55)
text(max(Agemos)+.7, max(P90), "90th", adj=0.3, cex=.55)
text(max(Agemos)+.7, max(P95), "95th", adj=0.3, cex=.55)
text(max(Agemos)+.7, max(P97), "97th", adj=0.3, cex=.55)

detach(mydataM)

