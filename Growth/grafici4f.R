#************************
#**Head circumference-for-age charts, birth to 36 months, LMS parameters and selected smoothed head circumference percentiles in centimeters, by sex and age 
#************************

#	load data
#	F
ccF <- get(load("./Growth/data/mydata4f.rda"))

#	create dataframe for F with numeric
mydataF <- ccF
cols = c(1:ncol(mydataF))
mydataF[,cols] = apply(mydataF[,cols], 2, function(x) as.numeric(as.character(x)))

#***********************
#	Grafico Femmine
#***********************

attach(mydataF)
par(mgp = c(3, 1, 0))
plot(lowess(Agemos, P3, f=.01), type="l", ylim=c(31, 53), xlim=c(0, 36), lwd=2, axes=F, xlab = "", ylab = "")
title(main="Head circumference-for-age charts: \n selected smoothed head circumference percentiles in centimeters", ylab="Head circumference (cm)", xlab="Age (months)")
mtext("Birth to 36 months, Girls", cex=.95, font=3, col="red")

# y axis left
axis(side=2, at=seq(31, 53, .2), labels=FALSE, col.ticks="red")
axis(side=2, at=seq(31, 53, 1), labels=seq(31, 53, 1), lwd.ticks=2)

# y axis right
axis(side=4, at=seq(31, 53, .2), labels=FALSE, col.ticks="red")
axis(side=4, at=seq(31, 53, 1), labels=seq(31, 53, 1), lwd.ticks=2)

# x axis down
axis(side=1, at=seq(0, 36, 0.5), labels=FALSE, col.ticks="red")
axis(side=1, at=seq(0, 36, 1), labels=seq(0, 36, 1), lwd.ticks=2)

lines(lowess(Agemos, P5,  f=.01))
lines(lowess(Agemos, P10, f=.01), lwd=2)
lines(lowess(Agemos, P25, f=.01))
lines(lowess(Agemos, P50, f=.01), lwd=2)
lines(lowess(Agemos, P75, f=.01))
lines(lowess(Agemos, P90, f=.01), lwd=2)
lines(lowess(Agemos, P95, f=.01))
lines(lowess(Agemos, P97, f=.01), lwd=2)

abline(v=seq(0, 36, 1),    col="#FF000050")
abline(h=seq(31, 53, 0.2), col="#FF000050")

text(max(Agemos)+.7, max(P3),  "3rd",  adj=0.3, cex=.55)
text(max(Agemos)+.7, max(P5),  "5th",  adj=0.3, cex=.55)
text(max(Agemos)+.7, max(P10), "10th", adj=0.3, cex=.55)
text(max(Agemos)+.7, max(P25), "25th", adj=0.3, cex=.55)
text(max(Agemos)+.7, max(P50), "50th", adj=0.3, cex=.55)
text(max(Agemos)+.7, max(P75), "75th", adj=0.3, cex=.55)
text(max(Agemos)+.7, max(P90), "90th", adj=0.3, cex=.55)
text(max(Agemos)+.7, max(P95), "95th", adj=0.3, cex=.55)
text(max(Agemos)+.7, max(P97), "97th", adj=0.3, cex=.55)

detach(mydataF)
