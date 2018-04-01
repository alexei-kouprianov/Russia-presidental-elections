###############################################################################
###############################################################################
# 
# Monte-Carlo simulation for St. Petersburg voters' turnout histogram
# 
###############################################################################
###############################################################################

###############################################################################
# 
# The following script is based on the methods described in 
# Dmitry Kobak, Sergey Shpilkin, and Maxim S. Pshenichnikov "Integer 
# percentages as electoral falsification fingerprints". The Annals of Applied 
# Statistics. Volume 10, Number 1 (2016), 54-73.
# 
# I am indebted to Sergey Shpilkin for his kind explanations and critical 
# remarks on the early versions of the script. 
# I am grateful to Boris Ovchinnikov for a stimulating discussion.
#
# Alexei Kouprianov, alexei.kouprianov@gmail.com
#
###############################################################################

###############################################################################
# Loading data
###############################################################################

spb.2018 <- read.table("spb.q.20180319.txt", h=TRUE, sep="\t")
spb.2018$VOTED <- spb.2018$BALL.INVALID + spb.2018$BALL.VALID
spb.2018$TURNOUT <- spb.2018$VOTED/spb.2018$VOTERS

###############################################################################
# Declaring objects for the loop
###############################################################################

# MK.repeats <- 100 # For preliminary testing;
# MK.repeats <- 1000 # For preliminary testing;
MK.repeats <- 10000 # Working repeats number;

spb.ksp.rbinom.ls <- NULL # ls of simulated polling stations;
spb.ksp.rbinom.ls <- as.list(spb.ksp.rbinom.ls)

spb.ksp.rbinom.TURNOUT <- NULL # complete vector of simulated turnouts;

spb.ksp.rbinom.TURNOUT.hist.counts.df <- NULL # df for simulated histograms' counts;
spb.ksp.rbinom.TURNOUT.hist.counts.df <- as.data.frame(spb.ksp.rbinom.TURNOUT.hist.counts.df)

j <- NULL
i <- NULL

###############################################################################
# Main simulation loop begins
###############################################################################

for (k in 1:MK.repeats) {

spb.ksp.rbinom.ls <- NULL # ls of simulated polling stations;
spb.ksp.rbinom.ls <- as.list(spb.ksp.rbinom.ls)

spb.ksp.rbinom.TURNOUT <- NULL # complete vector of simulated turnouts;

# Simulating polling stations' turnouts
j <- 1
while(j <= length(spb.2018$VOTERS)){
spb.ksp.rbinom.ls[[j]] <- rbinom(n=1, size=spb.2018$VOTERS[j], prob=spb.2018$TURNOUT[j])
j <- j + 1
}

# Gathering simulated turnouts in a vector
i <- 1
while(i <= length(spb.2018$VOTERS)){
spb.ksp.rbinom.TURNOUT <- c(spb.ksp.rbinom.TURNOUT, spb.ksp.rbinom.ls[[i]]/spb.2018$VOTERS[i])
i <- i + 1
}

# Calculating hist for simulated turnouts, bin 1% (hist plots appear in the graphics console)
spb.ksp.rbinom.TURNOUT.hist <- hist(spb.ksp.rbinom.TURNOUT, breaks=seq(-.005, 1.005, .01))

# Extracting simulated hist counts
spb.ksp.rbinom.TURNOUT.hist.counts.df <- rbind.data.frame(spb.ksp.rbinom.TURNOUT.hist.counts.df, spb.ksp.rbinom.TURNOUT.hist$counts)
}

###############################################################################
# Main simulation loop ends
###############################################################################

# Extracting summary stats for counts from all MK.repeats simulated hists into a data frame

spb.ksp.rbinom.TURNOUT.hist.counts.MEAN <- NULL
spb.ksp.rbinom.TURNOUT.hist.counts.SD <- NULL
spb.ksp.rbinom.TURNOUT.hist.counts.MEDIAN <- NULL
spb.ksp.rbinom.TURNOUT.hist.counts.Q1 <- NULL
spb.ksp.rbinom.TURNOUT.hist.counts.Q3 <- NULL
spb.ksp.rbinom.TURNOUT.hist.counts.IQR <- NULL

for (i in 1:101){
spb.ksp.rbinom.TURNOUT.hist.counts.MEAN <- c(spb.ksp.rbinom.TURNOUT.hist.counts.MEAN, mean(spb.ksp.rbinom.TURNOUT.hist.counts.df[,i]))
spb.ksp.rbinom.TURNOUT.hist.counts.SD <- c(spb.ksp.rbinom.TURNOUT.hist.counts.SD, sd(spb.ksp.rbinom.TURNOUT.hist.counts.df[,i]))
spb.ksp.rbinom.TURNOUT.hist.counts.MEDIAN <- c(spb.ksp.rbinom.TURNOUT.hist.counts.MEDIAN, median(spb.ksp.rbinom.TURNOUT.hist.counts.df[,i]))
spb.ksp.rbinom.TURNOUT.hist.counts.Q1 <- c(spb.ksp.rbinom.TURNOUT.hist.counts.Q1, summary(spb.ksp.rbinom.TURNOUT.hist.counts.df[,i])[2])
spb.ksp.rbinom.TURNOUT.hist.counts.Q3 <- c(spb.ksp.rbinom.TURNOUT.hist.counts.Q3, summary(spb.ksp.rbinom.TURNOUT.hist.counts.df[,i])[5])
spb.ksp.rbinom.TURNOUT.hist.counts.IQR <- c(spb.ksp.rbinom.TURNOUT.hist.counts.IQR, IQR(spb.ksp.rbinom.TURNOUT.hist.counts.df[,i]))
}

spb.hist.ksp.simulated.stats.1pct <- data.frame(
seq(0, 1, .01),
spb.ksp.rbinom.TURNOUT.hist.counts.MEAN,
spb.ksp.rbinom.TURNOUT.hist.counts.SD,
spb.ksp.rbinom.TURNOUT.hist.counts.MEDIAN,
spb.ksp.rbinom.TURNOUT.hist.counts.Q1,
spb.ksp.rbinom.TURNOUT.hist.counts.Q3,
spb.ksp.rbinom.TURNOUT.hist.counts.IQR
)

colnames(spb.hist.ksp.simulated.stats.1pct) <- c("PCT","MEAN","SD","MEDIAN","Q1","Q3","IQR")

# Control plots

png("hist.TURNOUT.spb.MEDIAN-IQR.simul.png", height=750, width=750)
par(cex=1.5, lwd=1.5)
hist(spb.2018.s.dyn$TURNOUT, breaks=seq(-.005, 1.005, .01), 
ylim=c(0,190), col=rgb(0,0,1,.3), border=rgb(0,0,1,.3),
main="Presidential elections in Russia, 2018-03-18\nSt. Petersburg",
xlab="Voters' turnout, 1% bin",
ylab="Frequency")
polygon(c(spb.hist.ksp.simulated.stats.1pct$PCT, spb.hist.ksp.simulated.stats.1pct$PCT[101:1]), 
c(spb.hist.ksp.simulated.stats.1pct$MEDIAN+1.5*spb.hist.ksp.simulated.stats.1pct$IQR, (spb.hist.ksp.simulated.stats.1pct$MEDIAN-1.5*spb.hist.ksp.simulated.stats.1pct$IQR)[101:1]), 
col=rgb(0,0,0,.3), border=rgb(1,0,0,.3))
points(spb.hist.ksp.simulated.stats.1pct$PCT, spb.hist.ksp.simulated.stats.1pct$MEDIAN, type="l", col=2, lwd=3)
legend("topleft", lwd=c(3,1), col=c(2,2), legend=c("MC-simulated median", "MC-simulated median +/- 1.5 IQR"), bty="n")
axis(1, at=seq(0,1,.1), labels=FALSE, lwd=1.5)
axis(2, at=seq(0,190,10), tcl=-.25, labels=FALSE, lwd=1.5)
dev.off()

png("hist.TURNOUT.spb.MEAN-SD.simul.png", height=750, width=750)
par(cex=1.5, lwd=1.5)
hist(spb.2018.s.dyn$TURNOUT, breaks=seq(-.005, 1.005, .01), 
ylim=c(0,190), col=rgb(0,0,1,.3), border=rgb(0,0,1,.3),
main="Presidential elections in Russia, 2018-03-18\nSt. Petersburg",
xlab="Voters' turnout, 1% bin",
ylab="Frequency")
polygon(c(spb.hist.ksp.simulated.stats.1pct$PCT, spb.hist.ksp.simulated.stats.1pct$PCT[101:1]), 
c(spb.hist.ksp.simulated.stats.1pct$MEAN+3*spb.hist.ksp.simulated.stats.1pct$SD, (spb.hist.ksp.simulated.stats.1pct$MEAN-3*spb.hist.ksp.simulated.stats.1pct$SD)[101:1]), 
col=rgb(0,0,0,.3), border=rgb(1,0,0,.3))
points(spb.hist.ksp.simulated.stats.1pct$PCT, spb.hist.ksp.simulated.stats.1pct$MEAN, type="l", col=2, lwd=3)
legend("topleft", lwd=c(3,1), col=c(2,2), legend=c("MC-simulated mean", "MC-simulated mean +/- 3 SD"), bty="n")
axis(1, at=seq(0,1,.1), labels=FALSE, lwd=1.5)
axis(2, at=seq(0,190,10), tcl=-.25, labels=FALSE, lwd=1.5)
dev.off()
