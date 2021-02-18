results <- read.csv("http://jonsmitchell.com/data/biol112labresults.csv", stringsAsFactors=F)
results
counts <- results[,c("yellow", "red", "green", "blue", "black", "tan")]
backgrounds <- c("White", "Red", "Yellow", "Green", "Blue", "Black")
backgroundCol <- c("white", "#d53e4f", "fee08b", "abdda4", "#3288bd", "black")
 Now let's calculate the chi squared statistic for the first row of counts
 
calcChi(counts[1,])
= 55.2

Now let's calculate chi squared for all of the rows at once

Chisq <- apply(counts, 1, calcChi)
Chisq

plotChis(counts)

The bars are not even when the chi squared is high and they are very even when the chi squared is low. 

Chisq(counts)

mean(Chisq)
The average chi squared is 60.99081

avg <- mean(Chisq)
avg
I would interpret that the difference among groups is relatively high which would result in a higher chi squared value. 

The chi squared value is much higher than the critical value of 11.70 meaning that the groups had differences on average. This shows that the expected values from the statistical set were very different from the observed results from lab. 

Does the chi squared differ in background?
backgroundAvgs <- tapply(Chisq, results[,3], mean)

backgroundAvgs
Each background has a different background avg but all are fairly similar with the exception of the outlier "red" with a low chi squared compared to other groups.

propSig <- length(which(Chisq > 11.70))/ length(Chisq)
percSig <- round(100 * propSig)
percSig = 92

The fact that 92 percent of trials were significantly different from the expectation. This does not surprise me because after looking at the chi squared of the counts and then seeing the critical value, it was clear that most of the data was a substantial distance from the exoected value. 

I do not think natural selection is the only reason this percentage is so high. I could also see the finite popualtion being a force as well as non random selection since each group must add offspring of the same color as the parent toothpick. 

par(las =1, mar=c(4, 4, 1, 1), mgp=c(2, 0.5, 0), tck = -0.01, cex.axis=1)
hist(Chisq, main="", xlab="chi-squared values", ylab="frequency")

As the chi squared value gets higher, the frequency of alleles gets lower.

par(las= 1, mar=c(4, 4, 1, 1), mgp=c(2, 0.5, 0), tck= -0.01, cex.axis=1)
plot(1, 1, xlim=c(0, 400), ylim=c(1, 8.5), xlab="", ylab="", type="n", yaxt="n")

axis(2, at=1:length(backgrounds), labels=backgrounds)
mtext(side=1,expression(chi^2), cex=1.75, line=2.5)
counter <- 1
for(i in backgrounds) {
	Data <- Chisq[which(results[,3]== i)]
	addHist(Y=counter, Dat=Data, Color=backgroundCol[counter])
	counter <- counter + 1
	}
	abline(v=11.70, lty=2, lwd=2, col='black')
The critical value line is to the left of most subjects showing that the backgrounds were usually a significant difference between one another. 

Red and white seemed to be outliers given the graph. 

Simulation <- simDraws(10000)
addHist(Y=7, Dat=Simulation, Color="lightgray")
mtext(side=2, at=7, line=0, "simulated")
abline(v=11.70, lty=2, lwd=2)
propSig <- length(which(Simulation > 11.70))/length(Simulation)
percSig <- round(100 * propSig)
percSig
89 percent of the simulation variables found a value that was significantly different from the critical value. 


Fit <- c(1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation2 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation2, Color=rgb(0,0,0,0.25))

Fit <- c(0.1, 1, 1, 1, 1, 1)
names(Fit) <- 1:6
Simulation3 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation3, Color=rgb(0,0,0,0.25))

Fit <- c(0.5, 0.6, 0.7, 1, 1, 1)
names(Fit) <- 1:6
Simulation4 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation4, Color=rgb(0, 0, 0, 0.25))

Fit <- c(0.1, 0.2, 0.3, 0.4, 0.5, 1)
names(Fit) <- 1:6
Simulation5 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation5, Color=rgb(0,0,0,0.25))

Fit <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
names(Fit) <- 1:6
Simulation6 <- simDraws(1e4, w=Fit)
addHist(Y=8, Dat=Simulation6, Color=rgb(0,0,0,0.25))
mtext(side=2, at=8, line=0, "sel. sim.")

Simulation7 <- c(Simulation2, Simulation3, Simulation4, Simulation5, Simulation6)
addHist(Y=8, Dat=Simulation7, Color=rgb(0,0,1,0.25))
The mixture seems to be much different than the student data. Most of the groups from the lab showed high selection towards a certain color where the simulated groups were spread over a larger range and kept their diversity. I would say that the relative strengh of selection was pretty high among the data seen from the lab groups. 

Evolutionary processes include selection, finite population, and i suppose you could say heredity. 

The same processes are included in the simulated version except it is much more random and selection is much slower. 

The graphs show that the relative strength of evolutionary processes are strong and high in the lab. I think comparing the values to a critical value was more helpful because there is a number that we know when we divert from, we become more reliant on selection and other processes.

I think that letting the toothpicks mutate wouold allow for a higher x^2 number becasue it wouold add another color possibility making it possible to differ even more from the values that are expected. 

