source("https://jonsmitchell.com/code/reformatData07.R")
source("https://jonsmitchell.com/code/simFxn.R")

plot(1,1, type="n", xlim=c(1998, 2013), ylim=c(0,1))
s <- apply(overallFreq, 2, function(x) lines(overallFreq[,1], x, col=rgb(0,0,0,0.01)))
rescaleFreq <- apply(overallFreq[,3:ncol(overallFreq)], 2, function(x)x-x[1]
plot(1,1, type="n", xlim=c(1998, 2013) ylim=c(-0.25, 0.25))
s <- apply(rescaleFreq, 2, function(x) lines(overalFreq[,1], x, col=rgb(0,0,0,0.01)))
plot(1, 1, type="n", xlim=c(1998, 2013), ylim=c(-0.25, 0.25))
dYear <- c()
dAlleles <- c()
for(i in 3:ncol(overalFreq)) {
	dYear <- c(dYear, overallFreq[,1])
	Vec <- overallFreq[,i]
	Init <- overallFreq[1,i]
	dAlleles <- c(dAlleles, Vec - Init)
	
]
smoothScatter(dYear, dAlleles, colramp=Pal, nbin=100)
smoothScatter(dYear, dAlleles, colramp=Pal, nbin=100, xlab="year", ylab="change in allele freq. since 1998")
addFit(nruns=50, n=100, ngens=18, startT=1997, simCol="gray40", rescale=TRUE)
plot(alleleFreqs$d_freq, alleleFreqs$d_imm, xlim=c(-0.015, 0.15), xlab="overall freq. change", ylab="freq. change in subset")
points(alleleFreqs$d_freq, alleleFreqs$d_birth, col='blue')
points(alleleFreqs$d_freq, alleleFreqs$d_surv, col='red')
The freq allele change in subset has a higher range in the birth subset than the survival subset. Possibly this means that heritability and drift effect individuals much more before they are born, because of the probabilityh of their parents even forwarding their geneitc info onto subsequent generations. Where as the survivors wont have any changes to their genetic makeup, but only the organisms that they birth will experience the selection or drift that the survivors encounter through life. 

