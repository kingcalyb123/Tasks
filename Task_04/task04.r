source("https://jonsmitchell.com/code/fxn05.R")

Pop1 <- simPop(Popsize = 50, nGenerations = 100, initial_p = 0.5, h=1, s=0)
plot(1:nrow(Pop1), Pop1[,1], ylim=c(0,1), type="1", xlab="generation", ylab="allele freq.", lwd=2)
lines(1:nrow(Pop1), Pop1[,2], lwd =2, col='red')
legend("topleft", legend =c('a', "b"), col=c("black", "red"), lwd=2, bty="n")

plotFit(nruns = 10, n = 50, ngens = 100, init_p = 0.5, h=1, s = 0)

Let's assume that we expect to see four equal categories. 
Expectation <- c(10, 10, 10, 10)

s
Now let's pretend what we actually see is four uneven categories
Observed <- c(15, 15, 5, 5)

We can use the chi-squared statistic using these numbers
Chisq <-sum(((Expectation - Observed) ^2)/ Expectation)

And we can visualize what this means pretty readily
barplot(rbind(Expectation, Observed), beside = T, main = bquote(chi^2 ~ "=" ~.(Chisq)), legend.text=c("expected", "observed"))

Observed <- c(5, 0, 0, 35)
Chisq = 85

Changing the observed to the latter numbers makes the Chi-Squared value go from 10 to 85 meaning that the observation between the expected and observed values got muuch more different than they previously were. 

Observed <- c(2, 3, 10, 30)
Chisq =51.3
The Chi Squared value dropped lower when changing the observed to 2, 3, 10, 30 than it was in the previous examples. This means that the numbers that were inserted were pretty distinct from the expected of 10, 10, 10, 10 but was still closer than the previous example was. 

Observed <- c(10, 10, 10, 10)
Chisq
When the observed values are the same as the expected values, the chi-squared test will always be 0 because it is 0% different than the expected value. This probably never happens. 

Observed <- c(40, 0, 0, 0)
Chisq = 120
The chi squared is 120 when all of the samples fall in one category and this is probbaly as far away from the expected values that you can get because there is no distribution whatsoever. 

The closer that the expected and observed values are, the closer x^2 will be to zero and the more even the bars on the plot will be. When there is no difference between observed and expected values and the chi-squared is 0, the bars will be even across all four sample spectrums.

simDraws <- function(nruns, ncols=6, nstart=10, nrounds=3, mu=0, twoway=TRUE, w=NULL) {
	Chiout <- c()
	for (j in 1:runs) {
		Start <- rep(1:ncols, nstart)
		Pop <- Start
		for (i in 1:nrounds) {
			if (is.null(w)) {
				Draws <- sample(Pop, 20, replace =T)
				}
				else if (!is.null(w)) {
					if(length(setdiff(unique(Pop), names(w))) ==0) {
						Draws <- sample(Pop, 20, replace=T, prob=w[Pop])
						}
					else if(length(setdiff(unique(Pop), names(w))) !=0) {
						cat("Not enough fitness values!", setdiff(unique(Pop), names(w)))
						}
					}
					Pop <- sort(c(Draws, Draws, Draws))
				}
				Summary <- c()
				for(k in 1:ncols) {
					Summary[k] <- length(which(Pop ==k))
				}
				
				Chiout[j] <- sum(((Summary - nstart)^2)/ nstart)
			}
			return(Chiout)
}

Simulation8 <- simDraws(1e4, mu=2)
addHist(Y=8, Dat=Simulation8, Color=rgb(0,0,0,0.25))
Simulation9 <- simDraws(1e4, mu=300)
addHist(Y=8, Dat=Simulation9, Color=rgb(0,0,0,0.25))
More mutations seem to make the distubtion less compact and pushed it to the right giving a higher chi squared value

Mutations make the chi squared values higher meaning that they change actual findings to be further from the expected meaning that there will be more diversity and therefore, more selection. So selection must be correlated with mutations. Also, drift is occurring with higher chi squared values so selection and drift are directly correlated in this instance. 
