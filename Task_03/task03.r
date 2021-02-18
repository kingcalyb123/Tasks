trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)
trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD)
Size <- 50
Sample1 <- sample(population1, Size)
Sample2 <- sample(population2, Size)
boxplot(Sample1, Sample2)
The populations are slightly different. This is only due to the slight change in means between the populations. Since the samples are different, then we can assume that the populations are different as well. 

source("https://jonsmitchell.com/code/simFxn04.R")
MatGrandma <- makeFounder("grandma_mom")
head(MatGrandma)
ncol(MatGrandma)
nrow(MatGrandma)
MatGrandpa <- makeFounder("grandpa_mom")
head(MatGrandpa)
nrow(MatGrandpa)
PatGrandma <- makeFounder("grandma_da")
head(PatGrandma)
nrow(PatGrandma)
PatGrandpa <- makeFounder("grandpa_da")
Alan <- makeBaby(PatGrandma, PatGrandpa)
Brenda <- makeBaby(MatGrandma, MatGrandpa)
Focus <- makeBaby(Brenda, Alan)
The number should be roughly 50%.
ToMom <- length(grep("mom", Focus))/length(Focus)
ToMom
Focus should share roughly 25% of the genes with each of the maternal grandparents. 
ToMomMom <- length(grep("grandma_mom", Focus))/ length(Focus)
ToMomMom
The genes shared are 0.27125 with the maternal grandma and 0.22875 with the maternal grandpa which is very close to the expected value. 
ToMomDad <- length(grep("grandpa_mom", Focus))/ length(Focus)
ToMomDad
ToDadMom <- length(grep("grandma_da", Focus))/ length(Focus)
ToDadDad <- length(grep("grandpa_da", Focus))/ length(Focus)
ToDadMom
ToDadDad
The paternal grandma shares 0.3411 percent of her genes with Focus and the paternal grandpa shares 0.1589 percent of his genes with focus which is much off from the expected 25%. 
mean(0.27125, 0.22875,0.3411,0.1589)
The mean or average is still 25%. 
Sibling_01 <- makeBaby(Brenda, Alan)
ToSib <- length(intersect(Focus, Sibling_01))/ length(Focus)
ToSib
Focus and the sibling share about 58.59% of their DNA. This is a little off from my expectations of them sharing around 50% of their DNA assuming that they are not twins. 
I would assume that Focus would share close to 50% of genes with all of the other children. 
ManySiblings <- replicate(1e3, length(intersect(Focus, makeBaby(Brenda, Alan)))/ length(Focus))
ManySiblings
Based on the ManySiblings input, Focus shared anywehre from 10 to almost 90% of DNA with the many siblings that were created.
quantile(ManySiblings)
mean(ManySiblings)
plot(density(ManySiblings), main="", xlab="proportion shared genes")
We see a range of analysis because all genes are not passed from generation to generation in a set percentage. Each parents pass a little bit of different genes to each child unless they are monozygotic twins. We see in the plot the most of the siblings of Focus have roughly 50 percent of shared DNA, but we also get to see how there continues to be a wide range of shared DNA.

HWE <- function(p)  {
aa <- p^2
ab <- 2*p*(1-p)
bb <- (1-p)^2
return(c(aa=aa, ab=ab, bb=bb))
}
HWE(0.5)
plot(1, 1, type="n", xlim=c(0,1), ylim=c(0,1), xlab="freq. allele a", ylab="geno. freq")
p <-seq(from = 0, to = 1, by = 0.01)
GenoFreq <- t(sapply(p, HWE))
lines(p, GenoFreq[,"aa"], lwd=2, col="red")
As the frequency of allele a increases, the genetic frequency of it also increases. As aa would decrease in frequencys, its genetic frequency in the population would also decrease. It does not seem that time is shown on this plot. I also do not believe that geographic space is shown on the plot. This plot such shows the frequency of allele a in a population where aa is 0.25, ab is 0.50, and bb is 0.25.

lines(p, GenoFreq[,"ab"], lwd=2, col="purple")
lines(p, GenoFreq[,"bb"], lwd=2, col="blue")
legend("top", legend=c('aa', "ab", "bb"), col=c('red', "purple", "blue"), lty=1, lwd=2, bty="n")
Pop <- simPop(500)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500, pch=21, bg="red")
Yes, the genetic grequency of aa is roughly 18 to 30 percent which would come close to having an average of 25% like the HWE suggested earlier. 
Pop <- simPop(50)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/50, pch=22, bg="red")
With a smaller population, the frequency of the aa genotype does not fit the expectation that we would draw from the HWE. This is because Hardy Weinberg assumed that populations are infinite. So the larger the populatino is, the close its genotypes will be to the HWE. Since we lowered the population size, the genetic frequencies will become farther away from the expected value. 

install.packages('learnPopGen')
library('learnPopGen')
x <- genetic.drift(Ne=200, nrep=5, pause=0.01)
x <- genetic.drift(Ne=50, nreo=5, pause=0.01)
By changing the amount of individuals in a population to a lower number, we are again able to see that genetic drift occurs much quicker when there is a small population. The graph spreaded out much more than it did with Ne being 200. 

PopSizes <- 5:50
Samples <- rep(PopSizes, 5)
Now we will simulate all 230 populations and see how long it takes for one of the two alleles to go extinct

tExt <- sapply(Samples, function(x) nrow(simPop(x,500)))
To fit a line in R, we use lm()
Line <- lm(tExt ~ Samples)
summary(Line)
Line$coef
plot(Samples, tExt)
abline(Line)
This graph provides visual evidence that as the sample numbers increase, the distance between the points and the lines continue to increase. This means to me that at small populations, if a gene goes extinct, there is less chance of survival as a species because the genetic diversity will be much lower meaning that even if the popualtion grows, it will be less likely to adapt to its environmental changes. As the population size increases, time of extinction gets much more spread out because at a higher number of individuals, genes are less likely to go extinct by randowm chance. So as the sample number increases, the time of extinction can increase higher than it could if the population was dwindling. 

install.packages("lmtest")
library(lmtest(plot(length, character))
if(require('package'))
plot(length,character)
library(lmtest)
summary(library(lmtest))
linearmodel <- library(lmtest)
plot(linearmodel ylim=0, xlim=20, xlab='length')
cars$length
cars$class
slope(library(lmtest))
lmrob(Samples)
install.packages("sandwich")
library(sandwich)
install.packages("robustbase")
library(robustbase)
lmrob(Line)
plot(lmrob(Line))
abline(lmrob(Line))
The graph using this lmtest is differenct than the prior graph because it is much more uniform. We see a bunch of heteroskedasticity in the prior graph but not much in this robust linear model . The slope starts off very similar but it ends up going up and being greater than the previous plot at about halfway through the plot. So i would say that the estimation of the slope is slightly greater in the robust linear model than it was in the time of extinction plot!