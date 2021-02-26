library(learnPopGen)
learnPopGen
?coalescent.plot()
install.packages("learnPopGen")
library("learnPopGen")
install.packages("coala")
library("coala")
install.packages("phytools")
library("phytools")
coalescent.plot()
coalescent.plot
This model has 10 alleles that can be changed via the n= function
pdf('r.05=question1', hegiht =5, width=5)
The allele goes to fixcation after 10 generations. 
pdf(r'05-question2'.pdf, height=5, width=5)

coalescent.plot(n=20, ngen=20, color=NULL)
dev.off()
The average number of offspring was 2.
pdf('r.05-question3.pdf', height=5, width=5)
coalescent.plot(n=10, ngen=1, colors=NNULL)
coalescent.plot(n=10,ngen=5, colors=NULL)
dev.off()
Individuals go extinct when they can not mate or reproduce many times. If they are unfit, it is less likely that they will create progeny therefore they will be pushed towards extinction. 
Yes.

model <- coal_model(sample_size = 5, loci_number = 10, loci_length = 500, ploidy = 2) +
feat_mutation(10) +
feat_recombination(10) +
sumstat_trees() +
sumstat_nucleotide_div()
stats <- simulate(model, nsim = 1)
Diversity <- stats$pi
The alleles change due to mutations, recombinations and crossover events during replication. 

Nloci <- length(stats$trees)
t1 <- read.tree(text=stats$trees[[1]][1])
plot(t1)

axisPhylo()
There are two alleles for each individual so the number of tips will be change.

Age1 <- read.tree(text=stats$trees[[2]][1])
t2 <- read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()
The newest is much different than the first one.
No
par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()
compare.chronograms(t1, t2)
t1_1 <- read.tree(text=stats$trees[[1]][1])
t1_2 <- read.tree(text=stats$trees[[1]][2])
compare.chronograms(t1_1, t1_2)
for (loacus in 1:Nloci) {
	ntrees <- length(stats$trees[[locus]])
	for (n in 1:ntrees) {
		if(locus == 1 && n ==1) {
			outPhy <- read.tree(text=stats$trees[[locus]][n])
			}
			else {
				outPhy <- ape:::c.phylo(outPhy,
			read.tree(text=stats$trees[[locus]][n]))
			}
		}
	}
par(mfrow=c(1,1))
densityTree(outPhy)
model3 <- coal_model(10,50) +
feat_mutation(par_prior("theta", sample,int(100,1))) +
sumstat_nucleotide_div()
stats <- simulate(model3, nsim=40)
mean_pi <- sapply(stats, function(x) mean(x$pi))
theta <- sapply(stats, function(x) x$pars[["theta"]])