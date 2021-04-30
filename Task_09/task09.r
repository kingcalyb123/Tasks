setwd("~/Desktop/Evolution/Tasks/Task_09")
library(phytools)
trees<-list()
births<- c()
Fractions<- c()
for(i in 1:100) {
	births[i] <- runif(1)
	Fractions[i] <- runif(1)
	trees[[i]] <- pbtree(b=births[i], d=(births[i] * Fractions[i]), n=100, nsim=1)
	}
trees
trees[[i]]
plot(trees[[i]])
install.packages('geiger')
library('geiger')
install.packages('TreeTools')
library('TreeTools')
install.packages('rbibutils')
yes
Y
tips<- sapply(trees,NTip)
library('rbibutils')
logtips<- log(tips)
diversification<- sapply(trees, bd.ms)
plot(diversification, logtips, xlab='tota diversification', ylab= 'log of total tips')
abline(lm(diversification~logtips), col='red')
Question 4: The plot provides support that there is a postive correlation between diversification and the amount of tips. 
cor(diversification, logtips).   < shows a + correlation
speciation<- sapply(trees, bd.km)
numtips<- c()
avgBL<- c()
for(i in 1:length(trees)) {
	y<- trees[[i]]
	numtips[i]<- Ntip(y)
	avgBL[i]<- mean(y$edge.length)
	}
plot(speciation, avgBL, clab='speciation rate', ylab='avg branch length')
Question 5: Speciation and branch length are inversely proportional to eachother according to the graph made. 
cor(speciation, avgBL)
*the correlation is -0.38
which.max('tips')
bigTree<- trees[[66]]
plot(bigTree)
rates<- c()
traits<- list()
for(i in 1:100) {
	rates[i]<- runif(1)
	traits[[i]]<- fastBM(tree=bigTree, sig2=rates[i])
	}
avgtrait<- sapply(traits, mean)
avgtrait
avgrate<- sapply(rates, mean)
avgrate
correlation<- cor(avgtrait, avgrate)
print(correlation)
plot(avgrate, avgtrait)		
The correlation between trait and rate is -0.009630845
abline(lm(avgrate~avgtrait), col='black')

vartraits<- sapply(traits, var)
cor(vartraits, rates)
Question 9: THere is a positive correlation between rates and var of traits. It is 0.5766717

trait1<- traits[1]
trait1
trait2<- traits[2]
trait2
traitmat<- cbind(traits[[1]], traits[[2]])
traitmat
var(traitmat)
cor(traitmat[,1], traitmat[,2])
Question 10: The correlation between the first and second elements of traits is -0.02832264. This number is fairly close to 0 so I would not deem it significant. 
plot(traitmat[,1], traitmat[,2])
abline(lm(traitmat[,1]~traitmat[,2]), col='green')

