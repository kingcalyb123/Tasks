install.packages('phytools')
install.packages('ape')
library(phytools)
library(ape)
text.string<-
"(((((((cow, pig), whale), (bat,(lemur, human))),(robin, iguana)), coelacanth),(gold_fish, trout)),shark);"
vert.tree<- read.tree(text=text.string)
plot(vert.tree, edge.width=2)
nodelabels(frame="circle", bg='white', cex=1)
Question 1= "what's more closely related to the gold fish, a shark or a human?"
Gold fish are more closely related to humans than they are to sharks. Gold fish and sharks are connected at node 13 where humans and goldfish are connected more recently at node 14.

vert.tree
str(vert.tree)
Question 2; There are no branch lengths in this tree although it does tell us how many nodes are present. It also includes the numbers or each nodes. 

tree<-read.tree(text="(((A,B), (C,D)), E);")
plotTree(tree, offset=1)
tiplabels(frame="circle", bg='lightblue', cex=1)
nodelabels(frame="circle", bg='white', cex=1)
tree$tip.label
tree$edge
AnolisTree<- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
par(las=1)
hist(AnolisTree$edge.length, mcol='black', border='white', main="", xlab="edge lengths for the Anolis tree", ylim=c(0,50), xlim=c(0,6))
tipEdges<-which(AnolisTree$edge[,2]<=Ntip(AnolisTree))
Lengths<-AnolisTree$edge.length
names(Lengths)<- AnolisTree$tip.label
names(Lengths)[which(Lengths== min(Lengths))]
plot(AnolisTree, cex=0.25)
Labs<-sapply(AnolisTree$edge.length,round,digits=2)
edgelabels(text=Labs, cex=0.25)
?plot.phylo
Question 3:
plotTree(AnolisTree,offset=1, show.tip.label=FALSE)

Question 4:
anolis <- collapseTree(AnolisTree)
Question 5:
plotTree(AnolisTree,offset=1,show.tip.label=TRUE, tip.color="red")

Question 6,7,8
AnolisTree$edge
EdgesThatAreTips <- which(AnolisTree$edge[,2] <= Ntip(AnolisTree))
TipLengths <- AnolisTree$edge.length[EdgesThatAreTips]
AnolisShortestTips <- which(TipLengths == min(TipLengths))
Anolistree_no_short <- drop.tip(tree, AnolisShortestTips[1])
plot(Anolistree_no_short)
ltt(AnolisTree)
abline(0,1,lwd=2,col='red', lty=2)
The line never goes down. I would say it continues to rise because the lineages continue increasing in length and time present as time goes on. I think this may be the case because there has been more time to evolve and adapt to the environment allowing certain branches of the phylogeny and lineages to remain constant for longer periods of time. Although this is the case, there might be decreases eventually because environments may change due to catastrophe or limited resources which cause punctuated equilibrium. 

fit.bd(AnolisTree, rho=0.2)
?fit.bd
The birth rate is 0.8031 and the death rate is 0 according to this funtion. This means that the population should continue to rise without disaapearance. R also states that it thinks the phylogeny has converged. 