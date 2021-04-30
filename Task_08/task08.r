getwd()
setwd("/Users/calybking/Desktop/Evolution/Tasks/Task_08")

library('phytools')
tree <- read.tree('https://jonsmitchell.com/data/anolis.tre')
plot(tree, type='fan')
tree$tip.label
Question 1: Branch lengths are present and there are 81 tree tips. 
tree$edge.length

data <- read.csv('https://jonsmitchell.com/data/svl.csv', stringsAsFactors=F, row.names=1)
data
data[,1]
This data shows the species name of each lizard and the snout-vent-length of each one. This is a list object that contains 100 dimensions. 
dim(data)
typeof(data)

svl <- setNames(data$svl, rownames(data))
Ancestors <- fastAnc(tree, svl, vars=TRUE, CI=TRUE)
Ancestors
?fastAnc
Quesiton 3: They are stired in a vector containing internal nodes or in a list containing the estimates. The CI95 is the 95% confidence intervak of the estimates. 
Question 4: The two assumptions are that the function re-roots the tree at all internal nodes and computes the contrast state at the root every time. 

par(mar=c(0.1,0.1,0.1,0.1))
plot(tree, type="fan", lwd=2, show.tip.label=F)
tiplabels(pch=16, cex=0.25*Ancestors$ace)
obj <- contMap(tree, svl, plot=F)
plot(obj, type="fan", legend=0.7*max(nodeHeights(tree)), sig=2, fsize=c(0.7,0.9))
fossilData <- data.frame(svl=log(c(25.4,23.2,17.7,19.7,24,31)), tip1=c("aliniger","aliniger","occultus","christophei","cristatellus","occultus"), tip2=c("chlorocyanus","coelestinus","monticola","cybotes","augusticeps","augusticeps"))
fossilNodes<-c()
Question 5- The loop function runs an action that is wanted for every item in an entire list. 
nodeN<-c()
{
	for(i in 1:nrow(fossilData))
	i<-1
	if(i==1) {
		print(Ancestors)
		}
		}
Node<-fastMRCA(tree, fossilData[i,"tip1"], fossilData[i,"tip2"])
Node
fossilNodes[i]<- fossilData[i, 'svl']
fossilNodes[i]
nodeN[i] <- Node

names(fossilNodes)<- nodeN
Ancestors_withFossils<- fastAnc(tree, svl, anc.states=fossilNodes, CI=TRUE, var=TRUE)
Ancestors_withFossils
Ancestors_woFossils<- fastAnc(tree, svl,CI=TRUE, var=TRUE)
Ancestors_woFossils
plot(Ancestors_withFossils$ace, Ancestors_woFossils$ace, xlab='fossils', ylab='no fossils')
Question 7: Fossils increase estimated ancestral state estimates. 

Question 8-10
install.packages('geiger')
library('geiger')
?fitContinuous
fitContinuous(tree, svl, model='EB')
fitContinuous(tree, svl, model='OU')
fitContinuous(tree, svl, model='BM')

The first one 'EB' has the lowest and most negative A1C value meaning it is the best fit for the data. 
?fastAnc
fastAnc uses BM and therefore assumes differently than fitContinuous. EB is the best fit still.