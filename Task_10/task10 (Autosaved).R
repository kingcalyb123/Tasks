install.packages('diversitree')
library('diversitree')
transition_0to1 <- 0.1
transition_1to0 <- 0.1
speciation_0 <- 0.2
extinction_0 <- 0.1

speciation_1 <- 0.2
extinction_1 <- 0.1

maxN <- 1e3
maxT <- 50

Pars <- c(speciation_0, speciation_1, extinction_0, extinction_1, transition_0to1, transition_1to0)
simTree <- tree.bisse(Pars, max.taxa = maxN, max.t = maxT)

str(simTree)
stateTable <- table(simTree$tip.state)
stateTable / sum(stateTable)
setwd('/Users/calybking/Desktop/Evolution/Tasks/Task_10')
Q2

Frequenceis<- c('State 0', 'State 1')
Data <- matrix(c(0.55,0.68,0.61,0.633,0.644,0.45,0.35,0.645,0.568,0.755,0.422,0.544), nrow=2, ncol=6, byrow=TRUE)
data
Difference <- c(0.03,0.05,0.1,0.15,0.02)
Freq1<- c(0.32,0.4,0.43,0.33,0.33,0.568)
Freq0<- c(0.68,0.68,0.69,0.75,0.645,0.40)

pdf('Q1.pdf', height=6, width=6)
barplot(Data, names.arg=Difference, main= 'Changes in Frequency Based on Variation', xlab = 'Difference of Diversification Rate', ylab ='Frequency', beside=TRUE, col= c('blue', 'red'))
dev.off
Q2= State one never made it to 0 even when state 0 increased. 
Frequencies <- c('State 0', 'State 1')
Data <- matrix(c(0.83,0.8,0.95,0.80,0.65,0.99,0.925,0.922,0.965,0.965,0.998,0.45,0.15,0.16,0.25,0.66,0.456,0.852,0.455,0.66,0.12,0.16,0.645,0.985,0.054,0.065,0.256,0.0455), nrow=2, ncol=14, byrow=TRUE)
Difference <- c(0.05,0.05,0,0,0,0.1,0.1,0.1,0.2,0.2,0.2,0.3,0.3,0.3,0.45,0.45,0.45)
pdf('Q2.pdf', height=10, width=10)
barplot(Data, names.arg=Difference, main='How Close to 0 State 1 Can Get', xlab='Difference in Diversification Rate', ylab='Frequencies', col='red', 'black')

Recieve argument is of width is not num,eric or logical. Do not understand why. 

legend('top', Frequencies, fill='red', 'black')

Q3= The net diversification changes but the variation and deviation is low. 
Data<- read.csv('Users/calybking/Desktop/Evolution/Tasks/Task_10/q3_data.csv')
VarianceMatrix<- c(Var1, Var2, Var3)
Variance Matrix]]
Trial <-c(1,2,3)
barplot(VarianceMatrix, names.arg=Trial, main='Variance of Freq.(Each Trial)', ylim=c(0.0.5), xlab= 'Trial Number', ylab='Variacne in Freq', col='blue')

cd: /Users/calybking/Desktop/Evolution/Tasks/Task_10

