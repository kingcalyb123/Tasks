Diaper <- which(Data[,9] == 'diaper')
berenDiaper <- Data[Diaper,]
head(berenDiaper)
Diaper <- which(Data[,'event'] == 'diaper')
Diaper <- which(Data$event == 'diaper')
avgDiaper <- mean(beren3$value[Diaper])
avgDiaper <- tapply(beren3$value[Diaper], beren3$age[Diaper], mean)
varDiaper <- tapply(beren3$value[Diaper], beren3$age[Diaper], var)
totalDiaper <- tapply(beren3$value[Diaper], beren3$age[Diaper], sum)
numDiaper <- tapply(beren3$value[Diaper], beren3$age[Diaper], length) 
cor(beren3$value[Diaper], beren3$age[Diaper])
cor.test(beren3$value[Diaper], beren3$age[Diaper])


diaper <- which(Data[,9] == 'diaper')
berendiaper <- Data[diaper, ]
head(berendiaper)
diaper <- which(Data[, 'event'] == 'diaper')
diaper <- which(Data$event =='diaper')
avgdiaper <- mean(beren3$value[diaper])
avgdiaper <- tapply(beren3$value[diaper], beren3$age[diaper], mean)
vardiaper <- tapply(beren3$value[diaper], beren3$age[diaper], var)
totaldiaper <- tapply(beren3$value[diaper], beren3$age[diaper], sum)
numdiaper <- tapply(beren3$value[diaper], beren3$age[diaper], length)
cor(beren3$value[diaper], beren3$age[diaper])
cor.test(beren3$value[diaper], beren3$age[diaper])
There happens to be an error because there are only two events labeled diaper in the data sheet and neither of them have any unit or value given so there is no way to test this hypothesis that diapers decrease with age in Beren. 

In this case i am changing my hypothesis to Beren will consume more solids with age. 

solids <- which(Data[,9] =='solids')
berensolids <- Data[solids, ]
head(berensolids)
solids <- which(Data[, 'event'] == 'solids')
solids <- which(Data$event == 'solids')
avgsolids <- mean(beren3$value[solids])
avgsolids <- tapply(beren3$value[solids], beren3$age[solids], mean)
varsolids <- tapply(beren3$value[solids], beren3$age[solids], var)
totalsolids <- tapply(beren3$value[solids], beren3$age[solids], sum)
numsolids <- tapply(beren3$value[solids], beren3$age[solids], length)
cor(beren3$value[solids], beren3$age[solids])
solids
totalsolids <- sum(solids)
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck= -0.01)
plot(as.numeric(names(totalsolids)), totalsolids, type ="b", pch=16, xlab ="age in days", ylab = "amount of solids")

Although i had trouble, i was able to conclude that the hypothesis that derens diaper count was lower the older he got by looking at the data sheet. THe solids hypothesis gave me trouble as well. 

Feedback would be greatly appreciated so i know what to do better next time. 

