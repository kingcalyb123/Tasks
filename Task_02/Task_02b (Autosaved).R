setwd('~/Desktop/Evolution/Tasks/Task_02')
Data <- read.csv('https://jonsmitchell.com/data/beren.csv', stringsAsFactors=F)
write.csv(Data, 'rawdata.csv', quote=F)
length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(Data)
Data[1,]
Data[2,]
Data[1:3,]
Data[1:3, 4]
Data[1:5, 1:3]
Each one shows the data present for the particular day or set of dates that we put into R. 
We would type in Data[257] to find all the data that would be present on the 257th observation.
Feeds <- which(Data[,9] == 'bottle')
berenMilk <- Data[Feeds,]
head(berenMilk)
There are two sets of 6 rows, with each row containing the same numbers. The rows represent the dates and times that the child was given their bottle in the first set of 6 rows. The second set represents the amount of formula and caregiver that provided the bottle. All of the rows have a purpose of showing times that a bottle was administered to the child.
Feeds <- which(Data[,'event'] == 'bottle')
Feeds <- which(Data$event == 'bottle')
All of the functions use the (which) function which helps pinpoint certain characteristics. By using Data and event afterwards, we are able to target a specific set of data and show R that we are searching for an event within this Data. By adding ==bottle at the end, it tells R that we are looking for which events include bottle. I am convinced all the functions do the same because despite the different in ()'s and $', we are able to still pinpoint identical information. 

dayID <- apply(Data, 1, function(x) paste(x[1:3], collapse='-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data$age <- dateID - dateID[which(Data$event == 'birth')]
head(Data)
beren2 <- Data
beren3 <- beren2[order(beren2$age) ,]
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)

Question 1- I am having trouble finding the values given for weight and nap time which means they would not suffice as testable hypotheses. The amount of fluid given to Beren in a bottle is given repeatedly so whether the data supports the hypothesis or not, hypothesis 3 gives an assumption that can easily be tested. 
Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value[Feeds])
avgMilk

The units for this avgMilk is in ounces. 
The value column was used from the beren3 object to show the numeric values that were correlated with the "bottle" data that was previously used to create the "Feeds" object. 

The square brackets used around Feeds is to indicate the elements that are contained within the "Feeds" vector. So we used them to identify the mean of all the objects within the Feeds object. 

avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
avgFeed
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
boxplot(beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab= "who gave the bottle", ylab= "amount of milk consumed (oz)")
?par
las = edited axis labels
mar = the margins which should be used
mgp= the margins that should be used for the title and axis labels
tck= edited the length of tick marks

par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck= -0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch =16, xlab= "age in days", ylab= "ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
pdf("r02b-totalMilkByDay.pdf", height=4, width=4)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=mean(totalFeed), lty=2, col='red')
dev.off()
Question 2= The graph is very difficult to interpret. I am not exactly sure why it is so difficult to interpret but i would say it, in part, is due to such high intervals on the x-axis which causes a lot of the data to mashed together. 
sourceTime.pdf("https://jonsmitchell.com/code/plotFxn02b.R")

Self Quiz: The graph off of ecampus is easier to read. It shows that as the day goes forward, beren drinks more milk almost always. It also shows that the older Beren gets, he seems to drinks more milk by an earlier time in the day that he did when he was younger. For example, at 496 days of age, he drank almost 10 oz of milk by 11 o'clock whereas he had only consumed around 4 oz at this time when he was 126 days old. The graph we created also shows Beren's consumption of milk in ounces but the x axis goes by age in days rather than time of day. This is important because it makes the data jumble together rather than spreading it out and making it easier to decipher. The both show similar data but the one that we created through R is much more compressed than the comparison. 

unique(beren3$event)

