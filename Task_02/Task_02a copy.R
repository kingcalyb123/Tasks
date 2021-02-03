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

