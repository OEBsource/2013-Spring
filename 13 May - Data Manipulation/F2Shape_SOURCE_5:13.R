### to reduce PC scores to just first three PC scores for all individuals
setwd("/Users/Home/Dropbox/Amherst/Courses/SOURCE/Spring2013/Moira")

PCscores <- read.csv("MoiraPCScores.csv", header=F)
head(PCscores)
rowVec <- seq(from=1,to=465,by=3)
PCscoresNoLJ <- PCscores[rowVec,1:3]
write.csv(PCscoresNoLJ, file="PCscoresF2_FlapAll_noLJ_edited.csv")

###centroid

UJcentroid <- read.csv("MoiraFishLandmark.csv",header=F)
head(UJcentroid,15)
str(UJcentroid)

# to reduce data file to just x and y coordinates
n <- 1549
i <- 1
output <- NULL

while(i < n){
  coordinates <- seq(i, i+5, 1)
  output <- c(output,coordinates)
  i <- i+10
}

UJint <- UJcentroid[output,]

UJint[,1]<-as.character(UJint[,1])
UJint[,1]<-as.numeric(UJint[,1])

str(UJint)

# to find average x and y values for each individual
i <- 1
n <- 930

Xind <- NULL
Yind <- NULL

Xmeans <- NULL
Ymeans <- NULL

while (i < n){
  
  Xind <- mean(UJint[i:(i+5),1])
  Yind <- mean(UJint[i:(i+5),2])
  
  Xmeans <- c(Xmeans, Xind)
  Ymeans <- c(Ymeans, Yind)
  
  i <- i+6
}

Xmeans
Ymeans

###to find the distance of each landmark to the average for each individual
#creates a vector of average X and Y the length of raw coordinates
outputX <- NULL
tempX <- NULL
for(i in 1:(length(Xmeans))){
  tempX <- rep(Xmeans[i],6)
  outputX <- c(outputX,tempX)
}

outputY <- NULL
tempY <- NULL
for(i in 1:(length(Ymeans))){
  tempY <- rep(Ymeans[i],6)
  outputY <- c(outputY,tempY)
}

#subtracts each landmark from the average and squares it
distX<-(outputX-UJint$V1)^2
distY<-(outputY-UJint$V2)^2

distX
distY

#Pythagorean Theorem to find actual distance of each landmark to the average
distZ <- distX + distY
distZ

#to actually calculate centroid size
i <- 1 
n <- 930

tempCentroid <- NULL
CentroidSize <- NULL

while(i < n){
  tempCentroid <- sqrt(sum(distZ[i:(i+5)]))
  CentroidSize <- c(CentroidSize,tempCentroid)
  i <- i+6
}

CentroidSize
write.csv(CentroidSize,"CentroidSize_FlapAll_upperJaw.csv")
