#ggplot2 source outline
#load libraries

####price vs. carat####
#native R version

#ggplot version


####histogram of carat####
#load data

#load specified data

#instead of making a new ggObject, we can use the original one and specify the data in the layer


####storing layers to R objects####
#sleep total vs. sleep rem


#plot best fit line

#storing data layers

#storing geom layers

#setting color vs. mapping color
ggRem_Total+geom_point(color="blue")
ggRem_Total+geom_point(aes(color="blue"))
ggRem_Total+geom_point(aes(color=vore))


####specifying custom colors####

#####groups in aesthetics####

#multiple groups, one aesthetic
#separate data into groups, but render the same way
#height vs age
library(nlme)

#different groups on different layers
#useful if you want to plot summaries based on different levels of aggregation
#different levels might have their own aesthetics

#adding lines
ggOxboysLines+geom_smooth(method="lm",se=F,size=2)
#why don't we have to specify the grouping in this case?


####combining geoms and stats####
ggDiamonds<-ggplot(diamonds,aes(x=carat))+xlim(0,3)

ggDiamonds+stat_bin(
  aes(ymax=..count..), 
  binwidth=0.1,
  geom="area")


ggDiamonds+stat_bin(
  aes(size = ..density..), 
  binwidth = 0.1,
  geom = "point", 
  position="identity"
)

ggDiamonds+stat_bin(
  aes(y = 1, fill = ..count..), 
  binwidth = 0.1,
  geom = "tile", 
  position="identity"
)

####distributions####
head(diamonds)
qplot(depth, data=diamonds, geom="density", xlim = c(54, 70))

qplot(depth, data=diamonds, geom="density", xlim = c(54, 70),
      fill = cut,
      alpha=0.2)

qplot(depth, data=diamonds, geom="density", xlim = c(54, 70),
      fill = cut,
      alpha=I(0.2))

