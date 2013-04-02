####GGplot2####
#Making a fake dataset to see how ggplot2 works to make a boxplot
A<-rep("A", 10)
B<-rep("B",10)

AandB<-c(A,B)

C<-rep("C",5)
D<-rep("D", 5)

CandD<-c(C,D,C,D)

All<-cbind(AandB, CandD)

#What do our factors currently look like?
All

#Let's add in some data.
Data1<-seq(1,20,1)

#Stick it all together.
Final<-cbind(All,Data1)

#GGplot can't use a matrix, only a dataframe.
Final<-as.data.frame(Final)

#Thinks our data column is a factor
Final[,3]<-as.numeric(Final[,3])


#install.packages("ggplot2") #If you need to.
require(ggplot2)

ggplot(data = Final, aes(x = AandB, y = Data1)) +  geom_boxplot(aes(fill = CandD), width = 0.8) + theme_grey()


####Error Bars####
#There is no native script in R on how to add error bars to a plot.
#This is a DIY example of how you can do it.

superpose.eb <- function (x, y, ebl, ebu = ebl, lh = 0.1,   ...) 
{
  #ebu is the length of the upper error bar; assumed to be a vector with same length of x and y.
  #ebl is the length of the lower error bar; assumed to be a vector with same length of x and y
  #2*lh is a the length of the tick at the end of the error bar
  #it is assumed that the limits of the axes on the existing plot are 
  #sufficiently large to accomodate the error bars.
  
  segments(x, y + ebu, x, y - ebl, ...)
  segments(x - lh , y + ebu, x + lh , y + ebu, 
           ...)
  segments(x - lh, y - ebl, x + lh , y - ebl,  ...)
}
barplot(c(20,30),ylim=c(0,50))->bar
superpose.eb(bar,c(20,30),c(10,15),c(10,15))
