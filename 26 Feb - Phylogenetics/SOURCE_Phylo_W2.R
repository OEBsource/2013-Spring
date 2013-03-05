####Phylogenetics SOURCE####
#Week Two#
#Note that in RStudio if you put four #'s by some words it becomes an index point. This means that you can click the # tag at the bottom of this window, just above 'Console' and select a particular phrase.
#There's normally two things that you'll need when you come to perfoming comparative methods in R, a tree file and some trait data (continious or discrete).

#First, let's focus on the tree.
#We'll need to load in some packages that allow us to manipulate our tree and the data.

#Do install.packages("Your desired package name") if you are yet to have these packages
library(geiger)
library(laser)
library(phytools)


####Exciting Stuff####
###Now let's read in a real tree.
#Set the working directory
setwd("/Users/Home/Dropbox/Amherst/Courses/SOURCE/Spring2013/PhyloLesson")

#Read in the tree and data
anoleTree<-read.tree("anolis.phy")
anoleData<-read.csv("anolisDataAppended.csv", row.names=1)
#Rownames at the end allows us to pick the column that contains the taxa names (in this case column 1) and place them as row names.

#name.check sees if there if anything does not match
name.check(anoleTree, anoleData)

#This matches your data to the tip labels, it's required if they are not in the same order.
anoleData <- anoleData[match(anoleTree $tip.label,rownames(anoleData)),]
#I did this just as an example, it actually wasn't required.


#Quick checks of what is going on in the data.
anoleTree
head(anoleData)
dim(anoleData)
plot(anoleData)


#Make a separate vector for body size svl snout-vent length that we can test on our phylogeny.
svl<-anoleData[,1]
#Need to but the species names back in.
names(svl)<-rownames(anoleData)
svl


###Phylogenetic Signal####
#This term is used generally to denote whether a character evolves slowly enough to have the same state in closely related taxa as opposed to varying randomly.
#require = if the package is already loaded then nothing will happen
require(picante)

#Calculate Blomberg's K, which is a measure of phylogenetic signal for your trait.
phylosignal(svl, anoleTree)
#Is there phylo signal, yes, p>0.001, basically because of common ancestory.


####Ancestral State reconstruction####
#The actual ancestral state reconstruction
svlAnc<-ace(svl, anoleTree)
svlAnc

#Gives you a list of the ancestral states at each internal node.

#Can then plot this on the tree
plot(anoleTree, cex=0.5)
#Will scale the nodes by how big the states are
nodelabels(pch=21, cex=exp(svlAnc$ace)/40, bg="red")

##Reconstructing a discrete trait.
#We can do this for discrete characters, here we will use "ecomorph". It is a factor.
ecomorph<-anoleData[,"ecomorph"]
names(ecomorph)<-rownames(anoleData)

#Reconstruct ace's
ecomAce<-ace(ecomorph, anoleTree, type="discrete")
#Slight different outputs than the discrete. Shows you the liklihood of each discrete character being present at each internal node. There are 99 internal nodes here.
ecomAce$lik.anc

#Proportional likliehoods are in the matrix
#Gives you a pie chart of the likelihoods for each ecomorph
plot(anoleTree, cex=0.5)
nodelabels(pie= ecomAce $lik.anc, cex=0.5, piecol=1:7)
legend("bottomleft", fill=1:7, legend=levels(ecomorph), cex=0.5)



####Disparity####
#Let's look at disparity through time using our phylogeny
dtt.full(anoleTree, anoleData[,1:5])

#Dotted line is the line of Brownain Motion (takes a while to plot).
#Solid line is your data. What we're actually seeing is a snapshot through the clade's evolutionary history, so at the far left is the base of the tree and the far right is the tip of the tree.
#It's interesting if you start to see the solid line break away from the dotted line as then you have more (or perhaps less) morphological variation than would be expected by chance.



####Phylogenetic Independent Contrasts####
###Is there a correlation between two variables that we measured?
#Let's try attidude and awesomeness. Surely there's a correlation??!!
att<-anoleData[,"attitude"]
names(att)<-rownames(anoleData)

stdModel<-lm(anoleData[,"awesomeness"]~anoleData[,"attitude"])
summary(stdModel)

plot(anoleData[,"awesomeness"]~anoleData[,"attitude"], pch=19)
intercept<-0.11315
slope<-0.81373

abline(intercept, slope)
#You can also do this to save time and get te same result.
abline(stdModel)

##The standard model gives us a significant correlation, but what happens when we take evolutionary history into account?
# repeat the above analysis using PGLS
require(nlme)

pglsModel<-gls(awesomeness~attitude, correlation=corBrownian(1, anoleTree), data=anoleData)
summary(pglsModel)
#Still significant, great news!


#Now we can do something cool - compare the normal regression with the phylogenetic version
plot(anoleData[,"awesomeness"]~anoleData[,"attitude"], pch=19)
intercept<-0.11315
slope<-0.81373
abline(intercept, slope)

##Let's plot the intercept and slope from the PGLS model.
picintercept<-0.1122378
picslope<-0.7450389

#Appears to be little difference between the two regression lines.
abline(picintercept, picslope, col="red")
#Simpler method...
abline(pglsModel)

legend("topleft", lwd=1, col=c("black", "red"), legend=c("Standard regression", "PGLS"))



####Phylogenetic ANOVA####
##I want to see whether there is a difference in SVL means between four different ecomorphs.
anoleEcomorphData<-anoleData[anoleData$ecomorph=="CG" | anoleData$ecomorph =="T" | anoleData$ecomorph =="TC" | anoleData$ecomorph =="TG",]

##There's some odd bug here that I've yet to figure out. When I run the final analysis it seems to 'remember' the other factors that were in the original dataset. To cheat my way round this I made a new dataset so R never sees them.
write.csv(anoleEcomorphData, "anoleEcomorphData.csv")
anoleEcomorph<-read.csv("anoleEcomorphData.csv", header=T, row.names=1)

#Pruning a tree. We do this step as we dont have data for a number of the tips in our tree.
#I didn't want to look at all the different ecomorphs, just a select few. Hence our tree must now be 'pruned' as we have removed data from a number of the species that are on the tree.
Pruning <-treedata(anoleTree, anoleEcomorph)

#'Pruning' now contains a list of our tree and data, I will now extract that and place into new arguements.
anoleTreeIslands <-Pruning$phy
anoleEco <-Pruning$data

#Checking everything is in the correct order.
name.check(anoleTreeIslands, anoleEco)


#Associate with species names for the category variable
Ecomorphs <- anoleEco$ecomorph
names(Ecomorphs) <- rownames(anoleEco)

# Correlate tip labels with names for the continuous (response) varaible
SVLs <- anoleEco$SVL
names(SVLs) <- rownames(anoleEco)

# Run phylogenetic ANOVA. Note that you need the package Phytools for this step.
EcoANOVA <- phylANOVA(anoleTreeIslands, Ecomorphs, SVLs, nsim=1000, posthoc=TRUE, p.adj="bonferroni")
EcoANOVA


####Tasks####
####1, Ancestral State of Mass for Caniforms.####
CaniPhy <- read.tree("caniform.phy")
CaniData <- read.csv("caniform_mass.csv", header=T, row.names=1)

name.check(CaniPhy, CaniData)

plot(CaniPhy)
CaniData

CanMass<-CaniData[,1]
#Need to but the species names back in.
names(CanMass)<-rownames(CaniData)
CanMass

MassAnc<-ace(CanMass, CaniPhy)
MassAnc

#Gives you a list of the ancestral states at each internal node.

#Can then plot this on the tree
plot(CaniPhy, cex=0.5)
#Will scale the nodes by how big the states are
nodelabels(pch=21, cex=MassAnc$ace/2, bg="blue")
nodelabels()
#What is the ancestral state value?


####2, PICs for Geospiza, WingL against TarsusL ####

# 2,
geotree <- read.nexus("geospiza.nex")
geodata <- read.table("geospizadata.txt", header=T, row.names=1)

name.check(geotree, geodata)

plot(geotree)

Pruning <-treedata(geotree, geodata)

geotree <-Pruning$phy
geodata <-Pruning$data

Wing<-geodata[,"wingL"]
names(Wing)<-rownames(geodata)
Tars<-geodata[,"tarsusL"]
names(Tars)<-rownames(geodata)


stdModel<-lm(geodata[,"wingL"]~geodata[,"tarsusL"])
summary(stdModel)

plot(geodata[,"wingL"]~geodata[,"tarsusL"], pch=19)
intercept<-2.3846
slope<-0.6188

abline(intercept, slope)

require(nlme)

pglsModel<-gls(Wing~Tars, correlation=corBrownian(1, geotree), data=geodata)
summary(pglsModel)

#Compare both
plot(geodata[,"wingL"]~geodata[,"tarsusL"], pch=19)
intercept<-2.3846
slope<-0.6188
abline(intercept, slope)

##Let's plot the intercept and slope from the PGLS model.
picintercept<-0.9546792
picslope<-1.0764312
abline(picintercept, picslope, col="red")

legend("bottomright", lwd=1, bty="n", col=c("black", "red"), legend=c("Standard regression", "PGLS"))



##Another way to do it, note that you have to use a no intercept model.
#Hence there is no intercept section on the summary page.
wingPic <- pic(Wing, geotree)
tarsPic <- pic(Tars, geotree)
picModel <- lm(wingPic~tarsPic-1)
summary(picModel)



####Fitting models of trait evolution####
#Let's load in some more data. Darwin's Finches!
#Note that the tree we're loading in this time is a nexus file with a tree in it. This means that there is a translation box in the nexus file telling R what each of the taxon names are in the tree.
geotree <- read.nexus("geospiza.nex")
geodata <- read.table("geospizadata.txt", header=T, row.names=1)


plot(geotree)

wL<- geodata[,1] 
names(wL)<- row.names(geodata)

geotree2<- drop.tip(geotree, "olivacea")
geodata <- geodata[match(geotree2$tip.label,rownames(geodata)),]

BMfit <- fitContinuous(geotree2, wL, model="BM")
BMfit

OUfit <- fitContinuous(geotree2, wL, model="OU")
OUfit

plot(geotree2)
axisPhylo()