##Phylogenetics SOURCE##
#Week one#
#There's normally two things that you'll need when you come to perfoming comparative methods in R, a tree file and some trait data (continious or discrete).

#First, let's focus on the tree.
#We'll need to load in some packages that allow us to manipulate our tree and the data.

#Do install.packages("Your desired package name") if you are yet to have these packages
library(geiger)
library(laser)
library(phytools)

#You should always check what your tree file looks like before reading it in.
#Normally we would use read.tree, but we can actually see what a tree looks like to R.

TREE <- "(((TaxonA:4,TaxonB:4):3,TaxonC:7):5,TaxonD:12);"
TREE
#There are three different elements that are important here. The Taxon, the number and the parentheses.
#The taxon would represent your species name, the numbers represent your branch lengths, and the parenteses represent how your Taxa are related. The semi-colon at the end shows that is the final taxa in your tree.

#Let's plot it to get a better idea.
My.Tree <- read.tree(text = TREE)
plot(My.Tree)

#The branch lengths here are in millions of years, let's show that.
axisPhylo()

#How is R seeing our tree?
class(My.Tree)
#Should be as a phylo

My.Tree
str(My.Tree)
#A tree is basically a list, i'll discuss a few of these here.
#$edge This is a matrix with 2 columns and a number of rows that correspond to the size of you tree, in this case, 6.  Each number in the matrix represents a node.
#$tip.label Shows the names of our taxa The order of the taxa is important, as the order of a vector of character states will correspond to the order of taxa.

#For example
My.Tree$edge

#We can visualize how these nodes and tips are connected by plotting them.
nodelabels()
tiplabels()
#See the first row, it says 5 to 6, look at node 5 it connects node 6, follow this down the tree and you can see how each node and tip is connected.

#If you want to re-root the phylogeny, you can 
rootedphylogeny <- root(My.Tree, "TaxonB")
plot(rootedphylogeny, cex=0.7)

##Tells us at what node the most recent common ancestor of these two taxa is.
mrca(My.Tree)["TaxonA", "TaxonC"]
#It's at node 6, we can check this on the plot we just created.

DropTree<-drop.tip(My.Tree, "TaxonC")
plot(DropTree)

#The multi2di command will break polytomies in random order. In the anole tree there isn't any polytomies, I thought I wopuld just show it as an example.
dichotomousphylogeny <- multi2di(My.Tree, random = TRUE)


###Plotting Trees###
#We can dispaly the tree in different ways
plot(My.Tree, cex=0.7) #cex means character expansions
plot.phylo(My.Tree, type="radial", no.margin=T, cex=0.7)
plot.phylo(My.Tree, type="cladogram", no.margin=T, cex=0.7)

#Called the function ladderize that is input within the plotting function.
plot.phylo(ladderize(My.Tree, right=T), type="cladogram")