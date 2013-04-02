# Ned Young
# 18 December, 2012
rm(list=ls())
setwd("/Users/nedyoung/Botany/4genomes/R-graphics/divergence")
library(ggplot2)
library(scales)
vals <- read.table("BHA_sau2.valstrim_c", header = TRUE, sep = "\t")
ggplot(vals, aes(Window, Divergence, Coverage, Cent_pos)) 						  + 
	geom_line(aes(Window, Divergence), colour="black", lwd=0.1) 					+ 
	xlab("Position (bp)") 														                    +
	ylab("Divergence (Coverage in red)") 				 					   	            +
	ggtitle("BHA vs sau2") 													 	                    +
	geom_line(aes(Window, Coverage/100 + 0.01), colour="red",lwd=0.1)   	+ 
	facet_wrap(~ Chrom, ncol = 1) 											 	                +  	
  theme(axis.text.y = element_text(size = 5))		 							          +
  # Mean								 	                		
  geom_hline(yintercept=0.00243, colour="purple", linetype=2, lwd=0.2 )   		 +  					
  # Mean + 2 SD
  geom_hline(yintercept=0.00841, colour="blue", linetype=2, lwd=0.2 )     		 +    			
  geom_point(aes(Cent_pos), colour= I("Goldenrod1"), shape = I(15), y= 0.01)	 + 
  # right-hand axis scale
  geom_segment(aes(x=43880000, y=0.01, xend=44300000, yend = 0.01), colour="red", lwd=0.2)       +
  geom_segment(aes(x=43880000, y=0.015, xend=44300000, yend = 0.015), colour="red", lwd=0.2)     +
  geom_segment(aes(x=43880000, y=0.02, xend=44300000, yend = 0.02), colour="red", lwd=0.2)       +
  geom_text(aes(x=44800000, y=0.01), colour="red", label = "0.0", family="Helvetica",  size=1.0) +
  geom_text(aes(x=44800000, y=0.019), colour="red", label = "1.0", family="Helvetica", size=1.0) #+  

ggsave(file = "divcov5_BHA_sau2_gg.pdf")
	
