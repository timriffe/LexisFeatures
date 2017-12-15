
I.am.Tim <- FALSE
if (I.am.Tim){
	setwd("/home/tim/git/LexisFeatures/LexisFeatures")
	MxcResults <- local(get(load("/home/tim/Dropbox/compH/Reproduction/Data/Mxclong_results.RData")))
	head(MxcResults)
	
# All cause mortality.
	library(reshape2)
	
	HumpMales   <- acast(MxcResults[MxcResults$Sex == "m" & MxcResults$Cause == 0, ], Age~Year, value.var = "Hump")
	HumpMalesSm <- acast(MxcResults[MxcResults$Sex == "m" & MxcResults$Cause == 0, ], Age~Year, value.var = "HumpSm")
	
	write.csv(HumpMales, file = "Data/HumpMales.csv")
	write.csv(HumpMales, file = "Data/HumpMalesSm.csv")
}





