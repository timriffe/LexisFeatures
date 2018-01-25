
I.am.Tim <- FALSE
if (I.am.Tim){
	setwd("/home/tim/git/LexisFeatures")
	MxcResults <- local(get(load("/home/tim/Dropbox/compH/Reproduction/Data/Mxclong_results.RData")))
	
# All cause mortality.
	library(reshape2)
	
	HumpMales   <- acast(MxcResults[MxcResults$Sex == "m" & MxcResults$Cause == 0, ], Age~Year, value.var = "Hump")
	HumpMalesSm <- acast(MxcResults[MxcResults$Sex == "m" & MxcResults$Cause == 0, ], Age~Year, value.var = "HumpSm")
	
	write.csv(HumpMales, file = "data/csv_matrix/use_case_1/HumpMales.csv")
	write.csv(HumpMalesSm, file = "data/csv_matrix/use_case_1/HumpMalesSm.csv")
	
# tidy variables
#	use case
#	use case modifier (e.g. sample size based on)
#	age
#	year
#	value (z, fill
	
	HumpMalesTidy         <- melt(HumpMales, varnames = c("age","year"))
	HumpMalesTidy$sex     <- "males"
	HumpMalesTidy$usecase <- 1
	HumpMalesTidy$modifier <- "all-cause. smooth over age but not year"
	
	HumpMalesSmTidy         <- melt(HumpMalesSm, varnames = c("age","year"))
	HumpMalesSmTidy$sex     <- "males"
	HumpMalesSmTidy$usecase <- 1
	HumpMalesSmTidy$modifier <- "all-cause. smoothed with MortSmooth"
	
	write.csv(HumpMales, file = "data/csv_tidy/use_case_1/HumpMales.csv")
	write.csv(HumpMalesSm, file = "data/csv_tidy/use_case_1/HumpMalesSm.csv")
}





