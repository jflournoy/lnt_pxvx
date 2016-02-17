library(MplusAutomation)
library(knitr)
library(dplyr)
library(tidyr)
library(semPlot)

# Set working directory to that which contains Code, Data, etc
#setwd('E:/Projects/LnT_Values/')
setwd('/mnt/328392813EB3641A/Projects/lnt_pxvx/')

saveUniFN<-'Rez/uniMods.RData'

# # Create Models
# createModels('code/PxVx_UniTemplate.inp')
# 
# setwd('E:/Projects/LnT_Values/Rez/univariate')
# runModels(recursive = T)

# # Read models
# setwd('E:/Projects/LnT_Values/Rez/model_syntax')
# uniModelOut<-readModels(recursive = T,
#                         filefilter='ALT.*')
# uniModelOut_df <- data_frame(model=uniModelOut)
# save(uniModelOut_df,file=saveUniFN)

load(saveUniFN)

str(uniModelOut_df[1,1][[1]][[1]]$summaries, max.level=2)

summaries <- uniModelOut_df %>% rowwise %>%
  do({
    aSummary <- .[[1]]$summaries
    as_data_frame(aSummary)
  }) %>%
  extract(Title, 
          c('sample', 'variable', 'modelType'),
          'PxVx Univariate - (\\w+) ([\\w_]+) ([\\w_]+)')

#This method for comparison of models is taken from
# https://www.statmodel.com/chidiff.shtml

modelComparisonResults <- summaries %>% group_by(sample, variable) %>%
  do({
	  #loglikelihood
	  LLs <- .$LL
	  names(LLs) <- .$modelType
	  #correction factors
	  CFs <- .$LLCorrectionFactor
	  names(CFs) <- .$modelType
	  #number of parameters
	  nPs <- .$Parameters
	  names(nPs) <- .$modelType
	  #Degrees of freedom for each comparison
	  model1Names <- c('NoAR_Int', 'NoAr_Lin', 'AR_Int', 'NoAR_Int')
	  model2Names <- c('AR_Int', 'AR_Lin', 'AR_Lin', 'NoAr_Lin')
	  dfs <- -(nPs[model1Names] - nPs[model2Names])
	  #scaling correction for each comparison
	  cds <- (nPs[model1Names] * CFs[model1Names] - nPs[model2Names] * CFs[model2Names])/
		  (nPs[model1Names] - nPs[model2Names])
	  #Chi-square difference test statistic
	  TRds <- -2*(LLs[model1Names] - LLs[model2Names])/cds
	  PVals <- 1-pchisq(TRds, dfs)
	  data_frame(sample=.$sample[1],
		     variable=.$variable[1],
		     model1=model1Names,
		     model2=model2Names,
		     LLm1=LLs[model1Names],
		     LLm2=LLs[model2Names],
		     CFm1=CFs[model1Names],
		     CFm2=CFs[model2Names],
		     nPm1=nPs[model1Names],
		     nPm2=nPs[model2Names],
		     cd=cds,
		     TRd=TRds,
		     df=dfs,
		     PVal=PVals)
  })

kable(modelComparisonResults)
