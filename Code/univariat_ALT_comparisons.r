#'---
#'title: "Life and Time - PxVx - Base Model Comparisons"
#'output:
#'  html_document
#'---

#+ echo=F
library(MplusAutomation)
library(knitr)
library(dplyr)
library(tidyr)
library(semPlot)
library(printr)
opts_chunk$set(echo=F)

#'
#' **Template for model comparisons:**
#'
#'![](initial_model.png "Template for model comparisons")
#'
#' The above diagram captures all 4 models we wish to compare. Ignoring the 
#' blue portion, we have an intercept-only ALT model. We can decide whether
#' the auto-regressive paths improve fit by comparing a model where `AR` is
#' freely estimated to a model where `AR` is constrained to be 0 -- the
#' *AR_Int* versus *NoAR_Int* models in the tables below. This comparison
#' results in a difference of 1 parameter between the models.
#'
#' We can also include a latent slope (the blue portion, above) and compare
#' model fit between an intercept only, and intercept + slope model (with 
#' `AR` free or constrained). Adding the slope
#' gives us 4 new parameters to estimate: The slope mean, varriance, and 
#' covarriance with the intercept and initial status indicator (`a X`). These
#' two-parameter latent growth models are postfixed with *Lin* below (e.g., 
#' *NoAR_Lin*).
#'

# Set working directory to that which contains Code, Data, etc
#setwd('E:/Projects/LnT_Values/')

saveUniFN<-'../Rez/uniMods.RData'

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

summaries <- uniModelOut_df %>% rowwise %>%
  do({
    aSummary <- .[[1]]$summaries
    as_data_frame(aSummary)
  }) %>%
  extract(Title, 
          c('sample', 'variable', 'modelType'),
          'PxVx Univariate - (\\w+) ([\\w_]+) ([\\w_]+)')

#'
#' The below tables test fit improvements between models with and without 
#' the `AR` path constrained to 0, and with and without the latent slope.
#' because we use the `tscores` command in MPlus, the MLR estimator is used,
#' which requires an adjustment to the -2\*Log Likelhood values before their
#' difference is computed. For more information on this adjustment, see 
#' the [MPlus website]( https://www.statmodel.com/chidiff.shtml) or the
#' [UCLA website](and http://www.ats.ucla.edu/stat/mplus/faq/s_b_chi2.htm).
#' The *p*-values are calculated for the adjusted -2\*LL difference with
#' *df* equal to the difference in number of parameters estimated.
#'

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

sampleNames <- c(Col='College', Nat='National', `Inf`='Informant')

#+ results='asis'
theTables <- modelComparisonResults %>% group_by(sample, variable) %>%
	do({
		caption <- paste0(sampleNames[.$sample[[1]]], ' Sample, ',
				  .$variable[[1]])
		dataForTable <- select(., model1, model2, LLm1, LLm2, nPm1, nPm2, 
				       TRd, df, PVal)
		theTable <- kable(dataForTable,
		      		  digits=2,
		      		  caption=caption,
		      		  col.names=c('Model 1', 'Model 2', 'LL M1', 'LL M2', 
						    'Params M1', 'Params M2', 
						    'Adj -2*LL $\\Delta$', '*df*', '*p*'))
		print(theTable)
		data_frame(Table=list(theTable))
	})

