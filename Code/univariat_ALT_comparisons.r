#'---
#'title: "Life and Time - PxVx - Base Model Comparisons"
#'output:
#'  html_document:
#'    toc: true
#'---

#+ echo=F
library(MplusAutomation)
library(knitr)
library(dplyr)
library(tidyr)
library(semPlot)
library(printr)
library(stringr)
library(ggplot2)
library(broom)
opts_chunk$set(echo=F, message=F, warning=F)

#' # Intro
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
# setwd('E:/Projects/lnt_pxvx/')

loadUniFN<-'../Rez/uniMods.RData'

# # Create Models
# createModels('code/PxVx_UniTemplate.inp')
# 
# setwd('E:/Projects/lnt_pxvx/Rez/univariate')
# runModels(recursive = T)

# # Read models
# setwd('../Rez/univariate')
# saveUniFN<-'../uniMods.RData'
# uniModelOut<-readModels(recursive = T,
# 			filefilter='ALT.*')
# uniModelOut_df <- data_frame(model=uniModelOut)
# save(uniModelOut_df,file=saveUniFN)
# setwd('../../Code')

load(loadUniFN)

summaries <- uniModelOut_df %>% rowwise %>%
  do({
    aSummary <- .[[1]]$summaries
    as_data_frame(aSummary)
  }) %>%
  extract(Title, 
          c('sample', 'variable', 'modelType'),
          'PxVx Univariate - (\\w+) ([\\w_]+) ([\\w_]+)')

paramsummaries <- uniModelOut_df %>% rowwise %>%
	  do({
	    someParams <- .[[1]]$parameters$unstandardized
	    someParams.df <- as_data_frame(someParams) %>%
		    mutate(est_se=as.numeric(ifelse(est_se == '*********', NA, est_se)))
	    someParams.df$Title <- as.character(.[[1]]$summaries$Title)
	    someParams.df
	  })%>%
	  extract(Title, 
		  c('sample', 'variable', 'modelType'),
		  'PxVx Univariate - (\\w+) ([\\w_]+) ([\\w_]+)') %>%
	  mutate(paramstatement=paste(paramHeader, param, sep='.'),
		 paramgroup=str_replace(paramstatement, 
					'^([ABCDS]|Means|Intercepts|Variances|Residual\\.Variances).*?\\.(ON|WITH)*\\.*([ABCDIS]).*',
					'\\1 \\2 \\3'))
	


#' 
#' # Comparisons
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
#' ## All fit info
#'

modelComparisonResults <- summaries %>% group_by(sample, variable) %>%
  do({
	  AICs <- .$AIC
	  names(AICs) <- .$modelType
	  BICs <- .$BIC
	  names(BICs) <- .$modelType
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
	  model1Names <- c('AR_Int', 'AR_LinM')
	  model2Names <- c('AR_LinM', 'AR_Lin')
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
		     PVal=PVals,
		     AICm1=AICs[model1Names],
		     BICm1=BICs[model1Names],
		     AICm2=AICs[model2Names],
		     BICm2=BICs[model2Names])
  })

sampleNames <- c(Col='College', Nat='National', `Inf`='Informant')

#+ results='asis'
theTables <- modelComparisonResults %>% group_by(sample, variable) %>%
	do({
		caption <- paste0(sampleNames[.$sample[[1]]], ' Sample, ',
				  .$variable[[1]])
		dataForTable <- select(., model1, model2, LLm1, LLm2, 
				       AICm1, AICm2, 
				       BICm1, BICm2, 
				       nPm1, nPm2, 
				       TRd, df, PVal)
		theTable <- kable(dataForTable,
		      		  digits=2,
		      		  caption=caption,
		      		  col.names=c('Model 1', 'Model 2', 'LL M1', 'LL M2', 
						    'AIC M1', 'AIC M2',
						    'BIC M1', 'BIC M2',
						    'Prms M1', 'Prms M2', 
						    '-2*LL $\\Delta$', '*df*', '*p*'))
		print(theTable)
		data_frame(Table=list(theTable))
	})

#'
#' ## Fit $\Delta$ only
#'

#+ results='asis'
theTables <- modelComparisonResults %>% group_by(sample, variable) %>%
	do({
		caption <- paste0(sampleNames[.$sample[[1]]], ' Sample, ',
				  .$variable[[1]])
		dataForTable <- mutate(.,
				       AICdelta=AICm1 - AICm2,
				       BICdelta=BICm1 - AICm2) %>%
				select(model1, model2, 
				       AICdelta, 
				       BICdelta,
				       nPm1, nPm2, 
				       TRd, df, PVal)
		theTable <- kable(dataForTable,
		      		  digits=2,
		      		  caption=caption,
		      		  col.names=c('Model 1', 'Model 2', 
						    'AIC M1 - M2',
						    'BIC M1 - M2',
						    'Prms M1', 'Prms M2', 
						    '-2*LL $\\Delta$', '*df*', '*p*'))
		print(theTable)
		data_frame(Table=list(theTable))
	})

#'
#' ## Best fitting model tally
#'

winningModels <- modelComparisonResults %>% group_by(sample, variable) %>%
	do({
		#determine LL winner
		whichWins <- mutate(.,
				    pRace=PVal<.05,
				    pWinner=ifelse(pRace, model2, model1))
		if(sum(whichWins$pRace) %in% 2){
			LLwinner <- whichWins$pWinner[2]
		} else {
			LLwinner <- whichWins$pWinner[1]
		}
		#determine AIC winner
		AICs <- rbind(as.matrix(select(., model1, AICm1)),
			      as.matrix(select(., model2, AICm2)))
		minAIC <- min(AICs[, 2])
		AICwinner <- unique(AICs[AICs[, 2] == minAIC, 1])
		#determine BIC winner
		BICs <- rbind(as.matrix(select(., model1, BICm1)),
			      as.matrix(select(., model2, BICm2)))
		minBIC <- min(BICs[, 2])
		BICwinner <- unique(BICs[BICs[, 2] == minBIC, 1])
		#Wrap it up
		data_frame(sample=.$sample[[1]],
			   variable=.$variable[[1]],
			   criterion=c('LL', 'AIC', 'BIC'),
			   winningmodel=c(LLwinner, AICwinner, BICwinner))
	})

#+results='asis'
modelTallies <- winningModels %>%
	group_by(sample) %>%
	select(criterion, winningmodel) %>%
	do({
		sampleNames <- c(Nat='National', Col='College', `Inf`='Informants')
		print(kable(table(select(., criterion, winningmodel)),
	    		    caption=paste0(sampleNames[.$sample[[1]]],
					   ': Tally of winning models, by fit measure'),
			    align='l'))
		tidy(table(.))
	})

winnersByCriterion <- winningModels %>% spread(criterion, winningmodel) 

kable(winnersByCriterion,
      col.names=c('Sample', 'Variable', 'AIC', 'BIC', '-2*LL'),
      caption='List of which model performs best, by fit measure')


#'
#' ## Parameter histograms
#'

#+fig.width=10, fig.height=2.66, dev='svg', fig.cap='National: path estimates'
paramsummaries %>% filter(!str_detect(paramgroup, '(Variances|Intercepts|C ON B|D ON C)'),
			  sample=='Nat') %>%
	ggplot(aes(x=est))+
	geom_histogram()+
	facet_grid(sample+modelType~paramgroup, scales='free_x')+
	geom_vline(xintercept=0, color='red', alpha=.5)+
	theme(panel.background=element_rect(fill='white'),
	      axis.text.x=element_text(angle=360-45))

#+fig.width=5.9, fig.height=2.66, dev='svg', fig.cap='National: variance estimates'
paramsummaries %>% filter(str_detect(paramgroup, '(Variances)'), 
			  !str_detect(paramgroup, '( [CD])'),
			  sample=='Nat') %>%
ggplot(aes(x=est))+
	geom_histogram()+
	facet_grid(sample+modelType~paramgroup, scales='free_x')+
	theme(panel.background=element_rect(fill='white'),
	      axis.text.x=element_text(angle=360-45))+
	coord_cartesian(y=c(0,4))

#+fig.width=10, fig.height=8, dev='svg', fig.cap='All: path estimates'
paramsummaries %>% filter(!str_detect(paramgroup, '(Variances|Intercepts|C ON B|D ON C)')) %>%
ggplot(aes(x=est))+
	geom_histogram()+
	facet_grid(sample+modelType~paramgroup, scales='free_x')+
	geom_vline(xintercept=0, color='red', alpha=.5)+
	theme(panel.background=element_rect(fill='white'),
	      axis.text.x=element_text(angle=360-45))

#+fig.width=5.9, fig.height=8, dev='svg', fig.cap='All: variance estimates'
paramsummaries %>% filter(str_detect(paramgroup, '(Variances)'), 
			  !str_detect(paramgroup, '( [CD])')) %>%
ggplot(aes(x=est))+
	geom_histogram()+
	facet_grid(sample+modelType~paramgroup, scales='free_x')+
	theme(panel.background=element_rect(fill='white'),
	      axis.text.x=element_text(angle=360-45))+
	coord_cartesian(y=c(0,15))
