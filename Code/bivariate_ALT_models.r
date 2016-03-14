#'---
#'title: "Life and Time - PxVx - Bivariate Models"
#'output:
#'  html_document:
#'    toc: true
#'---

#+ echo=F, warning=F, message=F
library(MplusAutomation)
library(knitr)
library(dplyr)
library(tidyr)
library(semPlot)
library(printr)
library(stringr)
library(ggplot2)
library(broom)
opts_chunk$set(echo=F, message=F, warning=F, dev='svg')

#'
#' # Intro
#'
#' **Template for model comparisons:**
#'
#'![](bi_model.png "Template for bivartiate models")
#'
#' The above diagram captures all 4 possible models combinations. The black
#' is the univariate ALT model, and the blue adds all paths we estimate for the 
#' bivariate ALT. Red paths show the paths constrained to be 0 in the 
#' mean-only (fixed slope) model. Letter labels indicate paths constrained
#' to be equal.
#'

# # Set working directory to that which contains Code, Data, etc
# setwd('E:/Projects/lnt_pxvx/')

loadBiFN<-'../Rez/biMods.RData'

# # Create Models
# createModels('code/PxVx_BiTemplate.inp')
# 
# setwd('E:/Projects/lnt_pxvx/Rez/bivariate')
# runModels(recursive = T)

# # Read models
# setwd('../Rez/bivariate')
# saveBiFN<-'../biMods.RData'
# biModelOut<-readModels(recursive = T,
# 			filefilter='ALT.*')
# biModelOut_df <- data_frame(model=biModelOut)
# save(biModelOut_df,file=saveBiFN)
# setwd('../../Code')

load(loadBiFN)
load('../Rez/winningUniModels.RData')

summaries <- biModelOut_df %>% rowwise %>%
  do({
    aSummary <- .[[1]]$summaries
    aSummaryDF <- as_data_frame(aSummary)
    aSummaryDF$numWarnings <- length(.[[1]]$warnings)
    aSummaryDF$stdErrorWarn <- any(grepl('STANDARD ERRORS COULD NOT BE COMPUTED', 
					 .[[1]]$warnings))
    aSummaryDF$numErrors <- length(.[[1]]$errors)
    aSummaryDF
  }) %>%
  extract(Title, 
          c('sample', 
	    'modelTypeP', 'pVar', 
	    'modelTypeV', 'vVar'),
          'PxVx Univariate - (\\w+) (\\w+) ([\\w_]+) with (\\w+) ([\\w_]+)') %>%
  mutate(modelNum=1:n())

# biModelOut_df[[1]][grep('Nat.*\\.I_O.*MVS_mc.*plin\\.vmeanonlyV',names(biModelOut_df[[1]]))][[1]]$error

convSum <- summaries %>% 
	select(sample, pVar, vVar,
	       modelTypeP, modelTypeV, numErrors, stdErrorWarn) %>%
	unite(modCombo, modelTypeP, modelTypeV) %>%
	mutate(modComboText=str_replace(modCombo, 
				    '(Lin|Mean)\\w*_(Lin|Mean)\\w*',
				    'P \\1 - V \\2'),
	       modComboScore=(numErrors==0)*(1+!stdErrorWarn))

#'
#' ## What are the best models that converge?
#'
#' In the below figures, each bar represents the 'model score' of 
#' a model combination for a bivariate ALT model. A score of 2
#' indicates that there were no problems, 1 indicates that
#' there were warnings about the standard error being computed
#' (e.g., with MLF instead of MLR), and 0 indicates that there
#' were errors (non-convergence). 	
#' 
#' The tables show the winning model giving preference to 
#' the full bivariate random slope ALT model, even if it was
#' estimated with MLF instead of MLR (this is not very common in the 
#' national sample, but more so in the college sample).
#'
#' The model for the personality variable comes first, so 'Lin_MeanOnly' 
#' means 'full linear ALT model for personality variable, fixed mean slope
#' ALT model for value variable.'
#'
#' ### National
#'

modelComboSelection_l <- convSum %>% 
	group_by(sample, pVar, vVar) %>%
	do({
		scores <- .$modComboScore
		names(scores) <- .$modCombo
		scores['Lin_Lin'] <- scores['Lin_Lin']*100
		scores[c('Lin_MeanOnly','MeanOnly_Lin')] <- scores[c('Lin_MeanOnly','MeanOnly_Lin')]*10
		maxScore <- max(scores)
		bestModelCombo <- paste(names(scores)[scores==maxScore],
					collapse=' ')
		data_frame(sample=.$sample[[1]],
			   pVar=.$pVar[[1]],
			   vVar=.$vVar[[1]],
			   bestCombo=bestModelCombo)
	}) 
	
modelComboSelection <- modelComboSelection_l %>%
	spread(vVar, bestCombo)
			
#'	
modelComboSelection %>% ungroup %>%
	filter(sample=='Nat') %>%
	select(-sample) %>%
	kable
#'		

#+fig.width=8, fig.height=15, fig.cap='National Sample'
convSum %>% ungroup %>%
	filter(sample=='Nat') %>%
# 	slice(1:(8*9*4)) %>%
	ggplot(aes(x=1, y=modComboScore))+
	geom_bar(aes(fill=modComboText, y=modComboScore), 
		    stat='identity',
		    position=position_dodge())+
	facet_grid(pVar~vVar)+
	theme(axis.text.x=element_text(angle=(360-45), hjust=0))+
	labs(x='Value', color='Model Combo',
	     y='Model Score\n(higher is better)')+
	theme(panel.background=element_rect(fill='#555555'))+
	scale_x_continuous(breaks=NULL)+scale_y_continuous(breaks=NULL)

#'
#' ### College
#'
#'
modelComboSelection %>% ungroup %>%
	filter(sample=='Col') %>%
	select(-sample) %>%
	kable
#'
#+fig.width=8, fig.height=15, fig.cap='College Sample'
convSum %>% ungroup %>%
	filter(sample=='Col') %>%
# 	slice(1:(8*9*4)) %>%
	ggplot(aes(x=1, y=modComboScore))+
	geom_bar(aes(fill=modComboText, y=modComboScore), 
		    stat='identity',
		    position=position_dodge())+
	facet_grid(pVar~vVar)+
	theme(axis.text.x=element_text(angle=(360-45), hjust=0))+
	labs(x='Value', color='Model Combo',
	     y='Model Score\n(higher is better)')+
	theme(panel.background=element_rect(fill='#555555'))+
	scale_x_continuous(breaks=NULL)+scale_y_continuous(breaks=NULL)

#'
#' ## Non-Lin_Lin Models: Univariate best-fit
#'

winnersByCriterion <- winnersByCriterion %>% ungroup %>%
	mutate(sample=ifelse(sample=='Inf', 'Nat', sample))


winNames <- c('AIC', 'BIC', 'LL')
names(winNames) <- paste0(winNames,'_P')
winnersByCriterionP <- winnersByCriterion %>% rename_(.dots=winNames)
names(winNames) <- paste0(winNames,'_V')
winnersByCriterionV <- winnersByCriterion %>% rename_(.dots=winNames)

modelComboSelection_l %>% 
	filter(bestCombo != 'Lin_Lin') %>%
	left_join(winnersByCriterionP, by=c('sample'='sample', 'pVar'='variable')) %>%
	left_join(winnersByCriterionV, by=c('sample'='sample', 'vVar'='variable')) %>%
	kable()

#'
#' # Parameter plots
#'


# biModelOut_df[[1]][[1]]$parameters$unstandardized

paramsummaries <- biModelOut_df %>% rowwise %>%
	do({
		if(length(.[[1]]$errors)==0){
			someParams <- .[[1]]$parameters$unstandardized
			someParams.df <- as_data_frame(someParams) %>%
				mutate(est_se=as.numeric(ifelse(est_se == '*********', NA, est_se)))
			someParams.df$Title <- as.character(.[[1]]$summaries$Title)
		} else {
			someParams.df <- data_frame(Title=as.character(.[[1]]$summaries$Title))
		}
		someParams.df
	}) %>%
  extract(Title, 
          c('sample', 
	    'modelTypeP', 'pVar', 
	    'modelTypeV', 'vVar'),
          'PxVx Univariate - (\\w+) (\\w+) ([\\w_]+) with (\\w+) ([\\w_]+)') %>%
mutate(paramstatement=paste(paramHeader, param, sep='.'),
       paramgroup=str_replace(paramstatement, 
			      '^([ABCDSI]|Means|Intercepts|Variances|Residual\\.Variances).*?\\.(ON|WITH)*\\.*([ABCDIS]).*',
			      '\\1 \\2 \\3'),
       withoron=str_detect(paramstatement, '\\.(WITH|ON)\\.'),
       firstVar=str_replace(paramstatement,'[ABCDIS](.*)\\.(WITH|ON)\\.[ABCDIS].*','\\1'),
       secondVar=str_replace(paramstatement,'[ABCDIS].*\\.(WITH|ON)\\.[ABCDIS](.*)','\\2'),
       bivPathType=ifelse(!is.na(firstVar) & !is.na(secondVar) & withoron,
	    		  ifelse(firstVar==secondVar,
				     'Within Var',
 				     'Across Var'),
			  'Other'),
       bivPathDir=ifelse(str_detect(paramstatement, '\\.ON\\.'),
			 ifelse(str_to_upper(firstVar)==str_to_upper(pVar),
				'Target: Pers',
				'Target: Val'),
			 NA)) %>%
	unite(modelCombo, modelTypeP, modelTypeV)
		
# select(paramsummaries,paramstatement,paramgroup, bivPathType, bivPathDir) %>%slice(1:50)%>%kable


# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#' 
#' ## National: AR paths across variable combinations and model types
#' 
#+fig.height=12, fig.width=10
paramsummaries %>%
	filter(sample=='Nat', 
	       paramgroup=='B ON A',
	       bivPathType=='Within Var') %>%
	ggplot(aes(x=as.numeric(factor(pVar)), y=est))+
	geom_hline(yintercept=0, color='red')+
	geom_line(aes(color=modelCombo), alpha=.9, size=1)+
	facet_grid(vVar~bivPathType+bivPathDir, scales='free_y')+
	scale_x_continuous(breaks=unique(as.numeric(factor(paramsummaries$pVar))),
			   labels=levels(factor(paramsummaries$pVar)))+
	theme(axis.text.x=element_text(angle=360-45, hjust=0, size=8),
	      panel.background=element_rect(fill='white'))+
	coord_cartesian(y=c(0, .3))+
	scale_colour_manual(values=cbbPalette)
#' 
#' ## National: XL paths across variable combinations and model types
#' 
#+fig.height=12, fig.width=10
paramsummaries %>%
	filter(sample=='Nat', 
	       paramgroup=='B ON A',
	       bivPathType=='Across Var') %>%
	ggplot(aes(x=as.numeric(factor(pVar)), y=est))+
	geom_line(aes(color=modelCombo), alpha=.9, size=1)+
	facet_grid(vVar~bivPathType+bivPathDir, scales='free_y')+
	scale_x_continuous(breaks=unique(as.numeric(factor(paramsummaries$pVar))),
			   labels=levels(factor(paramsummaries$pVar)))+
	theme(axis.text.x=element_text(angle=360-45, hjust=0, size=8),
	      panel.background=element_rect(fill='white'))+
	coord_cartesian(y=c(-.15, .15))+
	scale_colour_manual(values=cbbPalette)

#' 
#' ## College: AR paths across variable combinations and model types
#' 
#+fig.height=12, fig.width=10
paramsummaries %>%
	filter(sample=='Col', 
	       paramgroup=='B ON A',
	       bivPathType=='Within Var') %>%
	ggplot(aes(x=as.numeric(factor(pVar)), y=est))+
	geom_hline(yintercept=0, color='red')+
	geom_line(aes(color=modelCombo), alpha=.9, size=1)+
	facet_grid(vVar~bivPathType+bivPathDir, scales='free_y')+
	scale_x_continuous(breaks=unique(as.numeric(factor(paramsummaries$pVar))),
			   labels=levels(factor(paramsummaries$pVar)))+
	theme(axis.text.x=element_text(angle=360-45, hjust=0, size=8),
	      panel.background=element_rect(fill='white'))+
	coord_cartesian(y=c(-.1, .39))+
	scale_colour_manual(values=cbbPalette)


#' 
#' ## College: XL paths across variable combinations and model types
#' 
#+fig.height=12, fig.width=10
paramsummaries %>%
	filter(sample=='Col', 
	       paramgroup=='B ON A',
	       bivPathType=='Across Var') %>%
	ggplot(aes(x=as.numeric(factor(pVar)), y=est))+
	geom_line(aes(color=modelCombo), alpha=.9, size=1)+
	facet_grid(vVar~bivPathType+bivPathDir, scales='free_y')+
	scale_x_continuous(breaks=unique(as.numeric(factor(paramsummaries$pVar))),
			   labels=levels(factor(paramsummaries$pVar)))+
	theme(axis.text.x=element_text(angle=360-45, hjust=0, size=8),
	      panel.background=element_rect(fill='white'))+
	coord_cartesian(y=c(-.31, .31))+
	scale_colour_manual(values=cbbPalette)

#' 
#' ## National: Within-wave correlations across variable combinations and model types
#' 
#+fig.height=12, fig.width=7
paramsummaries %>%
	filter(sample=='Nat', 
	       paramgroup=='B WITH B',
	       bivPathType=='Across Var') %>%
	ggplot(aes(x=as.numeric(factor(pVar)), y=est))+
	geom_hline(yintercept=0, color='red')+
	geom_line(aes(color=modelCombo), alpha=.9, size=1)+
	facet_grid(vVar~bivPathType, scales='free_y')+
	scale_x_continuous(breaks=unique(as.numeric(factor(paramsummaries$pVar))),
			   labels=levels(factor(paramsummaries$pVar)))+
	theme(axis.text.x=element_text(angle=360-45, hjust=0, size=8),
	      panel.background=element_rect(fill='white'))+
# 	coord_cartesian(y=c(0, .3))+
	scale_colour_manual(values=cbbPalette)


#' 
#' ## College: Within-wave correlations across variable combinations and model types
#' 
#+fig.height=12, fig.width=7
paramsummaries %>%
	filter(sample=='Col', 
	       paramgroup=='B WITH B',
	       bivPathType=='Across Var') %>%
	ggplot(aes(x=as.numeric(factor(pVar)), y=est))+
	geom_hline(yintercept=0, color='red')+
	geom_line(aes(color=modelCombo), alpha=.9, size=1)+
	facet_grid(vVar~bivPathType, scales='free_y')+
	scale_x_continuous(breaks=unique(as.numeric(factor(paramsummaries$pVar))),
			   labels=levels(factor(paramsummaries$pVar)))+
	theme(axis.text.x=element_text(angle=360-45, hjust=0, size=8),
	      panel.background=element_rect(fill='white'))+
# 	coord_cartesian(y=c(0, .3))+
	scale_colour_manual(values=cbbPalette)

# 
# #' 
# #' # Comparisons
# #'
# #' The below tables test fit improvements between these three models.
# #' Because we use the `tscores` command in MPlus, the MLR estimator is used,
# #' which requires an adjustment to the -2\*Log Likelhood values before their
# #' difference is computed. For more information on this adjustment, see 
# #' the [MPlus website]( https://www.statmodel.com/chidiff.shtml) or the
# #' [UCLA website](and http://www.ats.ucla.edu/stat/mplus/faq/s_b_chi2.htm).
# #' The *p*-values are calculated for the adjusted -2\*LL difference with
# #' *df* equal to the difference in number of parameters estimated.
# #'
# #' ## All fit info
# #'
# 
# modelComparisonResults <- summaries %>% group_by(sample, variable) %>%
#   do({
# 	  AICs <- .$AIC
# 	  names(AICs) <- .$modelType
# 	  BICs <- .$BIC
# 	  names(BICs) <- .$modelType
# 	  #loglikelihood
# 	  LLs <- .$LL
# 	  names(LLs) <- .$modelType
# 	  #correction factors
# 	  CFs <- .$LLCorrectionFactor
# 	  names(CFs) <- .$modelType
# 	  #number of parameters
# 	  nPs <- .$Parameters
# 	  names(nPs) <- .$modelType
# 	  #Degrees of freedom for each comparison
# 	  model1Names <- c('AR_Int', 'AR_LinM')
# 	  model2Names <- c('AR_LinM', 'AR_Lin')
# 	  dfs <- -(nPs[model1Names] - nPs[model2Names])
# 	  #scaling correction for each comparison
# 	  cds <- (nPs[model1Names] * CFs[model1Names] - nPs[model2Names] * CFs[model2Names])/
# 		  (nPs[model1Names] - nPs[model2Names])
# 	  #Chi-square difference test statistic
# 	  TRds <- -2*(LLs[model1Names] - LLs[model2Names])/cds
# 	  PVals <- 1-pchisq(TRds, dfs)
# 	  data_frame(sample=.$sample[1],
# 		     variable=.$variable[1],
# 		     model1=model1Names,
# 		     model2=model2Names,
# 		     LLm1=LLs[model1Names],
# 		     LLm2=LLs[model2Names],
# 		     CFm1=CFs[model1Names],
# 		     CFm2=CFs[model2Names],
# 		     nPm1=nPs[model1Names],
# 		     nPm2=nPs[model2Names],
# 		     cd=cds,
# 		     TRd=TRds,
# 		     df=dfs,
# 		     PVal=PVals,
# 		     AICm1=AICs[model1Names],
# 		     BICm1=BICs[model1Names],
# 		     AICm2=AICs[model2Names],
# 		     BICm2=BICs[model2Names])
#   })
# 
# sampleNames <- c(Col='College', Nat='National', `Inf`='Informant')
# 
# #+ results='asis'
# theTables <- modelComparisonResults %>% group_by(sample, variable) %>%
# 	do({
# 		caption <- paste0(sampleNames[.$sample[[1]]], ' Sample, ',
# 				  .$variable[[1]])
# 		dataForTable <- select(., model1, model2, LLm1, LLm2, 
# 				       AICm1, AICm2, 
# 				       BICm1, BICm2, 
# 				       nPm1, nPm2, 
# 				       TRd, df, PVal)
# 		theTable <- kable(dataForTable,
# 		      		  digits=2,
# 		      		  caption=caption,
# 		      		  col.names=c('Model 1', 'Model 2', 'LL M1', 'LL M2', 
# 						    'AIC M1', 'AIC M2',
# 						    'BIC M1', 'BIC M2',
# 						    'Prms M1', 'Prms M2', 
# 						    '-2*LL $\\Delta$', '*df*', '*p*'))
# 		print(theTable)
# 		data_frame(Table=list(theTable))
# 	})
# 
# #'
# #' ## Fit $\Delta$ only
# #'
# 
# #+ results='asis'
# theTables <- modelComparisonResults %>% group_by(sample, variable) %>%
# 	do({
# 		caption <- paste0(sampleNames[.$sample[[1]]], ' Sample, ',
# 				  .$variable[[1]])
# 		dataForTable <- mutate(.,
# 				       AICdelta=AICm1 - AICm2,
# 				       BICdelta=BICm1 - AICm2) %>%
# 				select(model1, model2, 
# 				       AICdelta, 
# 				       BICdelta,
# 				       nPm1, nPm2, 
# 				       TRd, df, PVal)
# 		theTable <- kable(dataForTable,
# 		      		  digits=2,
# 		      		  caption=caption,
# 		      		  col.names=c('Model 1', 'Model 2', 
# 						    'AIC M1 - M2',
# 						    'BIC M1 - M2',
# 						    'Prms M1', 'Prms M2', 
# 						    '-2*LL $\\Delta$', '*df*', '*p*'))
# 		print(theTable)
# 		data_frame(Table=list(theTable))
# 	})
# 
# #'
# #' ## Best fitting model tally
# #'
# 
# winningModels <- modelComparisonResults %>% group_by(sample, variable) %>%
# 	do({
# 		#determine LL winner
# 		whichWins <- mutate(.,
# 				    pRace=PVal<.05,
# 				    pWinner=ifelse(pRace, model2, model1))
# 		if(sum(whichWins$pRace) %in% 2){
# 			LLwinner <- whichWins$pWinner[2]
# 		} else {
# 			LLwinner <- whichWins$pWinner[1]
# 		}
# 		#determine AIC winner
# 		AICs <- rbind(as.matrix(select(., model1, AICm1)),
# 			      as.matrix(select(., model2, AICm2)))
# 		minAIC <- min(AICs[, 2])
# 		AICwinner <- unique(AICs[AICs[, 2] == minAIC, 1])
# 		#determine BIC winner
# 		BICs <- rbind(as.matrix(select(., model1, BICm1)),
# 			      as.matrix(select(., model2, BICm2)))
# 		minBIC <- min(BICs[, 2])
# 		BICwinner <- unique(BICs[BICs[, 2] == minBIC, 1])
# 		#Wrap it up
# 		data_frame(sample=.$sample[[1]],
# 			   variable=.$variable[[1]],
# 			   criterion=c('LL', 'AIC', 'BIC'),
# 			   winningmodel=c(LLwinner, AICwinner, BICwinner))
# 	})
# 
# variableTypes <- data_frame(variable=c('BFA_AC', 'BFA_AP', 'BFA_CI', 'BFA_CO', 'BFA_EA', 'BFA_EE', 'BFA_MT', 'BFA_NV', 'BFA_NW', 'BFA_OI', 'BFA_OO', 'BFI_A6', 'BFI_C', 'BFI_E', 'bfi_hp8', 'BFI_N', 'BFI_O', 'D_SCALE', 'VRT_COL', 'HRZ_COL', 'HRZ_IND', 'MVS_mc', 'S_SCALE', 'USI', 'VRT_IND', 'aspfin'),
# 			    vartype=c(rep('p', 6),
# 				      'v',
# 				      rep('p', 10),
# 				      rep('v', 9)))
# 
# #+results='asis'
# modelTallies <- winningModels %>%
# 	filter(sample != 'Inf') %>%
# 	left_join(variableTypes) %>%
# 	group_by(sample, vartype) %>%
# 	select(criterion, winningmodel) %>%
# 	do({
# 		vartypenames <- c(p='Pers', v='Val')
# 		sampleNames <- c(Nat='National', Col='College', `Inf`='Informants')
# 		print(kable(table(select(., criterion, winningmodel)),
# 	    		    caption=paste0(sampleNames[.$sample[[1]]],
# 					   ' ',
# 					   vartypenames[.$vartype[[1]]],
# 					   ': Tally of winning models, by fit measure'),
# 			    align='l'))
# 		tidy(table(.))
# 	})
# 
# winnersByCriterion <- winningModels %>% spread(criterion, winningmodel) 
# 
# kable(winnersByCriterion,
#       col.names=c('Sample', 'Variable', 'AIC', 'BIC', '-2*LL'),
#       caption='List of which model performs best, by fit measure')
# 
# 
# #'
# #' # Parameter histograms
# #'
# 
# #+fig.width=10, fig.height=2.66, dev='svg', fig.cap='National: path estimates'
# paramsummaries %>% filter(!str_detect(paramgroup, '(Variances|Intercepts|C ON B|D ON C)'),
# 			  sample=='Nat') %>%
# 	ggplot(aes(x=est))+
# 	geom_histogram()+
# 	facet_grid(sample+modelType~paramgroup, scales='free_x')+
# 	geom_vline(xintercept=0, color='red', alpha=.5)+
# 	theme(panel.background=element_rect(fill='white'),
# 	      axis.text.x=element_text(angle=360-45))
# 
# #+fig.width=5.9, fig.height=2.66, dev='svg', fig.cap='National: variance estimates'
# paramsummaries %>% filter(str_detect(paramgroup, '(Variances)'), 
# 			  !str_detect(paramgroup, '( [CD])'),
# 			  sample=='Nat') %>%
# ggplot(aes(x=est))+
# 	geom_histogram()+
# 	facet_grid(sample+modelType~paramgroup, scales='free_x')+
# 	theme(panel.background=element_rect(fill='white'),
# 	      axis.text.x=element_text(angle=360-45))+
# 	coord_cartesian(y=c(0,4))
# 
# #+fig.width=10, fig.height=8, dev='svg', fig.cap='All: path estimates'
# paramsummaries %>% filter(!str_detect(paramgroup, '(Variances|Intercepts|C ON B|D ON C)')) %>%
# ggplot(aes(x=est))+
# 	geom_histogram()+
# 	facet_grid(sample+modelType~paramgroup, scales='free_x')+
# 	geom_vline(xintercept=0, color='red', alpha=.5)+
# 	theme(panel.background=element_rect(fill='white'),
# 	      axis.text.x=element_text(angle=360-45))
# 
# #+fig.width=5.9, fig.height=8, dev='svg', fig.cap='All: variance estimates'
# paramsummaries %>% filter(str_detect(paramgroup, '(Variances)'), 
# 			  !str_detect(paramgroup, '( [CD])')) %>%
# ggplot(aes(x=est))+
# 	geom_histogram()+
# 	facet_grid(sample+modelType~paramgroup, scales='free_x')+
# 	theme(panel.background=element_rect(fill='white'),
# 	      axis.text.x=element_text(angle=360-45))+
# 	coord_cartesian(y=c(0,15))
