#'---
#'title: "Life and Time - PxVx - Results Summary"
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
library(data.table)
opts_chunk$set(echo=F, message=F, warning=F, dev='svg')

#'
#' # Intro
#'
#' **Template for models:**
#'
#'![](bi_model.png "Template for bivartiate models")
#'
#' The above diagram captures all 4 possible models combinations. The black
#' is the univariate ALT model, and the blue adds all paths we estimate for the 
#' bivariate ALT. Red paths show the paths constrained to be 0 in the 
#' mean-only (fixed slope) model. Letter labels indicate paths constrained
#' to be equal.
#'

loadBiFN<-'../Rez/biMods.RData'
load(loadBiFN)

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

convSum <- summaries %>% 
	select(sample, pVar, vVar,
	       modelTypeP, modelTypeV, numErrors, stdErrorWarn,
	       AIC, BIC) %>%
	unite(modCombo, modelTypeP, modelTypeV) %>%
	mutate(modComboText=str_replace(modCombo, 
				    '(Lin|Mean)\\w*_(Lin|Mean)\\w*',
				    'P \\1 - V \\2'),
	       modComboScore=(numErrors==0)*(1+!stdErrorWarn))

#'
#' ## What models are these results from?
#'
#' ### National
#'

modelComboSelection_l <- convSum %>% 
	group_by(sample, pVar, vVar) %>%
	do({	
		AICs <- .$AIC
		names(AICs) <- .$modCombo
		BICs <- .$BIC
		names(BICs) <- .$modCombo
		scores <- .$modComboScore
		names(scores) <- .$modCombo
		scores['Lin_Lin'] <- scores['Lin_Lin']*100
		scores[c('Lin_MeanOnly','MeanOnly_Lin')] <- scores[c('Lin_MeanOnly','MeanOnly_Lin')]*10
		maxScore <- max(scores)
		bestModelCombo <- paste(names(scores)[scores==maxScore],
					collapse=' ')
		bestModelComboAICs <- paste(AICs[scores==maxScore], collapse=' ')
		bestModelComboBICs <- paste(BICs[scores==maxScore], collapse=' ')
		data_frame(sample=.$sample[[1]],
			   pVar=.$pVar[[1]],
			   vVar=.$vVar[[1]],
			   bestCombo=bestModelCombo,
			   AIC_biv=bestModelComboAICs,
			   BIC_biv=bestModelComboBICs)
	}) 
	
modelComboSelection <- modelComboSelection_l %>%
	select(-AIC_biv, -BIC_biv) %>%
	spread(vVar, bestCombo)
			
#'	
modelComboSelection %>% ungroup %>%
	filter(sample=='Nat') %>%
	select(-sample) %>%
	kable

#'
#' ### College
#'
#'
modelComboSelection %>% ungroup %>%
	filter(sample=='Col') %>%
	select(-sample) %>%
	kable

load('../Rez/winningUniModels.RData')

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
#' In this case, we should choose to use the model with restricted slope variance for HRZ_IND.
#'

#'
#' # Parameter Summaries
#'

paramsummaries <- biModelOut_df %>% rowwise %>%
	do({
		if(length(.[[1]]$errors)==0){
			someParams <- .[[1]]$parameters$unstandardized
			someParams.df <- as_data_frame(someParams) %>%
				mutate(est_se=as.numeric(ifelse(est_se == '*********', NA, est_se)))
			someParams.df$Title <- as.character(.[[1]]$summaries$Title)
			someParams.df$N <- .[[1]]$summaries$Observations
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

pVarInfNames <- c(I_A="BFI_A6",
		  I_C="BFI_C",
		  I_D="D_SCALE",
		  I_E="BFI_E",
		  I_N="BFI_N",
		  I_O="BFI_O",
		  I_S="S_SCALE")

pVarNames <- c(S_SCALE="Social Self-Regulation",
	       BFI_C="Conscientiousness BFI",
	       BFA_CI="Industriousness BFAS", 
	       BFA_CO="Orderliness BFAS", 
	       bfi_hp8="Honesty/Propriety BFI", 
	       BFI_A6="Agreeableness-Six BFI", 
	       BFA_AC="Compassion BFAS", 
	       BFA_AP="Politeness BFAS", 
	       BFI_N="Neuroticism BFI", 
	       BFA_NV="Volatility BFAS", 
	       BFA_NW="Withdrawal BFAS", 
	       D_SCALE="Dynamism", 
	       BFI_E="Extraversion BFI", 
	       BFA_EA="Assertiveness BFAS", 
	       BFA_EE="Enthusiasm BFAS", 
	       BFI_O="Openness BFI", 
	       BFA_OI="Intellect BFAS", 
	       BFA_OO="Openness BFAS")

paramsummaries %>% as.data.table %>% 
	mutate(sample=ifelse(str_detect(pVar, '^I_'),
			     'Inf',
			     sample),
	       pVar=ifelse(str_detect(pVar, '^I_'),
			   pVarInfNames[pVar],
			   pVar),
	       ScaleName=factor(pVarNames[pVar], levels=pVarNames)) %>%
	filter(bivPathType=='Across Var',
	       paramgroup=='B ON A',
	       ifelse(pVar=='D_SCALE' & vVar=='HRZ_IND' & sample=='Col',
		      modelCombo=='Lin_MeanOnly',
		      modelCombo=='Lin_Lin')) %>%
	select(ScaleName, vVar, sample, bivPathDir, N, est, se, pval) %>% 
	gather(parameter, value, -(ScaleName:bivPathDir)) %>%
	unite(Sample_EfDir_Param, sample, bivPathDir, parameter, sep=' ') %>%
	spread(Sample_EfDir_Param, value) %>%
	arrange(ScaleName) %>%
	group_by(vVar) %>% kable

# paramsummaries
# unique(paramsummaries$bivPathDir)
# unique(paramsummaries$bivPathType)
# unique(paramsummaries$paramgroup)

		
