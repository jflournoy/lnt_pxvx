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
# createModels('Code/PxVx_BiTemplate.inp')
# 
# setwd('E:/Projects/lnt_pxvx/Rez/bivariate')
# runModels(recursive = T)

# # Read models
# setwd('../Rez/bivariate')
# saveBiFN<-'../biMods.RData'
# biModelOut<-readModels(recursive = T,
# 		       filefilter='ALT.*')
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
	       modelTypeP, modelTypeV, numErrors, stdErrorWarn,
	       AIC, BIC) %>%
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
#' ## National: Effects on personality across variable combinations and model types
#' 
#+fig.height=35, fig.width=10
paramsummaries %>%
	filter(sample=='Nat', 
	       paramgroup=='B ON A',
	       bivPathDir=='Target: Pers',
	       !str_detect(pVar, '^I_')) %>%
	ggplot(aes(x=as.numeric(factor(vVar)), y=est))+
	geom_hline(yintercept=0, color='red')+
	geom_line(aes(color=modelCombo), alpha=.9, size=1)+
	facet_grid(pVar~bivPathType+bivPathDir)+
	scale_x_continuous(breaks=unique(as.numeric(factor(paramsummaries$vVar))),
			   labels=levels(factor(paramsummaries$vVar)))+
	theme(axis.text.x=element_text(angle=360-45, hjust=0, size=8),
	      panel.background=element_rect(fill='white'))+
# 	coord_cartesian(y=c(0, .3))+
	scale_colour_manual(values=cbbPalette)

#' 
#' ## National: Effects on values across variable combinations and model types
#' 
#+fig.height=10, fig.width=10
paramsummaries %>%
	filter(sample=='Nat', 
	       paramgroup=='B ON A',
	       bivPathDir=='Target: Val',
	       !str_detect(pVar, '^I_')) %>%
	ggplot(aes(x=as.numeric(factor(pVar)), y=est))+
	geom_hline(yintercept=0, color='red')+
	geom_line(aes(color=modelCombo), alpha=.9, size=1)+
	facet_grid(vVar~bivPathType+bivPathDir)+
	scale_x_continuous(breaks=unique(as.numeric(factor(paramsummaries$pVar))),
			   labels=levels(factor(paramsummaries$pVar)))+
	theme(axis.text.x=element_text(angle=360-45, hjust=0, size=8),
	      panel.background=element_rect(fill='white'))+
# 	coord_cartesian(y=c(0, .3))+
	scale_colour_manual(values=cbbPalette)


#' 
#' ## National Informants: Effects on personality across variable combinations and model types
#' 
#+fig.height=15, fig.width=10
paramsummaries %>%
	filter(sample=='Nat', 
	       paramgroup=='B ON A',
	       bivPathDir=='Target: Pers',
	       str_detect(pVar, '^I_')) %>%
	ggplot(aes(x=as.numeric(factor(vVar)), y=est))+
	geom_hline(yintercept=0, color='red')+
	geom_line(aes(color=modelCombo), alpha=.9, size=1)+
	facet_grid(pVar~bivPathType+bivPathDir)+
	scale_x_continuous(breaks=unique(as.numeric(factor(paramsummaries$vVar))),
			   labels=levels(factor(paramsummaries$vVar)))+
	theme(axis.text.x=element_text(angle=360-45, hjust=0, size=8),
	      panel.background=element_rect(fill='white'))+
# 	coord_cartesian(y=c(0, .3))+
	scale_colour_manual(values=cbbPalette)

#' 
#' ## National Informants: Effects on values across variable combinations and model types
#' 

infpVarFactor <- factor(paramsummaries$pVar[str_detect(paramsummaries$pVar, '^I_')])

#+fig.height=10, fig.width=10
paramsummaries %>%
	filter(sample=='Nat', 
	       paramgroup=='B ON A',
	       bivPathDir=='Target: Val',
	       str_detect(pVar, '^I_')) %>%
	ggplot(aes(x=as.numeric(factor(pVar)), y=est))+
	geom_hline(yintercept=0, color='red')+
	geom_line(aes(color=modelCombo), alpha=.9, size=1)+
	facet_grid(vVar~bivPathType+bivPathDir)+
	scale_x_continuous(breaks=unique(as.numeric(infpVarFactor)),
			   labels=levels(infpVarFactor))+
	theme(axis.text.x=element_text(angle=360-45, hjust=0, size=8),
	      panel.background=element_rect(fill='white'))+
# 	coord_cartesian(y=c(0, .3))+
	scale_colour_manual(values=cbbPalette)


#' 
#' ## College: Effects on personality across variable combinations and model types
#' 
#+fig.height=25, fig.width=10
paramsummaries %>%
	filter(sample=='Col', 
	       paramgroup=='B ON A',
	       bivPathDir=='Target: Pers') %>%
	ggplot(aes(x=as.numeric(factor(vVar)), y=est))+
	geom_hline(yintercept=0, color='red')+
	geom_line(aes(color=modelCombo), alpha=.9, size=1)+
	facet_grid(pVar~bivPathType+bivPathDir)+
	scale_x_continuous(breaks=unique(as.numeric(factor(paramsummaries$vVar))),
			   labels=levels(factor(paramsummaries$vVar)))+
	theme(axis.text.x=element_text(angle=360-45, hjust=0, size=8),
	      panel.background=element_rect(fill='white'))+
# 	coord_cartesian(y=c(0, .3))+
	scale_colour_manual(values=cbbPalette)

#' 
#' ## College: Effects on values across variable combinations and model types
#' 
#+fig.height=10, fig.width=10
paramsummaries %>%
	filter(sample=='Col', 
	       paramgroup=='B ON A',
	       bivPathDir=='Target: Val') %>%
	ggplot(aes(x=as.numeric(factor(pVar)), y=est))+
	geom_hline(yintercept=0, color='red')+
	geom_line(aes(color=modelCombo), alpha=.9, size=1)+
	facet_grid(vVar~bivPathType+bivPathDir)+
	scale_x_continuous(breaks=unique(as.numeric(factor(paramsummaries$pVar))),
			   labels=levels(factor(paramsummaries$pVar)))+
	theme(axis.text.x=element_text(angle=360-45, hjust=0, size=8),
	      panel.background=element_rect(fill='white'))+
# 	coord_cartesian(y=c(0, .3))+
	scale_colour_manual(values=cbbPalette)


#'
#' ## National Personality: Stability of AR component
#'

#+fig.height=30, fig.width=5
paramsummaries %>%
	filter(sample=='Nat',
	       paramgroup=='B ON A',
	       bivPathType=='Within Var',
	       modelCombo=='Lin_Lin',
	       bivPathDir=='Target: Pers') %>%
	ggplot(aes(x=factor(vVar), y=est))+
	geom_hline(yintercept=0, color='red')+
	geom_point()+
	geom_errorbar(aes(ymin=est-1.96*se, ymax=est+1.96*se), width=.5)+
	facet_grid(pVar~1)+
	theme(axis.text.x=element_text(angle=360-45, hjust=0, size=8),
	      panel.background=element_rect(fill='white'))+
 	coord_cartesian(y=c(-.3, .6))+
	scale_colour_manual(values=cbbPalette)

#'
#' ## National Values: Stability of AR component
#'

#+fig.height=10, fig.width=5
paramsummaries %>%
	filter(sample=='Nat',
	       paramgroup=='B ON A',
	       bivPathType=='Within Var',
	       modelCombo=='Lin_Lin',
	       bivPathDir=='Target: Val') %>%
	ggplot(aes(x=factor(pVar), y=est))+
	geom_hline(yintercept=0, color='red')+
	geom_point()+
	geom_errorbar(aes(ymin=est-1.96*se, ymax=est+1.96*se), width=.5)+
	facet_grid(vVar~1)+
	theme(axis.text.x=element_text(angle=360-45, hjust=0, size=8),
	      panel.background=element_rect(fill='white'))+
 	coord_cartesian(y=c(-.1, .3))+
	scale_colour_manual(values=cbbPalette)


#'
#' ## College Personality: Stability of AR component
#'

#+fig.height=23, fig.width=5
paramsummaries %>%
	filter(sample=='Col',
	       paramgroup=='B ON A',
	       bivPathType=='Within Var',
	       modelCombo=='Lin_Lin',
	       bivPathDir=='Target: Pers') %>%
	ggplot(aes(x=factor(vVar), y=est))+
	geom_hline(yintercept=0, color='red')+
	geom_point()+
	geom_errorbar(aes(ymin=est-1.96*se, ymax=est+1.96*se), width=.5)+
	facet_grid(pVar~1)+
	theme(axis.text.x=element_text(angle=360-45, hjust=0, size=8),
	      panel.background=element_rect(fill='white'))+
 	coord_cartesian(y=c(-.4, .6))+
	scale_colour_manual(values=cbbPalette)

#'
#' ## College Values: Stability of AR component
#'

#+fig.height=10, fig.width=5
paramsummaries %>%
	filter(sample=='Col',
	       paramgroup=='B ON A',
	       bivPathType=='Within Var',
	       modelCombo=='Lin_Lin',
	       bivPathDir=='Target: Val') %>%
	ggplot(aes(x=factor(pVar), y=est))+
	geom_hline(yintercept=0, color='red')+
	geom_point()+
	geom_errorbar(aes(ymin=est-1.96*se, ymax=est+1.96*se), width=.5)+
	facet_grid(vVar~1)+
	theme(axis.text.x=element_text(angle=360-45, hjust=0, size=8),
	      panel.background=element_rect(fill='white'))+
 	coord_cartesian(y=c(-.9, .9))+
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
