#'---
#'title: "Life and Time - PxVx - Value Stability"
#'output:
#'  html_document:
#'    toc: true
#'---

#+ echo=F
library(MplusAutomation)
library(knitr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
opts_chunk$set(echo=F, message=F, warning=F)

# Set working directory to that which contains Code, Data, etc
# setwd('E:/Projects/lnt_pxvx/')

loadStbFN<-'../Rez/stability.RData'

# # Create Models
# createModels('Code/value_stability_invariance_template.inp')
# createModels('Code/value_stability_invariance_template-Col.inp')
# 
# setwd('E:/Projects/lnt_pxvx/Rez/stability')
# runModels(recursive = T)
# 
# # Read models
# # setwd('../Rez/stability')
# saveStbFN<-'../stability.RData'
# stbModelOut<-readModels(recursive = T,
# 			filefilter='stability.*')
# stbModelOut_df <- data_frame(model=stbModelOut)
# save(stbModelOut_df,file=saveStbFN)
# setwd('../../Code')

load(loadStbFN)

summaries <- stbModelOut_df %>% rowwise %>%
  do({
    aSummary <- .[[1]]$summaries
    as_data_frame(aSummary)
  }) %>%
  extract(Title, 
          c('sample', 'variable', 'modelType'),
          'Stability of values - (\\w+) ([\\w_]+) ([\\w_]+);')

paramsummaries <- stbModelOut_df %>% rowwise %>%
	  do({
	    someParams <- .[[1]]$parameters$stdyx.standardized
	    someParams.df <- as_data_frame(someParams) %>%
		    mutate(est_se=as.numeric(ifelse(est_se == '*********', NA, est_se)))
	    someParams.df$Title <- as.character(.[[1]]$summaries$Title)
	    someParams.df
	  })%>%
	extract(Title, 
		c('sample', 'variable', 'modelType'),
		'Stability of values - (\\w+) ([\\w_]+) ([\\w_]+);') %>%
	mutate(paramstatement=paste(paramHeader, param, sep='.'),
	       paramgroup=str_replace(paramstatement, 
				      '^([ABCDS]|Means|Intercepts|Variances|Residual\\.Variances).*?\\.(ON|WITH)*\\.*([ABCDIS]).*',
				      '\\1 \\2 \\3'))

summaryTableLong <- summaries %>% 
	select(sample, variable, modelType, Parameters, 
	       ChiSqM_Value, ChiSqM_DF, ChiSqM_PValue, 
	       AIC, BIC) %>%
	gather(stat, value, ChiSqM_Value, ChiSqM_DF, AIC, BIC) 

summaryDiffs <- summaryTableLong %>%
	group_by(sample, variable, stat) %>%
	do(data_frame(value=.$value[.$modelType=='group']-
	   .$value[.$modelType=='longitudinal'])) %>%
	mutate(modelType='difference') 


#'
#' # Model Fit
#'
#'
#+results='asis'
nada <- bind_rows(summaryTableLong, summaryDiffs) %>%
	spread(stat, value) %>%
	mutate(modelType=factor(modelType, levels=c('longitudinal', 'group', 'difference'))) %>%
	arrange(sample, variable, modelType) %>%
	mutate(ChiSqM_PValue=ifelse(is.na(ChiSqM_PValue),
				    1-pchisq(ChiSqM_Value, ChiSqM_DF),
				    ChiSqM_PValue)) %>%
	group_by(sample) %>%
	do({
		asamp <- unique(.$sample)
		atable <- as_data_frame(.) %>% select(variable, modelType, Parameters, 
			     ChiSqM_Value, ChiSqM_DF, ChiSqM_PValue,
			     AIC, BIC) %>%
			mutate(Parameters=ifelse(is.na(Parameters),
						 '',
						 Parameters)) %>%
			kable(digits=2, caption=paste0('Sample: ', asamp))
		print(atable)
		cat('\n\n\n')
		data_frame(table=list(atable), sample=asamp)
	})

#'
#' # Group-invariant stability
#'
#' Correlation estimates assuming longitudinal invariance in college sample, 
#' and longitudinal and group invariance in the national sample.	
#'	

paramsummaries %>% filter(modelType=='group'| sample=='Col',
			  paramgroup %in% c('A WITH B', 'A WITH C', 'A WITH D'),
			  Group=='D2' | sample=='Col') %>%
	select(sample, variable, est, paramgroup) %>%
	group_by(sample, variable) %>%
	spread(paramgroup, est) %>%
	kable(digits=2)

#'
#' # Longitudinal-invariant stability
#'
#' Correlation estimates assuming longitudinal invariance only in the national sample.	
#'	
#+results='asis'

nada <- paramsummaries %>% filter(modelType=='longitudinal',
			  paramgroup %in% c('A WITH B', 'A WITH C', 'A WITH D'),
			  sample=='Nat') %>%
	select(variable, Group, est, paramgroup) %>%
	group_by(variable, Group) %>%
	spread(paramgroup, est) %>%
	mutate(Group=str_replace(Group, 'D([2345])', '\\10\'s')) %>%
	group_by(variable) %>%
	do({
		avar <- unique(.$variable)
		atable <- as_data_frame(.) %>% select(-variable) %>% 
			kable(digits=2, caption=paste0('Variable: ', avar))
		print(atable)
		cat('\n\n\n')
		data_frame(table=list(atable), variable=avar)
	})
