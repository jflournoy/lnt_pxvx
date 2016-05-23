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

source('./ggplottheme.r')

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
#' ## Absolute fit of longitudinal invariance models
#'
#'
#+results='asis'
nada <- summaries %>%
	filter(modelType=='longitudinal') %>%
	group_by(sample) %>%
	do({
		asamp <- unique(.$sample)
		atable <- as_data_frame(.) %>% 
			mutate_each(funs(gsub('^0(\\.\\d+)',
					      '\\1', 
					      sprintf('%.2f', .))),
				    CFI, TLI, 
				    RMSEA_Estimate, 
				    RMSEA_90CI_LB, 
				    RMSEA_90CI_UB, 
				    SRMR) %>%
			mutate(RMSEA=paste0(RMSEA_Estimate,
					    ' [',
					    RMSEA_90CI_LB,
					    ',',
					    RMSEA_90CI_UB,
					    ']')) %>%
			select(variable,
			       RMSEA,
			       SRMR,
			       CFI, TLI) %>%
			rename(`RMSEA [90% CI]`=RMSEA) %>%
			kable(digits=2, caption=paste0('Sample: ', asamp),
			      align=c('l', rep('r', 4)))
		print(atable)
		cat('\n\n\n')
		data_frame(table=list(atable), sample=asamp)
	})

#'
#' ## Absolute fit of group invariance models
#'
#+results='asis'
nada <- summaries %>%
	filter(modelType=='group',
	       sample=='Nat') %>%
	group_by(sample) %>%
	do({
		asamp <- unique(.$sample)
		atable <- as_data_frame(.) %>% 
			mutate_each(funs(gsub('^0(\\.\\d+)',
					      '\\1', 
					      sprintf('%.2f', .))),
				    CFI, TLI, 
				    RMSEA_Estimate, 
				    RMSEA_90CI_LB, 
				    RMSEA_90CI_UB, 
				    SRMR) %>%
			mutate(RMSEA=paste0(RMSEA_Estimate,
					    ' [',
					    RMSEA_90CI_LB,
					    ',',
					    RMSEA_90CI_UB,
					    ']')) %>%
			select(variable,
			       RMSEA,
			       SRMR,
			       CFI, TLI) %>%
			rename(`RMSEA [90% CI]`=RMSEA) %>%
			kable(digits=2, caption=paste0('Sample: ', asamp),
			      align=c('l', rep('r', 4)))
		print(atable)
		cat('\n\n\n')
		data_frame(table=list(atable), sample=asamp)
	})

#'
#' ## Relative fit
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

#'
#'
#'	
#+fig.width=9, fig.height=6
paramsummaries %>% filter(modelType=='longitudinal',
			  paramgroup %in% c('A WITH B', 'A WITH C', 'A WITH D'),
			  sample=='Nat') %>%
	select(variable, Group, est, se, paramgroup) %>%
	mutate(Group=factor(str_replace(Group, 'D([2345])', '\\10\'s')),
	       LL=est-1.96*se,
	       UL=est+1.96*se,
	       lag=as.numeric(as.factor(paramgroup))) %>%
	ggplot(aes(Group, est))+
	geom_point(size=1)+
	geom_errorbar(aes(ymin=LL, ymax=UL), width=0)+
	facet_grid(lag~variable)+
	coord_cartesian(y=c(0, 1))+
	theme(axis.text.x=element_text(angle=360-45, hjust=0))+
	labs(title="Age and Value Stability, National Sample",
	     x="Age Group",
	     y="Correlation with 95% CI")
	
