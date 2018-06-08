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

source('~/code_new/lnt_pxvx/Code/ggplottheme.r')

mfi <- function(X2,df,N){
  exp(-0.5 * (X2 - df)/N)
}

# Set working directory to that which contains Code, Data, etc
# setwd('E:/Projects/lnt_pxvx/')

loadStbFN<-'~/code_new/lnt_pxvx/Rez/stability.RData'

# # Create Models
# createModels('Code/value_stability_invariance_template.inp')
# createModels('Code/value_stability_invariance_template-Col.inp')
# 
# setwd('E:/Projects/lnt_pxvx/Rez/stability')
# runModels(target = '~/code_new/lnt_pxvx/Rez/stability/',
#           Mplus_command = '/opt/mplus/8/mplus',
#           recursive = T)
# 
# # Read models
# # setwd('../Rez/stability')
# saveStbFN<-'Rez/stability.RData'
# stbModelOut<-readModels(target = '~/code_new/lnt_pxvx/Rez/stability/',
#                         recursive = T,
#                         filefilter='stability.*')
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
          'Stability of values - (\\w+) ([\\w_]+) ([\\w_]+);') %>%
  mutate(MFI = mfi(X2 = ChiSqM_Value, df = ChiSqM_DF, N = 863))

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
	       ChiSqM_Value, ChiSqM_DF, ChiSqM_PValue, MFI,
	       AIC, BIC) %>%
	gather(stat, value, ChiSqM_Value, ChiSqM_DF, MFI, AIC, BIC) 

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
				    MFI, CFI, 
				    RMSEA_Estimate, 
				    RMSEA_90CI_LB, 
				    RMSEA_90CI_UB) %>%
			mutate(RMSEA=paste0(RMSEA_Estimate,
					    ' [',
					    RMSEA_90CI_LB,
					    ',',
					    RMSEA_90CI_UB,
					    ']')) %>%
			select(variable,
			       RMSEA,
			       MFI,
			       CFI) %>%
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
					  MFI,CFI, 
				    RMSEA_Estimate, 
				    RMSEA_90CI_LB, 
				    RMSEA_90CI_UB) %>%
			mutate(RMSEA=paste0(RMSEA_Estimate,
					    ' [',
					    RMSEA_90CI_LB,
					    ',',
					    RMSEA_90CI_UB,
					    ']')) %>%
			select(variable,
			       RMSEA,
			       MFI,
			       CFI) %>%
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
			     ChiSqM_Value, ChiSqM_DF, ChiSqM_PValue, MFI,
			     AIC, BIC) %>%
			mutate(Parameters=ifelse(is.na(Parameters),
						 '',
						 Parameters)) %>%
			kable(caption=paste0('Sample: ', asamp))
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
  ungroup() %>%
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
#+fig.width=9, fig.height=6, dpi=300
vVarNames <- c(
  usi ='Unmitigated\nSelf-Interest',
  usi_d ='Unmitigated Self-Interest Invariant',
  vrt_ind ='Vertical\nIndividualism',
  vrt_ind_d ='Vertical Individualism Invariant',
  bfa_mt ='Materialism',
  bfa_mt_d ='Materialism Invariant',
  aspfin ='Financial\nAspirations',
  aspfin_d ='Financial Aspirations Invariant',
  mvi ='Mature\nValues Index',
  vrt_col ='Vertical\nCollectivism',
  vrt_col_d ='Vertical Collectivism Invariant',
  hrz_col ='Horizontal\nCollectivism',
  hrz_col_d ='Horizontal Collectivism Invariant',
  hrz_ind='Horizontal\nIndividualism',
  hrz_ind_d='Horizontal Individualism Invariant'
)

bordergray <- '#dddddd'
meangray <- '#777777'

paramsummaries %>% filter(modelType=='longitudinal',
			  paramgroup %in% c('A WITH B', 'A WITH C', 'A WITH D'),
			  sample=='Nat',
			  !grepl('_d$',variable)) %>%
  mutate(variable = factor(variable, levels = names(vVarNames), labels = vVarNames)) %>%
	select(variable, Group, est, se, paramgroup) %>% ungroup() %>%
	mutate(Group=factor(str_replace(Group, 'D([2345])', '\\10\'s')),
	       LL=est-1.96*se,
	       UL=est+1.96*se,
	       lag=factor(as.numeric(as.factor(paramgroup)), levels = c(1,2,3), labels = c('1-year lag', '2-year lag', '3-year lag'))) %>%
	ggplot(aes(Group, est))+
  geom_hline(yintercept = .6, color = bordergray, linetype = 3) +
	geom_point(size=1)+
	geom_errorbar(aes(ymin=LL, ymax=UL), width=0)+
	facet_grid(lag~variable)+
	coord_cartesian(y=c(.3, 1))+
  scale_y_continuous(breaks = c(.4,.6,.8,1)) +
	theme(axis.text.x=element_text(angle=360-45, hjust=0),
	      panel.border = element_rect(fill = NA, color = bordergray, size = 1, linetype = 1),
	      strip.background = element_rect(fill=bordergray, color = bordergray, size = 1, linetype = 1),
	      axis.line.x = element_line(color = NA, size = .5, linetype = 1),
	      axis.line.y = element_line(color = NA, size = .5, linetype = 1),
	      panel.spacing = unit(0, units = 'in')) + 
	labs(x="Age Decade Group",
	     y="Correlation, 95% CI")

paramsummaries %>% filter(modelType=='longitudinal',
                          paramgroup %in% c('A WITH B', 'A WITH C', 'A WITH D'),
                          sample=='Nat',
                          grepl('_d$',variable)) %>%
  mutate(variable = factor(variable, levels = names(vVarNames), labels = vVarNames)) %>%
  select(variable, Group, est, se, paramgroup) %>% ungroup() %>%
  mutate(Group=factor(str_replace(Group, 'D([2345])', '\\10\'s')),
         LL=est-1.96*se,
         UL=est+1.96*se,
         lag=factor(as.numeric(as.factor(paramgroup)), levels = c(1,2,3), labels = c('1-year lag', '2-year lag', '3-year lag'))) %>%
  ggplot(aes(Group, est))+
  geom_hline(yintercept = .6, color = bordergray, linetype = 3) +
  geom_point(size=1)+
  geom_errorbar(aes(ymin=LL, ymax=UL), width=0)+
  facet_grid(lag~variable)+
  coord_cartesian(y=c(.3, 1))+
  scale_y_continuous(breaks = c(.4,.6,.8,1)) +
  theme(axis.text.x=element_text(angle=360-45, hjust=0),
        panel.border = element_rect(fill = NA, color = bordergray, size = 1, linetype = 1),
        strip.background = element_rect(fill=bordergray, color = bordergray, size = 1, linetype = 1),
        axis.line.x = element_line(color = NA, size = .5, linetype = 1),
        axis.line.y = element_line(color = NA, size = .5, linetype = 1),
        panel.spacing = unit(0, units = 'in')) + 
  labs(x="Age Decade Group",
       y="Correlation, 95% CI")


paramsummaries %>% filter(modelType=='longitudinal',
                          paramgroup %in% c('A WITH B', 'A WITH C', 'A WITH D'),
                          sample=='Nat',
                          !grepl('_d$',variable)) %>%
  mutate(variable = factor(variable, levels = names(vVarNames), labels = vVarNames)) %>%
  select(variable, Group, est, se, paramgroup) %>% ungroup() %>%
  mutate(Group=factor(str_replace(Group, 'D([2345])', '\\10\'s')),
         LL=est-1.96*se,
         UL=est+1.96*se,
         lag=factor(as.numeric(as.factor(paramgroup)), levels = c(1,2,3), labels = c('1-year lag', '2-year lag', '3-year lag'))) %>%
  ggplot(aes(x = Group, y = est, alpha = lag, group = lag))+
  geom_hline(yintercept = .6, color = bordergray, linetype = 3) +
  geom_point(size=1, position = position_dodge(width = .65))+
  geom_errorbar(aes(ymin=LL, ymax=UL), width=0, position = position_dodge(width = .65))+
  facet_grid(~variable)+
  coord_cartesian(y=c(.3, 1))+
  scale_y_continuous(breaks = c(.4,.6,.8,1)) +
  scale_alpha_discrete(range = c(1, .3)) +
  theme(axis.text.x=element_text(angle=360-45, hjust=0),
        panel.border = element_rect(fill = NA, color = bordergray, size = 1, linetype = 1),
        strip.background = element_rect(fill=bordergray, color = bordergray, size = 1, linetype = 1),
        axis.line.x = element_line(color = NA, size = .5, linetype = 1),
        axis.line.y = element_line(color = NA, size = .5, linetype = 1),
        panel.spacing = unit(0, units = 'in')) + 
  labs(x="Age Decade Group",
       y="Correlation, 95% CI")
