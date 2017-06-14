#'---
#'title: "Life and Time - PxVx - LGCs"
#'output:
#'  html_document:
#'    toc: true
#'---

#+ echo=F
library(MplusAutomation)
library(knitr)
library(tidyverse)
library(semPlot)
library(printr)
library(stringr)
library(ggplot2)
library(broom)
opts_chunk$set(echo=F, message=F, warning=F)

#'
#' # Intro
#'

# Set working directory to that which contains Code, Data, etc
# setwd('E:/Projects/lnt_pxvx/')
# setwd('/Users/johnflournoy/Documents/lnt_pxvx/')

loadUniFN<-'../Rez/uniLGCMods.RData'

# # Create Models
# createModels('Code/PxVx_UniLGCTemplate.inp')
# 
# setwd('/Users/johnflournoy/Documents/lnt_pxvx/Rez/univariate-lgc')
# runModels(recursive = T)

# # Read models
# setwd('../Rez/univariate-lgc')
# saveUniFN<-'../uniLGCMods.RData'
# uniModelOut<-readModels(recursive = T,
# 			filefilter='lgc.*')
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
          c('sample', 'variable'),
          'PxVx Univariate - (\\w+) ([\\w_]+)')

paramsummaries <- uniModelOut_df %>% rowwise %>%
	  do({
	    someParams <- .[[1]]$parameters$unstandardized
	    someParams.df <- as_data_frame(someParams) %>%
		    mutate(est_se=as.numeric(ifelse(est_se == '*********', NA, est_se)))
	    someParams.df$Title <- as.character(.[[1]]$summaries$Title)
	    someParams.df
	  }) %>%
	  extract(Title, 
		  c('sample', 'variable'),
		  'PxVx Univariate - (\\w+) ([\\w_]+)') %>%
	  mutate(paramstatement=paste(paramHeader, param, sep='.'),
		 paramgroup=str_replace(paramstatement, 
					'^([ABCDS]|Means|Intercepts|Variances|Residual\\.Variances).*?\\.(ON|WITH)*\\.*([ABCDIS]).*',
					'\\1 \\2 \\3'))
	

#'
#' # Parameter estimates
#'

#+results='asis'
paramsummaries %>%
  filter(paramHeader == 'Means', sample == 'Nat') %>%
  select(param, est, se, pval) %>%
  gather(stat, value, -param) %>%
  extract(param, c('param', 'measure'), regex = '([IS])_(\\w+)') %>%
  filter(!grepl('(BFA|BFI)', measure)) %>%
  spread(stat, value) %>%
  select(measure, param, est, se, pval) %>%
  arrange(measure, param) %>%
  knitr::kable()
  


