#'---
#'title: "Life and Time - PxVx - Wave as Time Variable"
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

library(cowplot)
jftheme <- theme_cowplot()+
  theme(axis.line=element_line(size=0),
        strip.background=element_rect(fill='white'))
theme_set(jftheme)

#'
#' # Intro
#'
#' **Template for model comparisons:**
#'
#'
#' Using [Latent Curve Model with Structured Residuals](http://psycnet.apa.org/fulltext/2013-44751-001.pdf) -- see [example code](http://curran.web.unc.edu/lcm-sr-data-code/).
#'

# Set working directory to that which contains Code, Data, etc
# setwd('E:/Projects/lnt_pxvx/')
# setwd('/Users/johnflournoy/Documents/lnt_pxvx/')
# setwd("~/Documents/lnt_pxvx")

loadUniFN <- '~/code_new/lnt_pxvx/Rez/uniLCMSRMods_wavetime.RData'

# # Create Models
# createModels('Code/PxVx_UniLCMSR_nat_wave_time.inp')
# 
# setwd('E:/Projects/lnt_pxvx/Rez/univariate')
# setwd("~/Documents/lnt_pxvx/Rez/univariate-lcmsr")
# runModels(target = '~/code_new/lnt_pxvx/Rez/univariate-lcmsr-wavetime/',
#           recursive = T,
#           Mplus_command = '/opt/mplus/8/mplus')

# # Read models
# setwd("~/Documents/lnt_pxvx/Rez/univariate-lcmsr")
# saveUniFN<-'Rez/uniLCMSRMods_wavetime.RData'
# uniModelOut<-readModels(
#   target = '~/code_new/lnt_pxvx/Rez/univariate-lcmsr-wavetime/',
#   recursive = T,
#   filefilter='LCM-SR.*')
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
          c('variable'),
          'PxVx Univariate LCM-SR - wavetime - ([\\w_]+)')

paramsummaries <- uniModelOut_df %>% rowwise %>%
  do({
    someParams <- .[[1]]$parameters$unstandardized
    someParams.df <- as_data_frame(someParams) %>%
      mutate(est_se=as.numeric(ifelse(est_se == '*********', NA, est_se)))
    someParams.df$Title <- as.character(.[[1]]$summaries$Title)
    someParams.df
  })%>%
  extract(Title, 
          c('variable'),
          'PxVx Univariate LCM-SR - wavetime - ([\\w_]+)') %>%
  mutate(paramstatement=paste(paramHeader, param, sep='.'),
         paramgroup=str_replace(paramstatement, 
                                '^([ABCDS]|Means|Intercepts|Variances|Residual\\.Variances).*?\\.(ON|WITH)*\\.*([ABCDIS]).*',
                                '\\1 \\2 \\3'))

#' 
#' # Parameter estimates
#' 
#+ results='asis'

paramNames <- c(`P4.ON.P3` = 'AR',
                `Means  I` = 'i_mu',
                `Means  S` = 's_mu',
                `Variances  I` = 'i_var',
                `Variances  S` = 's_var')

variableTypes <- data_frame(variable=c('aspfin',  'aspfin_d',  'bfa_mt',  'bfa_mt_d',  'hrz_col',  'hrz_col_d',  'hrz_ind',  'hrz_ind_d',  'mvi',  'usi',  'usi_d',  'vrt_col',  'vrt_col_d',  'vrt_ind',  'vrt_ind_d'),
                            vartype='v')

summary_data_for_tables <- paramsummaries %>%
  left_join(variableTypes) %>%
  filter((paramstatement == 'P4.ON.P3') |
           grepl('Means.[IS]_',paramstatement) |
           grepl('Variances.[IS]_',paramstatement)) %>%
  mutate(param = paramNames[paramgroup],
         vartype = factor(vartype, levels = c('v', 'p'))) %>%
  select(variable, param, est, se, pval, vartype) %>%
  arrange(desc(vartype), variable, param)

nada <- summary_data_for_tables %>%
  group_by(vartype, variable) %>%
  do({
    caption <- paste0('Variable: ',
                      .$variable[[1]])
    dataForTable <- select(., -vartype)
    theTable <- kable(dataForTable,
                      #digits=2,
                      caption=caption)
    print(theTable)
    data_frame(Table=list(theTable))
  })
