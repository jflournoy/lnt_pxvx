#'---
#'title: "Life and Time - PxVx - Bivariate Models"
#'output:
#'  html_document:
#'    toc: true
#'    toc_float:
#'      toc_collapsed: true
#'    toc_depth: 3
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
pVarNames <- c(bfi_c="Conscientiousness",
               bfi_c_d="Conscientiousness$_{inv}$",
               bfas_ci=".Industriousness",
               bfas_ci_d=".Industriousness$_{inv}$", 
               bfas_co=".Orderliness",
               bfas_co_d=".Orderliness$_{inv}$", 
               bfi_hp8="Honesty/Propriety",
               bfi_hp8_d="Honesty/Propriety$_{inv}$", 
               bfi_a6="Agreeableness-Six",
               bfi_a6_d="Agreeableness-Six$_{inv}$", 
               bfas_ac=".Compassion",
               bfas_ac_d=".Compassion$_{inv}$", 
               bfas_ap=".Politeness",
               bfas_ap_d=".Politeness$_{inv}$", 
               bfi_n="Neuroticism",
               bfi_n_d="Neuroticism$_{inv}$", 
               bfas_nv=".Volatility",
               bfas_nv_d=".Volatility$_{inv}$", 
               bfas_nw=".Withdrawal",
               bfas_nw_d=".Withdrawal$_{inv}$", 
               bfi_e="Extraversion",
               bfi_e_d="Extraversion$_{inv}$", 
               bfas_ea=".Assertiveness",
               bfas_ea_d=".Assertiveness$_{inv}$", 
               bfas_ee=".Enthusiasm",
               bfas_ee_d=".Enthusiasm$_{inv}$", 
               bfi_o="Openness",
               bfi_o_d="Openness$_{inv}$", 
               bfas_oi=".Intellect",
               bfas_oi_d=".Intellect$_{inv}$", 
               bfas_oo=".Openness",
               bfas_oo_d=".Openness$_{inv}$")

vVarNames <- c(
  usi ='Unmitigated Self-Interest',
  usi_d ='Unmitigated Self-Interest Invariant',
  vrt_ind ='Vertical Individualism',
  vrt_ind_d ='Vertical Individualism Invariant',
  bfa_mt ='Materialism',
  bfa_mt_d ='Materialism Invariant',
  aspfin ='Financial Aspirations',
  aspfin_d ='Financial Aspirations Invariant',
  mvi ='Mature Values Index',
  vrt_col ='Vertical Collectivism',
  vrt_col_d ='Vertical Collectivism Invariant',
  hrz_col ='Horizontal Collectivism',
  hrz_col_d ='Horizontal Collectivism Invariant',
  hrz_ind='Horizontal Individualism',
  hrz_ind_d='Horizontal Individualism Invariant',
  aspfinc='Financial Aspirations MC'
)
periodsToEmSpaces <- function(varname, spacer = '\\.', replace = '&emsp;'){
  return(gsub(paste0(spacer, '(?=[', spacer, '[:alpha:]])'), replace, varname, perl = T))
}

# # Set working directory to that which contains Code, Data, etc
# setwd('E:/Projects/lnt_pxvx/')
# # Create Models
# setwd("~/code_new/lnt_pxvx")
# createModels('Code/PxVx_BiLCMSR_nat_mi_template.inp')
# runModels(target = '/home/jflournoy/code_new/lnt_pxvx/Rez/bivariate-lcmsr-post_mi/',
#           recursive = T, replaceOutfile = 'never',
#           Mplus_command = '/opt/mplus/8/mplus')
# createModels('Code/PxVx_BiLCMSR_nat_decadegroup_template.inp')
# runModels(target = '/home/jflournoy/code_new/lnt_pxvx/Rez/bivariate-lcmsr-decadegroup/',
#           recursive = T, replaceOutfile = 'never',
#           Mplus_command = '/opt/mplus/8/mplus')
# createModels('Code/PxVx_BiLCMSR_nat_40splitgroup_template.inp')
# runModels(target = '/home/jflournoy/code_new/lnt_pxvx/Rez/bivariate-lcmsr-40splitgroup/',
#           recursive = T, replaceOutfile = 'never',
#           Mplus_command = '/opt/mplus/8/mplus')
# createModels('Code/PxVx_BiLCMSR_nat_mi_template_test_famc.inp')
# runModels(target = '/home/jflournoy/code_new/lnt_pxvx/Rez/bivariate-lcmsr-post_mi_famc/',
#           recursive = T, replaceOutfile = 'never',
#           Mplus_command = '/opt/mplus/8/mplus')

# # Read models
# setwd('~/code_new/lnt_pxvx/Rez/bivariate-lcmsr')
saveBiFN<-'~/code_new/lnt_pxvx/Rez/biLCMSR_postmi.RDS'
if(!file.exists(saveBiFN)){
  biModelOut<-readModels(target = '~/code_new/lnt_pxvx/Rez/bivariate-lcmsr-post_mi/',
                         recursive = T,
                         filefilter='BivLCM-SR.*')
  biModelOut_df <- data_frame(model=biModelOut)
  saveRDS(biModelOut_df,file=saveBiFN)
} else {
  biModelOut_df <- readRDS(saveBiFN)
}
saveBiFN_decadegrp<-'~/code_new/lnt_pxvx/Rez/biLCMSR_decadegrp.RDS'
if(!file.exists(saveBiFN_decadegrp)){
  biModelOut_decadegrp<-readModels(target = '~/code_new/lnt_pxvx/Rez/bivariate-lcmsr-decadegroup/',
                         recursive = T,
                         filefilter='BivLCM-SR.*')
  biModelOut_decadegrp_df <- data_frame(model=biModelOut_decadegrp)
  saveRDS(biModelOut_decadegrp_df,file=saveBiFN_decadegrp)
} else {
  biModelOut_decadegrp_df <- readRDS(saveBiFN_decadegrp)
}
saveBiFN_40spltgrp<-'~/code_new/lnt_pxvx/Rez/biLCMSR_40spltgrp.RDS'
if(!file.exists(saveBiFN_40spltgrp)){
  biModelOut_40spltgrp<-readModels(target = '~/code_new/lnt_pxvx/Rez/bivariate-lcmsr-40splitgroup//',
                         recursive = T,
                         filefilter='BivLCM-SR.*')
  biModelOut_40spltgrp_df <- data_frame(model=biModelOut_40spltgrp)
  saveRDS(biModelOut_40spltgrp_df,file=saveBiFN_40spltgrp)
} else {
  biModelOut_40spltgrp_df <- readRDS(saveBiFN_40spltgrp)
}

summaries <- biModelOut_df %>% 
  rowwise %>%
  do({
    aSummary <- .[[1]]$summaries
    aSummaryDF <- as_data_frame(aSummary)
    aSummaryDF$numWarnings <- length(.[[1]]$warnings)
    aSummaryDF$stdErrorWarn <- any(grepl('STANDARD ERRORS COULD NOT BE COMPUTED', 
					 .[[1]]$warnings))
    aSummaryDF$numErrors <- length(.[[1]]$errors)
    aSummaryDF
  }) %>%
  #Title example: PxVx Bivariate LCM-SR - post mi - Linear bfas_ac with Linear aspfin;
  extract(Title, 
          c('modelTypeP', 'pVar', 'modelTypeV', 'vVar'),
          'PxVx Bivariate LCM-SR - post mi - (\\w+) ([\\w_]+) with (\\w+) ([\\w_]+);') %>%
  mutate(modelNum=1:n())

summaries_decadegrp <- biModelOut_decadegrp_df %>% 
  rowwise %>%
  do({
    aSummary <- .[[1]]$summaries
    aSummaryDF <- as_data_frame(aSummary)
    aSummaryDF$numWarnings <- length(.[[1]]$warnings)
    aSummaryDF$stdErrorWarn <- any(grepl('STANDARD ERRORS COULD NOT BE COMPUTED', 
                                         .[[1]]$warnings))
    aSummaryDF$numErrors <- length(.[[1]]$errors)
    aSummaryDF
  }) %>%
  #Title example: PxVx Bivariate LCM-SR - decadegroup - Linear bfas_ac with Linear aspfin;
  extract(Title, 
          c('modelTypeP', 'pVar', 'modelTypeV', 'vVar'),
          'PxVx Bivariate LCM-SR - decadegroup - (\\w+) ([\\w_]+) with (\\w+) ([\\w_]+);') %>%
  mutate(modelNum=1:n())

summaries_40spltgrp <- biModelOut_40spltgrp_df %>%
  rowwise %>%
  do({
    aSummary <- .[[1]]$summaries
    aSummaryDF <- as_data_frame(aSummary)
    aSummaryDF$numWarnings <- length(.[[1]]$warnings)
    aSummaryDF$stdErrorWarn <- any(grepl('STANDARD ERRORS COULD NOT BE COMPUTED',
                                         .[[1]]$warnings))
    aSummaryDF$numErrors <- length(.[[1]]$errors)
    aSummaryDF
  }) %>%
  #Title example: PxVx Bivariate LCM-SR - 40splitgroup - Linear bfas_ac with Linear aspfin;
  extract(Title,
          c('modelTypeP', 'pVar', 'modelTypeV', 'vVar'),
          'PxVx Bivariate LCM-SR - 40splitgroup - (\\w+) ([\\w_]+) with (\\w+) ([\\w_]+);') %>%
  mutate(modelNum=1:n())

convSum <- summaries %>%
  select(pVar, vVar,
         modelTypeP, modelTypeV, numErrors, stdErrorWarn,
         AIC, BIC, LL, LLCorrectionFactor, Parameters) %>%
  unite(modCombo, modelTypeP, modelTypeV) %>%
  mutate(modComboText=str_replace(modCombo,
                                  'LCM-SR-*(\\w+)*_(Lin|Mean)\\w*_(Lin|Mean)\\w*',
                                  '\\1 P \\2 - V \\3'),
         modComboScore=(numErrors==0)*(1+!stdErrorWarn))

convSum_decadegrp <- summaries_decadegrp %>%
  select(pVar, vVar,
         modelTypeP, modelTypeV, numErrors, stdErrorWarn,
         AIC, BIC, LL, LLCorrectionFactor, Parameters) %>%
  unite(modCombo, modelTypeP, modelTypeV) %>%
  mutate(modComboText=str_replace(modCombo,
                                  'LCM-SR-*(\\w+)*_(Lin|Mean)\\w*_(Lin|Mean)\\w*',
                                  '\\1 P \\2 - V \\3'),
         modComboScore=(numErrors==0)*(1+!stdErrorWarn))

convSum_40spltgrp <- summaries_40spltgrp %>%
  select(pVar, vVar,
         modelTypeP, modelTypeV, numErrors, stdErrorWarn,
         AIC, BIC, LL, LLCorrectionFactor, Parameters) %>%
  unite(modCombo, modelTypeP, modelTypeV) %>%
  mutate(modComboText=str_replace(modCombo,
                                  'LCM-SR-*(\\w+)*_(Lin|Mean)\\w*_(Lin|Mean)\\w*',
                                  '\\1 P \\2 - V \\3'),
         modComboScore=(numErrors==0)*(1+!stdErrorWarn))

convSum_combn <- dplyr::full_join(
  dplyr::full_join(
    dplyr::rename_at(
      dplyr::select(convSum, pVar, vVar, AIC, LL, LLCorrectionFactor, Parameters),
      dplyr::vars(AIC, LL, LLCorrectionFactor, Parameters),
      dplyr::funs(paste0(., '_const'))),
    dplyr::rename_at(
      dplyr::select(convSum_decadegrp, pVar, vVar, AIC, LL, LLCorrectionFactor, Parameters),
      dplyr::vars(AIC, LL, LLCorrectionFactor, Parameters),
      dplyr::funs(paste0(., '_dec'))),
    by = c('pVar', 'vVar')
  ),
  dplyr::rename_at(
    dplyr::select(convSum_40spltgrp, pVar, vVar, AIC, LL, LLCorrectionFactor, Parameters),
    dplyr::vars(AIC, LL, LLCorrectionFactor, Parameters),
    dplyr::funs(paste0(., '_40s'))),
  by = c('pVar', 'vVar')
)

#Using MLR scaling factor to correct ChiSq test
# L0 = log-likelihood for the more constrained model.
# L1 = log-likelihood for the less constrained model.
# 
# c0 = scaling correction factor for the more constrained model.
# c1 = scaling correction factor for the less constrained model.
# 
# p0 = number of parameters estimated in the more constrained model.
# p1 = number of parameters estimated in the less constrained model.
# From this information the tests statistic TRd can be calculated (note that cd is calculated first then used to calculate TRd), along with the degrees of freedom:
#   
# cd = (p0*c0-p1*c1)/(p0-p1)
# TRd = -2*(L0-L1)/cd
# df = p1-p0

chisq_MLR <- function(L1, L0, c1, c0, p1, p0){
  cd <- (p0*c0-p1*c1)/(p0-p1)
  TRd <- -2*(L0-L1)/cd
  df <- p1-p0
  p <- pchisq(TRd, df, lower.tail = F)
  return(list(TRd = TRd, df = df, p = p))
}

modelComparisons_l <- convSum_combn %>%
  arrange(pVar, vVar) %>%
  mutate(TRd_dec = round(chisq_MLR(LL_dec, LL_const, 
                         LLCorrectionFactor_dec, LLCorrectionFactor_const, 
                         Parameters_dec, Parameters_const)$TRd, 1),
         df_dec = chisq_MLR(LL_dec, LL_const, 
                        LLCorrectionFactor_dec, LLCorrectionFactor_const, 
                        Parameters_dec, Parameters_const)$df,
         p_dec = round(chisq_MLR(LL_dec, LL_const, 
                       LLCorrectionFactor_dec, LLCorrectionFactor_const, 
                       Parameters_dec, Parameters_const)$p, 5),
         dAIC_dec = AIC_dec - AIC_const,
         TRd_40s = round(chisq_MLR(LL_40s, LL_const,
                                   LLCorrectionFactor_40s, LLCorrectionFactor_const,
                                   Parameters_40s, Parameters_const)$TRd, 1),
         df_40s = chisq_MLR(LL_40s, LL_const,
                            LLCorrectionFactor_40s, LLCorrectionFactor_const,
                            Parameters_40s, Parameters_const)$df,
         p_40s = round(chisq_MLR(LL_40s, LL_const,
                                 LLCorrectionFactor_40s, LLCorrectionFactor_const,
                                 Parameters_40s, Parameters_const)$p, 5),
         dAIC_40s = AIC_40s - AIC_const
         ) %>% 
  mutate(pVar = factor(pVar, levels = names(pVarNames), labels = pVarNames), 
         vVar = factor(vVar, levels = names(vVarNames), labels = vVarNames))


#'
#' Sample sizes for group analyses:
#' 
#' ```
#' Decade age groups
#' Group D2                                                    301
#' Group D3                                                    233
#' Group D4                                                    198
#' Group D5                                                    132
#' Total sample size                                           864
#' ```
#' 
#' ```
#' 40-split age groups
#' Group EARLIER                                               534
#' Group LATER                                                 330
#' Total sample size                                           864
#' ```
#'
#' # Compare age-group models
#'
#' The following tables show the AIC, and $-2\cdot log(\ell)$ differences,
#' as well as the statistical test for the $-2\cdot log(\ell)$ difference. This test
#' has been adjusted as is necessary when using the robust maximum likelihood estimator.
#' For more information, you can see [this UCLA stats page](https://stats.idre.ucla.edu/mplus/faq/how-can-i-compute-a-chi-square-test-for-nested-models-with-the-mlr-or-mlm-estimators/).
#' The column "TRd" is the resulting test statistic
#' which is then compared to the $\chi^2$ distribution.
#' 
#' Negative values of dAIC, and significant _p_-values for p_dec or p_40s indicate that the _less constrained_ model (that is, the model with structred residuals allowed to vary between groups)
#' fits the data better. The total number of model comparisons is $16(\text{Personality scales})\times 15(\text{Values scales})\times 2(\text{Age group schemes}) = `r 16*2*15`$, meaning that a conservative, bonferoni _p_-value cut-off would be $.05/`r (16*2*15)` = `r sprintf('%0.5f', .05/(16*2*15))`$ (but of course, these comparisons are not independent).
#' 
#+results='asis'	
nada <- modelComparisons_l %>%
  select(-(LL_const:Parameters_const), 
         -(LL_dec:Parameters_dec),
         -(LL_40s:Parameters_40s)) %>%
  group_by(vVar) %>%
  do({
    cat(paste('\n####', unique(.$vVar)))
    arrange(., as.numeric(pVar)) %>%
      select(-vVar) %>%
      mutate(pVar = periodsToEmSpaces(pVar)) %>%
      kable %>%
      print
    data.frame()
  })

#' First, are there any errors in model convergence or warnings about standard errors?
#'
(areThereErrors <- any(convSum$stdErrorWarn) | any(convSum$numErrors > 0) |
    any(convSum_decadegrp$stdErrorWarn) | any(convSum_decadegrp$numErrors > 0) | 
    any(convSum_40spltgrp$stdErrorWarn) | any(convSum_40spltgrp$numErrors > 0))

#'FALSE means, "Nope!"
#'
#' # Parameter Estimates
#'
#' Note that bold values designate p < .05, and superscript "a" designates p < .005. 
#' The subscript "inv" designates a row that uses the invariant version of the _values_ scale (i.e., the scale scores with some items removed).
#' 

pre_param_df <- biModelOut_df %>% 
  rowwise %>%
  do({
    if(length(.[[1]]$errors)==0){
      someParams <- .[[1]]$parameters$unstandardized
      someParams.df <- as_data_frame(someParams) %>%
        mutate(est_se=as.numeric(ifelse(est_se == '*********', NA, est_se)))
      someParams.df$Title <- as.character(.[[1]]$summaries$Title)
      someParams.df$N <- .[[1]]$summaries$Observations
      someParams.df$Estimator <- .[[1]]$summaries$Estimator
    } else {
      someParams.df <- data_frame(Title=as.character(.[[1]]$summaries$Title))
    }
    someParams.df
  }) %>% 
  dplyr::mutate(Group = 'ALL')

pre_param_decadegrp_df <- biModelOut_decadegrp_df %>% 
  rowwise %>%
  do({
    if(length(.[[1]]$errors)==0){
      someParams <- .[[1]]$parameters$unstandardized
      someParams.df <- as_data_frame(someParams) %>%
        mutate(est_se=as.numeric(ifelse(est_se == '*********', NA, est_se)))
      someParams.df$Title <- as.character(.[[1]]$summaries$Title)
      someParams.df$N <- .[[1]]$summaries$Observations
      someParams.df$Estimator <- .[[1]]$summaries$Estimator
    } else {
      someParams.df <- data_frame(Title=as.character(.[[1]]$summaries$Title))
    }
    someParams.df
  }) 

pre_param_40spltgrp_df <- biModelOut_40spltgrp_df %>% 
  rowwise %>%
  do({
    if(length(.[[1]]$errors)==0){
      someParams <- .[[1]]$parameters$unstandardized
      someParams.df <- as_data_frame(someParams) %>%
        mutate(est_se=as.numeric(ifelse(est_se == '*********', NA, est_se)))
      someParams.df$Title <- as.character(.[[1]]$summaries$Title)
      someParams.df$N <- .[[1]]$summaries$Observations
      someParams.df$Estimator <- .[[1]]$summaries$Estimator
    } else {
      someParams.df <- data_frame(Title=as.character(.[[1]]$summaries$Title))
    }
    someParams.df
  }) 

pre_param_allgrp_df <- dplyr::bind_rows(
  list(pre_param_df, pre_param_decadegrp_df, pre_param_40spltgrp_df))

paramsummaries <- 
  pre_param_allgrp_df %>%
  #Title example: PxVx Bivariate LCM-SR - Nat Linear BFA_AC with Linear aspfin;
  extract(Title, 
          c('modelTypeP', 'pVar', 'modelTypeV', 'vVar'),
          'PxVx Bivariate LCM-SR - (?:[\\w ]+) - (\\w+) ([\\w_]+) with (\\w+) ([\\w_]+);') %>%
  mutate(paramstatement=paste(paramHeader, param, sep='.')) %>%
  filter(!grepl('\\.BY\\.', paramstatement))  %>% # filter(grepl('P4',paramstatement)) %>% select(paramstatement)
  data.table::as.data.table() %>%
  mutate(paramgroup=str_replace(paramstatement, 
                                '^P*V*([ABCDSI1234]|Means|Intercepts|Variances|Residual\\.Variances).*?\\.(ON|WITH)*\\.*P*V*([ABCDIS1234]).*',
                                '\\1 \\2 \\3'),
         withoron=str_detect(paramstatement, '\\.(WITH|ON)\\.'),
         firstVar=str_replace(paramstatement,'[ABCDIS]*_*(P*V*.*?)[1-4]*\\.(WITH|ON)\\.[ABCDISPV]*_*.*','\\1'),
         secondVar=str_replace(paramstatement,'[ABCDISPV]*_*.*\\.(WITH|ON)\\.[ABCDIS]*_*(P*V*.*?)[1-4]*','\\2'),
         bivPathType=ifelse(!is.na(firstVar) & !is.na(secondVar) & withoron,
                            ifelse(firstVar==secondVar,
                                   'Within Var',
                                   'Across Var'),
                            'Other'),
         bivPathDir=ifelse(str_detect(paramstatement, '\\.ON\\.'),
                           ifelse(str_to_upper(firstVar)==str_to_upper(pVar) | grepl('P', firstVar),
                                  'Target: Pers',
                                  'Target: Val'),
                           NA)) %>%
  unite(modelCombo, modelTypeP, modelTypeV) %>%
  group_by(pVar, vVar, modelCombo, Group) %>% 
  do({
    varsS <- .$est[.$paramgroup=='Variances  S']
    varsI <- .$est[.$paramgroup=='Variances  I']
    varsPVwithin <- .$est[.$paramgroup=='Residual.Variances  2']
    if(!length(varsI) %in% c(0,2)){
      stop(paste0('Too many intercept variances in ', 
                  paste0(unique(.[, c('pVar','vVar','modelCombo')]), 
                         collapse=' '),
                  ': ',
                  paste0(varsI, collapse=', ')))
    } else if(length(varsI)==0){
      withIDF <- .
    } else {
      stdIwithIRow <- .[.$paramgroup=='I WITH I', ]
      stdIwithIRow$paramgroup <- 'I WITH I STD'
      covPIVI <- stdIwithIRow$est
      stdIwithIRow$est <- covPIVI/prod(varsI^.5)
      withIDF <- rbind(.,stdIwithIRow)
    }
    if(!length(varsS) %in% c(0,2)){
      stop(paste0('Too many slope variances in ', 
                  paste0(unique(.[, c('pVar','vVar','modelCombo')]), 
                         collapse=' '),
                  ': ',
                  paste0(varsS, collapse=', ')))
    } else if(length(varsS)==0){
      message("NOOOOOPE")
      withIandSDF <- withIDF
    } else {
      stdSwithSRow <- .[.$paramgroup=='S WITH S', ]
      stdSwithSRow$paramgroup <- 'S WITH S STD'
      covPSVS <- stdSwithSRow$est
      stdSwithSRow$est <- covPSVS/prod(varsS^.5)
      withIandSDF <- rbind(withIDF,stdSwithSRow)
    }
    if(!length(varsPVwithin) %in% c(0,2)){
      stop(paste0('Too many within-person variances in ', 
                  paste0(unique(.[, c('pVar','vVar','modelCombo')]), 
                         collapse=' '),
                  ': ',
                  paste0(varsPVwithin, collapse=', ')))
    } else if(length(varsPVwithin)==0){
      message("uh-uh")
      withIandSandWithinDF <- withIandSDF
    } else {
      stdPVwithinRow <- .[.$paramgroup=='2 WITH 2', ]
      stdPVwithinRow$paramgroup <- '2 WITH 2 STD'
      covPVwithin <- stdPVwithinRow$est
      stdPVwithinRow$est <- covPVwithin/prod(varsPVwithin^.5)
      withIandSandWithinDF <- rbind(withIandSDF,stdPVwithinRow)
    }
    withIandSandWithinDF
  }) 

allParams <- paramsummaries %>%
  data.table::as.data.table() %>%
  filter(#bivPathType=='Across Var',
         paramgroup %in% c('2 ON 1','I WITH I', 'I WITH I STD',
                           'S WITH S', 'S WITH S STD', 
                           '2 WITH 2', '2 WITH 2 STD')) %>%
  mutate(pVar=ifelse(str_detect(pVar, '^I_'),
                     pVarInfNames[pVar],
                     pVar),
         pVar=ifelse(grepl('_d$', vVar), paste0(pVar, '_d'), pVar),
         vVar=sub('_d$', '', vVar),
         ScaleName=factor(pVarNames[pVar], levels=pVarNames),
         colName=ifelse(is.na(bivPathDir),
                        str_replace_all(paramgroup, 
                                        c('^I WITH I STD$'='rPiVi',
                                          '^I WITH I$'='covPiVi',
                                          '^S WITH S STD$'='rPsVs',
                                          '^S WITH S$'='covPsVs',
                                          '^2 WITH 2 STD$'='rPVwithin',
                                          '^2 WITH 2$'='covPVwithin')), 
                        ifelse(bivPathType=='Across Var',
                               str_replace_all(bivPathDir, 
                                               c('Target: Pers'='VtoP',
                                                 'Target: Val'='PtoV')),
                               str_replace_all(bivPathDir, 
                                               c('Target: Pers'='PtoP',
                                                 'Target: Val'='VtoV')))),
         est.stars=ifelse(pval<.05, 
                          ifelse(pval<.005, sprintf('*%.2fª*', est), sprintf('*%.2f*', est)),
                          sprintf('%.2f', est)),
         est.bf=ifelse(pval<.05, 
                          ifelse(pval<.005, sprintf('\\textbf{%.2fª}', est), sprintf('\\textbf{%.2f}', est)),
                          sprintf('%.2f', est)),
         se.d=sprintf('%.2f', se),
         pval=sprintf('%.3f', pval),
         ci.u=est+1.96*se,
         ci.l=est-1.96*se) %>%
  select(ScaleName, vVar, modelCombo, colName, 
         Estimator, N, est, est.bf, est.stars,  se, se.d,
         ci.u, ci.l, pval, pVar, Group) 

allParams_w_sampleLong  <- allParams %>% 
  gather(parameter, value, -(ScaleName:N), -Group) %>%
  unite(EfDir_Param, colName, parameter, sep=' ') %>%
  spread(EfDir_Param, value) %>%
  arrange(ScaleName)

library(tables)
I2 <- function(x){
  if (length(x)==0){
    ''
  } else {
    I(x)
  }
}
table_options(justification='l', doCSS=T)

#'
#' ## Single Group Models
#'
#+results='asis'
nada <- allParams_w_sampleLong %>% 
  filter(Group == 'ALL') %>%
  mutate(ScaleName = factor(periodsToEmSpaces(ScaleName),
                            levels = periodsToEmSpaces(pVarNames)),
         `PtoV est.stars` = gsub('\\*', '**', `PtoV est.stars`),
         `VtoV est.stars` = gsub('\\*', '**', `VtoV est.stars`),
         `VtoP est.stars` = gsub('\\*', '**', `VtoP est.stars`),
         `PtoP est.stars` = gsub('\\*', '**', `PtoP est.stars`),
         `rPiVi est.stars` = gsub('\\*', '**', `rPiVi est.stars`),
         `rPsVs est.stars` = gsub('\\*', '**', `rPsVs est.stars`),
         `rPVwithin est.stars` = gsub('\\*', '**', `rPVwithin est.stars`)) %>%
  group_by(vVar) %>%
  do({
    atable <- tabular(Heading()*Justify(l)*(scale=Factor(ScaleName, texify=F))~
                        Heading()*I2*
                        # Heading()*Justify(c)*
                        # (sample=factor(sample, 
                        #                levels=c('Nat', 'Col', 'Inf'),
                        #                labels=c('National Sample',
                        #                         'Student Sample',
                        #                         'Informant Sample')))*
                        Justify(r)*
                        ((`$P\\rightarrow V$`=`PtoV est.stars`)+
                           (`SE`=`PtoV se.d`)+
                           (`$p_{\\text{PV}}$`=`PtoV pval`)+
                           (`$V\\rightarrow P$`=`VtoP est.stars`)+
                           (`SE`=`VtoP se.d`)+
                           (`$p_{\\text{VP}}$`=`VtoP pval`)+
                           (`$V\\rightarrow V$`=`VtoV est.stars`)+
                           (`SE`=`VtoV se.d`)+
                           (`$p_{\\text{VV}}$`=`VtoV pval`)+
                           (`$P\\rightarrow P$`=`PtoP est.stars`)+
                           (`SE`=`PtoP se.d`)+
                           (`$p_{\\text{PP}}$`=`PtoP pval`)+
                           (`$\\text{cor}(\\text{I}_{V},\\text{I}_{P})$`=`rPiVi est.stars`)+
                           (`$p_{\\text{II}}$`=`rPiVi pval`)+
                           (`$\\text{cor}(\\text{V}_{\\text{w/i}},\\text{P}_{\\text{w/i}})$`=`rPVwithin est.stars`)+
                           (`$p_{cor(\\text{PV})}$`=`rPVwithin pval`)+
                           (`$\\text{cor}(\\text{S}_{V},\\text{S}_{P})$`=`rPsVs est.stars`)+
                           (`$p_{\\text{SS}}$`=`rPsVs pval`)), 
                      data=.) # %>% cat #%>% latex()
    cat(paste0('\n### ', vVarNames[.$vVar[[1]]], '\n'))
    html(atable)
    data_frame(aTable=list(atable))
  })

#'
#' ## All Group Models
#'
#+results='asis'
nada <- allParams_w_sampleLong %>% 
  # filter(vVar == allParams_w_sampleLong$vVar[[1]]) %>%
  mutate(ScaleName = factor(periodsToEmSpaces(ScaleName),
                            levels = periodsToEmSpaces(pVarNames)),
         Group = factor(
           Group, 
           levels = c('ALL', 'EARLIER', 'LATER', 'D2', 'D3', 'D4', 'D5'),
           labels = c('Combined', '$\\lt 40$', '$\\geqslant 40$', '20s', '30s', '40s', '50s')),
         `PtoV est.stars` = gsub('\\*', '**', `PtoV est.stars`),
         `VtoV est.stars` = gsub('\\*', '**', `VtoV est.stars`),
         `VtoP est.stars` = gsub('\\*', '**', `VtoP est.stars`),
         `PtoP est.stars` = gsub('\\*', '**', `PtoP est.stars`),
         `rPiVi est.stars` = gsub('\\*', '**', `rPiVi est.stars`),
         `rPsVs est.stars` = gsub('\\*', '**', `rPsVs est.stars`),
         `rPVwithin est.stars` = gsub('\\*', '**', `rPVwithin est.stars`)) %>%
  group_by(vVar) %>%
  do({
    atable <- tabular(Heading()*Justify(l)*(scale=Factor(ScaleName, texify=F))* 
                        Heading()*(Group = Factor(Group, texify=F))~
                        Heading()*I2*
                        # Heading()*Justify(c)*
                        # (sample=factor(sample, 
                        #                levels=c('Nat', 'Col', 'Inf'),
                        #                labels=c('National Sample',
                        #                         'Student Sample',
                        #                         'Informant Sample')))*
                        Justify(r)*
                        ((`$P\\rightarrow V$`=`PtoV est.stars`)+
                           (`SE`=`PtoV se.d`)+
                           (`$p_{\\text{PV}}$`=`PtoV pval`)+
                           (`$V\\rightarrow P$`=`VtoP est.stars`)+
                           (`SE`=`VtoP se.d`)+
                           (`$p_{\\text{VP}}$`=`VtoP pval`)+
                           (`$V\\rightarrow V$`=`VtoV est.stars`)+
                           (`SE`=`VtoV se.d`)+
                           (`$p_{\\text{VV}}$`=`VtoV pval`)+
                           (`$P\\rightarrow P$`=`PtoP est.stars`)+
                           (`SE`=`PtoP se.d`)+
                           (`$p_{\\text{PP}}$`=`PtoP pval`)+
                           (`$\\text{cor}(\\text{I}_{V},\\text{I}_{P})$`=`rPiVi est.stars`)+
                           (`$p_{\\text{II}}$`=`rPiVi pval`)+
                           (`$\\text{cor}(\\text{V}_{\\text{w/i}},\\text{P}_{\\text{w/i}})$`=`rPVwithin est.stars`)+
                           (`$p_{cor(\\text{PV})}$`=`rPVwithin pval`)+
                           (`$\\text{cor}(\\text{S}_{V},\\text{S}_{P})$`=`rPsVs est.stars`)+
                           (`$p_{\\text{SS}}$`=`rPsVs pval`)), 
                      data=.) # %>% cat #%>% latex()
    cat(paste0('\n### ', vVarNames[.$vVar[[1]]], '\n'))
    html(atable)
    data_frame(aTable=list(atable))
  })

# nada <- allParams_w_sampleLong %>% 
#   group_by(vVar) %>%
#   do({
#     atable <- tabular(Heading()*(scale=Factor(ScaleName))~
#                         Heading()*I2*
#                         Heading()*Justify(c)*
#                         (sample=factor(sample, 
#                                        levels=c('Nat', 'Col', 'Inf'),
#                                        labels=c('National Sample',
#                                                 'Student Sample',
#                                                 'Informant Sample')))*
#                         Justify(r)*
#                         ((`P to V`=`PtoV est.stars`)+
#                            (`SE`=`PtoV se.d`)+
#                            (`V to P`=`VtoP est.stars`)+
#                            (`SE`=`VtoP se.d`)), 
#                       data=.) # %>% cat #%>% latex()
#     csvFilename <- paste0('../Rez/csv/LCMSR-', unique(.$vVar), '.csv')
#     write.csv.tabular(atable, file=csvFilename, leftpad=F)
#     data_frame(aTable=list(atable))
#   })