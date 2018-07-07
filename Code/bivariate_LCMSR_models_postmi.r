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
  hrz_ind_d='Horizontal Individualism Invariant'
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
#           recursive = T,
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



convSum <- summaries %>%
  select(pVar, vVar,
         modelTypeP, modelTypeV, numErrors, stdErrorWarn,
         AIC, BIC, LL, LLCorrectionFactor, Parameters) %>%
  unite(modCombo, modelTypeP, modelTypeV) %>%
  mutate(modComboText=str_replace(modCombo,
                                  'LCM-SR-*(\\w+)*_(Lin|Mean)\\w*_(Lin|Mean)\\w*',
                                  '\\1 P \\2 - V \\3'),
         modComboScore=(numErrors==0)*(1+!stdErrorWarn))

#' First, are there any errors in model convergence or warnings about standard errors?
#'
(areThereErrors <- any(convSum$stdErrorWarn) | any(convSum$numErrors > 0))

#'FALSE means, "Nope!"

#' # Parameter Estimates
#'
#' Note that bold values designate p < .05, and superscript "a" designates p < .005. 
#' The subscript "inv" designates a row that uses the invariant version of the _values_ scale (i.e., the scale scores with some items removed).
#' 

paramsummaries <- 
  biModelOut_df %>% rowwise %>%
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
  #Title example: PxVx Bivariate LCM-SR - Nat Linear BFA_AC with Linear aspfin;
  extract(Title, 
          c('modelTypeP', 'pVar', 'modelTypeV', 'vVar'),
          'PxVx Bivariate LCM-SR - post mi - (\\w+) ([\\w_]+) with (\\w+) ([\\w_]+);') %>%
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
  group_by(pVar, vVar, modelCombo) %>% 
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
         ci.u, ci.l, pval, pVar) 

allParams_w_sampleLong  <- allParams %>% 
  gather(parameter, value, -(ScaleName:N)) %>%
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

#+results='asis'
nada <- allParams_w_sampleLong %>% 
  # filter(vVar == allParams_w_sampleLong$vVar[[1]]) %>%
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
    cat(paste0('\n## ', vVarNames[.$vVar[[1]]], '\n'))
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