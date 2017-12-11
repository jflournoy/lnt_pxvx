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
pVarNames <- c(S_SCALE="Social Self-Regulation",
               BFI_C=".Conscientiousness BFI",
               BFA_CI="..Industriousness BFAS", 
               BFA_CO="..Orderliness BFAS", 
               bfi_hp8=".Honesty/Propriety BFI", 
               BFI_A6=".Agreeableness-Six BFI", 
               BFA_AC="..Compassion BFAS", 
               BFA_AP="..Politeness BFAS", 
               BFI_N=".Neuroticism BFI", 
               BFA_NV="..Volatility BFAS", 
               BFA_NW="..Withdrawal BFAS", 
               D_SCALE="Dynamism", 
               BFI_E=".Extraversion BFI", 
               BFA_EA="..Assertiveness BFAS", 
               BFA_EE="..Enthusiasm BFAS", 
               BFI_O=".Openness BFI", 
               BFA_OI="..Intellect BFAS", 
               BFA_OO="..Openness BFAS")

vVarNames <- c('aspfin'='Financial Aspirations',
               'BFA_MT'='Materialism',
               'HRZ_COL'='Horizontal Collectivism',
               'HRZ_IND'='Horizontal Individualism',
               'MVI_POMP'='Mature Values Index',
               'USI'='Unmitigated Self-Interest',
               'VRT_COL'='Vertical Collectivism',
               'VRT_IND'='Vertical Individualism')


# # Set working directory to that which contains Code, Data, etc
# setwd('E:/Projects/lnt_pxvx/')
# # Create Models
# setwd("~/code_new/lnt_pxvx")
# createModels('Code/PxVx_BiLCMSRTemplate.inp')

# # Read models
setwd('~/code_new/lnt_pxvx/Rez/bivariate-lcmsr')
saveBiFN<-'../biLCMSR.RDS'
if(!file.exists(saveBiFN)){
  biModelOut<-readModels(recursive = T,
                         filefilter='BivLCM-SR.*')
  biModelOut_df <- data_frame(model=biModelOut)
  saveRDS(biModelOut_df,file=saveBiFN)
} else {
  biModelOut_df <- readRDS(saveBiFN)
}
setwd('../../Code')

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
  #Title example: PxVx Bivariate LCM-SR - Nat Linear BFA_AC with Linear aspfin;
  extract(Title, 
          c('constraintType', 'sample', 'modelTypeP', 'pVar', 'modelTypeV', 'vVar'),
          'PxVx Bivariate (LCM-SR(?:-Uncons)*) - (\\w+) (\\w+) ([\\w_]+) with (\\w+) ([\\w_]+)') %>%
  mutate(modelNum=1:n()) %>% 
  filter(sample == 'Nat')



convSum <- summaries %>% 
	select(constraintType, sample, pVar, vVar,
	       modelTypeP, modelTypeV, numErrors, stdErrorWarn,
	       AIC, BIC, LL, LLCorrectionFactor, Parameters) %>%
	unite(modCombo, constraintType, modelTypeP, modelTypeV) %>%
	mutate(modComboText=str_replace(modCombo, 
				    'LCM-SR-*(\\w+)*_(Lin|Mean)\\w*_(Lin|Mean)\\w*',
				    '\\1 P \\2 - V \\3'),
	       modComboScore=(numErrors==0)*(1+!stdErrorWarn))

#'
#' ## Are unconstrained models better?
#'
#' ### National
#'
#' First, are there any errors in model convergence or warnings about standard errors?
#' 
(areThereErrors <- any(convSum$stdErrorWarn) | any(convSum$numErrors > 0))
#' There are `r ifelse(areThereErrors, '', 'no')` errors!
#' 
#' Now we can examine show the difference between AIC and BIC scores. 
#' 

#Using MLR scaling factor to correct ChiSq test
# L0 = log-likelihood for the null model.
# L1 = log-likelihood for the alternative model.
# 
# c0 = scaling correction factor for the null model.
# c1 = scaling correction factor for the alternative model.
# 
# p0 = number of parameters estimated in the null model.
# p1 = number of parameters estimated in the alternative model.
# From this information the tests statistic TRd can be calculated (note that cd is calculated first then used to calculate TRd), along with the degrees of freedom:
#   
# cd = (p0*c0-p1*c1)/(p0-p1)
# TRd = -2*(L0-L1)/cd
# df = p1-p0

modelComparisons_l <- convSum %>%
  arrange(pVar, vVar, modCombo) %>%
	group_by(sample, pVar, vVar) %>%
  mutate(n2LL = -2*LL,
         paramXcorrection = Parameters*LLCorrectionFactor) %>% #select(modCombo, n2LL, Parameters, LLCorrectionFactor)
  summarize(dAIC = diff(AIC), 
            dBIC = diff(BIC),
            dn2LL = -diff(n2LL), 
            dParam = diff(Parameters),  
            cd = diff(paramXcorrection) / dParam,
            TRd = dn2LL/cd,
            pChisqTRd = round(pchisq(TRd, 
                                  dParam, 
                                  lower.tail = F),
                           4)) %>% 
  ungroup %>%
  mutate(pVar = factor(pVar, levels = names(pVarNames), labels = pVarNames), 
         vVar = factor(vVar, levels = names(vVarNames), labels = vVarNames))

#'
#' The following tables show the AIC, BIC, and $-2\cdot log(\ell)$ differences,
#' as well as the statistical test for the $-2\cdot log(\ell)$ difference. This test
#' has been adjusted as is necessary when using the robust maximum likelihood estimator.
#' For more information, you can see [this UCLA stats page](https://stats.idre.ucla.edu/mplus/faq/how-can-i-compute-a-chi-square-test-for-nested-models-with-the-mlr-or-mlm-estimators/).
#' The column "cd" is the correction factor, and "TRd" is the resulting test statistic
#' which is then compared to the $\chi^2$ distribution (each comparison has `r unique(modelComparisons_l$dParam)` degrees of freedom).
#' 
#' Negative values of dAIC or dBIC, and significant _p_-values for pChisqTRd indicate that the _unconstrained_ model
#' fits the data better. Recall that the rule of thumb for differernces in AIC is between 2-4. Also recall that dBIC
#' penalizes parameters more than dAIC, and given the large number of parameters we would need to interpret if we were
#' to prefer the unconstrained model, this may be the best metric for our purposes (in the sense that we really value
#' parsimony when writing up these large models).
#' 
#+results='asis'	
periodsToEmSpaces <- function(varname, spacer = '\\.', replace = '&emsp;'){
  return(gsub(paste0(spacer, '(?=[', spacer, '[:alpha:]])'), replace, varname, perl = T))
}
nada <- modelComparisons_l %>%
  filter(sample=='Nat') %>% 
  group_by(vVar) %>%
	select(-sample, -dParam, -dn2LL) %>%
	do({
	  cat(paste('\n####', unique(.$vVar)))
	  arrange(., as.numeric(pVar)) %>%
	    select(-vVar) %>%
	    mutate(pVar = periodsToEmSpaces(pVar)) %>%
	    kable %>%
	    print
	  data.frame()
	  })

#'
#' I'm going to output parameters from the constrained models only,
#' but please keep in mind some models, according to AIC and $\chi^2$
#' test of likelihood differences, fit better when the parameters
#' for residuals, autoregressions, and cross-lag regressions are
#' allowed to vary. We can potentially investigate each of these cases, or simply
#' report that this is the case. 
#'
#'
#' # Parameter Estimates
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
          c('constraintType', 'sample', 'modelTypeP', 'pVar', 'modelTypeV', 'vVar'),
          'PxVx Bivariate (LCM-SR(?:-Uncons)*) - (\\w+) (\\w+) ([\\w_]+) with (\\w+) ([\\w_]+)') %>%
  mutate(paramstatement=paste(paramHeader, param, sep='.')) %>%
  filter(sample == 'Nat', 
         constraintType == 'LCM-SR', 
         !grepl('\\.BY\\.', paramstatement))  %>% # filter(grepl('P4',paramstatement)) %>% select(paramstatement)
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
  group_by(pVar, vVar, modelCombo, sample) %>% 
  do({
    varsS <- .$est[.$paramgroup=='Variances  S']
    varsI <- .$est[.$paramgroup=='Variances  I']
    if(!length(varsI) %in% c(0,2)){
      stop(paste0('Too many intercept variances in ', 
                  paste0(unique(.[, c('pVar','vVar','modelCombo','sample')]), 
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
                  paste0(unique(.[, c('pVar','vVar','modelCombo','sample')]), 
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
    withIandSDF
  }) 

allParams <- paramsummaries %>%
  data.table::as.data.table() %>%
  filter(bivPathType=='Across Var',
         paramgroup %in% c('2 ON 1','I WITH I', 'I WITH I STD',
                           'S WITH S', 'S WITH S STD')) %>%
  mutate(sample=ifelse(str_detect(pVar, '^I_'),
                       'Inf',
                       sample),
         pVar=ifelse(str_detect(pVar, '^I_'),
                     pVarInfNames[pVar],
                     pVar),
         ScaleName=factor(pVarNames[pVar], levels=pVarNames),
         colName=ifelse(is.na(bivPathDir),
                        str_replace_all(paramgroup, 
                                        c('^I WITH I STD$'='rPiVi',
                                          '^I WITH I$'='covPiVi',
                                          '^S WITH S STD$'='rPsVs',
                                          '^S WITH S$'='covPsVs')), 
                        str_replace_all(bivPathDir, 
                                        c('Target: Pers'='VtoP',
                                          'Target: Val'='PtoV'))),
         est.stars=ifelse(pval<.05, 
                          ifelse(pval<.005, sprintf('*%.2fª*', est), sprintf('*%.2f*', est)),
                          sprintf('%.2f', est)),
         est.bf=ifelse(pval<.01, 
                       sprintf('\\textbf{%.2f}', est),
                       sprintf('%.2f', est)),
         se.d=sprintf('%.2f', se),
         pval=sprintf('%.3f', pval),
         ci.u=est+1.96*se,
         ci.l=est-1.96*se) %>%
  select(ScaleName, vVar, sample, modelCombo, colName, 
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
         `VtoP est.stars` = gsub('\\*', '**', `VtoP est.stars`),
         `rPiVi est.stars` = gsub('\\*', '**', `rPiVi est.stars`),
         `rPsVs est.stars` = gsub('\\*', '**', `rPsVs est.stars`)) %>%
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
                           (`$\\text{cor}(\\text{I}_{V},\\text{I}_{P})$`=`rPiVi est.stars`)+
                           (`$p_{\\text{II}}$`=`rPiVi pval`)+
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