#'---
#'title: "Life and Time - PxVx - Base Model Comparisons"
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

loadUniFN<-'../Rez/uniLCMSRMods.RData'

# # Create Models
# createModels('Code/PxVx_UniLCMSRTemplate.inp')
# 
# setwd('E:/Projects/lnt_pxvx/Rez/univariate')
# setwd("~/Documents/lnt_pxvx/Rez/univariate-lcmsr")
# runModels(recursive = T)

# # Read models
# setwd("~/Documents/lnt_pxvx/Rez/univariate-lcmsr")
# saveUniFN<-'../uniLCMSRMods.RData'
# uniModelOut<-readModels(recursive = T,
# 			filefilter='LCM-SR.*')
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
          c('sample', 'variable', 'modelType'),
          'PxVx Univariate LCM-SR - (\\w+) ([\\w_]+) ([\\w_]+)')

paramsummaries <- uniModelOut_df %>% rowwise %>%
	  do({
	    someParams <- .[[1]]$parameters$unstandardized
	    someParams.df <- as_data_frame(someParams) %>%
		    mutate(est_se=as.numeric(ifelse(est_se == '*********', NA, est_se)))
	    someParams.df$Title <- as.character(.[[1]]$summaries$Title)
	    someParams.df
	  })%>%
	  extract(Title, 
		  c('sample', 'variable', 'modelType'),
		  'PxVx Univariate LCM-SR - (\\w+) ([\\w_]+) ([\\w_]+)') %>%
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

variableTypes <- data_frame(variable=c('BFA_AC', 'BFA_AP', 'BFA_CI', 'BFA_CO', 'BFA_EA', 'BFA_EE', 'BFA_MT', 'BFA_NV', 'BFA_NW', 'BFA_OI', 'BFA_OO', 'BFI_A6', 'BFI_C', 'BFI_E', 'bfi_hp8', 'BFI_N', 'BFI_O', 'D_SCALE', 'VRT_COL', 'HRZ_COL', 'HRZ_IND', 'MVI_POMP', 'S_SCALE', 'USI', 'VRT_IND', 'aspfin'),
                            vartype=c(rep('p', 6),
                                      'v',
                                      rep('p', 11),
                                      rep('v', 4),
                                      'p',
                                      rep('v', 3)))

summary_data_for_tables <- paramsummaries %>%
  left_join(variableTypes) %>%
  filter(modelType == 'AR_Lin', sample == 'Nat') %>%
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

#' 
#' # Comparisons
#'
#' The below tables test fit improvements between these three models.
#' Because we use the `tscores` command in MPlus, the MLR estimator is used,
#' which requires an adjustment to the -2\*Log Likelhood values before their
#' difference is computed. For more information on this adjustment, see 
#' the [MPlus website]( https://www.statmodel.com/chidiff.shtml) or the
#' [UCLA website](and http://www.ats.ucla.edu/stat/mplus/faq/s_b_chi2.htm).
#' The *p*-values are calculated for the adjusted -2\*LL difference with
#' *df* equal to the difference in number of parameters estimated.
#'
#' ## All fit info
#'

modelComparisonResults <- summaries %>% group_by(sample, variable) %>%
  do({
	  AICs <- .$AIC
	  names(AICs) <- .$modelType
	  BICs <- .$BIC
	  names(BICs) <- .$modelType
	  #loglikelihood
	  LLs <- .$LL
	  names(LLs) <- .$modelType
	  #correction factors
	  CFs <- .$LLCorrectionFactor
	  names(CFs) <- .$modelType
	  #number of parameters
	  nPs <- .$Parameters
	  names(nPs) <- .$modelType
	  #Degrees of freedom for each comparison
	  model1Names <- c('AR_Int', 'AR_LinM')
	  model2Names <- c('AR_LinM', 'AR_Lin')
	  dfs <- -(nPs[model1Names] - nPs[model2Names])
	  #scaling correction for each comparison
	  cds <- (nPs[model1Names] * CFs[model1Names] - nPs[model2Names] * CFs[model2Names])/
		  (nPs[model1Names] - nPs[model2Names])
	  #Chi-square difference test statistic
	  TRds <- -2*(LLs[model1Names] - LLs[model2Names])/cds
	  PVals <- 1-pchisq(TRds, dfs)
	  data_frame(sample=.$sample[1],
		     variable=.$variable[1],
		     model1=model1Names,
		     model2=model2Names,
		     LLm1=LLs[model1Names],
		     LLm2=LLs[model2Names],
		     CFm1=CFs[model1Names],
		     CFm2=CFs[model2Names],
		     nPm1=nPs[model1Names],
		     nPm2=nPs[model2Names],
		     cd=cds,
		     TRd=TRds,
		     df=dfs,
		     PVal=PVals,
		     AICm1=AICs[model1Names],
		     BICm1=BICs[model1Names],
		     AICm2=AICs[model2Names],
		     BICm2=BICs[model2Names])
  })

sampleNames <- c(Col='College', Nat='National', `Inf`='Informant')

#+ results='asis'
theTables <- modelComparisonResults %>% group_by(sample, variable) %>%
	do({
		caption <- paste0(sampleNames[.$sample[[1]]], ' Sample, ',
				  .$variable[[1]])
		dataForTable <- select(., model1, model2, LLm1, LLm2, 
				       AICm1, AICm2, 
				       BICm1, BICm2, 
				       nPm1, nPm2, 
				       TRd, df, PVal)
		theTable <- kable(dataForTable,
		      		  digits=2,
		      		  caption=caption,
		      		  col.names=c('Model 1', 'Model 2', 'LL M1', 'LL M2', 
						    'AIC M1', 'AIC M2',
						    'BIC M1', 'BIC M2',
						    'Prms M1', 'Prms M2', 
						    '-2*LL $\\Delta$', '*df*', '*p*'))
		print(theTable)
		data_frame(Table=list(theTable))
	})

#'
#' ## Fit $\Delta$ only
#'

#+ results='asis'
theTables <- modelComparisonResults %>% group_by(sample, variable) %>%
	do({
		caption <- paste0(sampleNames[.$sample[[1]]], ' Sample, ',
				  .$variable[[1]])
		dataForTable <- mutate(.,
				       AICdelta=AICm1 - AICm2,
				       BICdelta=BICm1 - AICm2) %>%
				select(model1, model2, 
				       AICdelta, 
				       BICdelta,
				       nPm1, nPm2, 
				       TRd, df, PVal)
		theTable <- kable(dataForTable,
		      		  digits=2,
		      		  caption=caption,
		      		  col.names=c('Model 1', 'Model 2', 
						    'AIC M1 - M2',
						    'BIC M1 - M2',
						    'Prms M1', 'Prms M2', 
						    '-2*LL $\\Delta$', '*df*', '*p*'))
		print(theTable)
		data_frame(Table=list(theTable))
	})

#'
#' ## Best fitting model tally
#'

winningModels <- modelComparisonResults %>% group_by(sample, variable) %>%
	do({
		#determine LL winner
		whichWins <- mutate(.,
				    pRace=PVal<.05,
				    pWinner=ifelse(pRace, model2, model1))
		if(sum(whichWins$pRace) %in% 2){
			LLwinner <- whichWins$pWinner[2]
		} else {
			LLwinner <- whichWins$pWinner[1]
		}
		#determine AIC winner
		AICs <- rbind(as.matrix(select(., model1, AICm1)),
			      as.matrix(select(., model2, AICm2)))
		minAIC <- min(AICs[, 2])
		AICwinner <- unique(AICs[AICs[, 2] == minAIC, 1])
		#determine BIC winner
		BICs <- rbind(as.matrix(select(., model1, BICm1)),
			      as.matrix(select(., model2, BICm2)))
		minBIC <- min(BICs[, 2])
		BICwinner <- unique(BICs[BICs[, 2] == minBIC, 1])
		#Wrap it up
		data_frame(sample=.$sample[[1]],
			   variable=.$variable[[1]],
			   criterion=c('LL', 'AIC', 'BIC'),
			   winningmodel=c(LLwinner, AICwinner, BICwinner))
	})


#+results='asis'
modelTallies <- winningModels %>%
	filter(sample != 'Inf') %>%
	left_join(variableTypes) %>%
	group_by(sample, vartype) %>%
	select(criterion, winningmodel) %>%
	do({
		vartypenames <- c(p='Pers', v='Val')
		sampleNames <- c(Nat='National', Col='College', `Inf`='Informants')
		print(kable(table(select(., criterion, winningmodel)),
	    		    caption=paste0(sampleNames[.$sample[[1]]],
					   ' ',
					   vartypenames[.$vartype[[1]]],
					   ': Tally of winning models, by fit measure'),
			    align='l'))
		tidy(table(select(., criterion, winningmodel)))
	})

winnersByCriterion <- winningModels %>% spread(criterion, winningmodel) 

save(winnersByCriterion, file='../Rez/winningUniLCMSRModels.RData')

kable(winnersByCriterion,
      col.names=c('Sample', 'Variable', 'AIC', 'BIC', '-2*LL'),
      caption='List of which model performs best, by fit measure')


# #'
# #' # Parameter histograms
# #'
# 
# #+fig.width=10, fig.height=2.66, dev='svg', fig.cap='National: path estimates'
# paramsummaries %>% filter(!str_detect(paramgroup, '(Variances|Intercepts|C ON B|D ON C)'),
# 			  sample=='Nat') %>%
# 	ggplot(aes(x=est))+
# 	geom_histogram()+
# 	facet_grid(sample+modelType~paramgroup, scales='free_x')+
# 	geom_vline(xintercept=0, color='red', alpha=.5)+
# 	theme(panel.background=element_rect(fill='white'),
# 	      axis.text.x=element_text(angle=360-45))
# 
# #+fig.width=5.9, fig.height=2.66, dev='svg', fig.cap='National: variance estimates'
# paramsummaries %>% filter(str_detect(paramgroup, '(Variances)'), 
# 			  !str_detect(paramgroup, '( [CD])'),
# 			  sample=='Nat') %>%
# ggplot(aes(x=est))+
# 	geom_histogram()+
# 	facet_grid(sample+modelType~paramgroup, scales='free_x')+
# 	theme(panel.background=element_rect(fill='white'),
# 	      axis.text.x=element_text(angle=360-45))+
# 	coord_cartesian(y=c(0,4))
# 
# #+fig.width=10, fig.height=8, dev='svg', fig.cap='All: path estimates'
# paramsummaries %>% filter(!str_detect(paramgroup, '(Variances|Intercepts|C ON B|D ON C)')) %>%
# ggplot(aes(x=est))+
# 	geom_histogram()+
# 	facet_grid(sample+modelType~paramgroup, scales='free_x')+
# 	geom_vline(xintercept=0, color='red', alpha=.5)+
# 	theme(panel.background=element_rect(fill='white'),
# 	      axis.text.x=element_text(angle=360-45))
# 
# #+fig.width=5.9, fig.height=8, dev='svg', fig.cap='All: variance estimates'
# paramsummaries %>% filter(str_detect(paramgroup, '(Variances)'), 
# 			  !str_detect(paramgroup, '( [CD])')) %>%
# ggplot(aes(x=est))+
# 	geom_histogram()+
# 	facet_grid(sample+modelType~paramgroup, scales='free_x')+
# 	theme(panel.background=element_rect(fill='white'),
# 	      axis.text.x=element_text(angle=360-45))+
# 	coord_cartesian(y=c(0,15))

#'
#' # Load the raw data
#' 

pVarNames <- c(S_SCALE="Social Self-Regulation",
               BFI_C=" Conscientiousness BFI",
               BFA_CI="  Industriousness BFAS", 
               BFA_CO="  Orderliness BFAS", 
               bfi_hp8=" Honesty/Propriety BFI", 
               BFI_A6=" Agreeableness-Six BFI", 
               BFA_AC="  Compassion BFAS", 
               BFA_AP="  Politeness BFAS", 
               BFI_N=" Neuroticism BFI", 
               BFA_NV="  Volatility BFAS", 
               BFA_NW="  Withdrawal BFAS", 
               D_SCALE="Dynamism", 
               BFI_E=" Extraversion BFI", 
               BFA_EA="  Assertiveness BFAS", 
               BFA_EE="  Enthusiasm BFAS", 
               BFI_O=" Openness BFI", 
               BFA_OI="  Intellect BFAS", 
               BFA_OO="  Openness BFAS")

vVarNames <- c(
  'USI'='Unmitigated Self-Interest',
  'VRT_IND'='Vertical Individualism',
  'BFA_MT'='Materialism',
  'aspfin'='Financial Aspirations',
  'MVI_POMP'='Mature Values Index',
  'VRT_COL'='Vertical Collectivism',
  'HRZ_COL'='Horizontal Collectivism',
  'HRZ_IND'='Horizontal Individualism'
)

baseMainDFColNames <- c(
  'subjid',
  'Sample',
  'aGENDER',
  'aage',
  'aethnic1',
  'aethnic2',
  'aedu_mom',
  'aedu_dad',
  'aEDUCATN',
  'bEDUCATN',
  'cEDUCATN',
  'aEMPLOYD',
  'bEMPLOYD',
  'cEMPLOYD',
  'dEMPLOYD',
  'aINCOME',
  'bINCOME',
  'cINCOME',
  'dINCOME',
  'aS_SCALE',
  'bS_SCALE',
  'cS_SCALE',
  'dS_SCALE',
  'aD_SCALE',
  'bD_SCALE',
  'cD_SCALE',
  'dD_SCALE',
  'aS_COMP',
  'bS_COMP',
  'cS_COMP',
  'dS_COMP',
  'aD_COMP',
  'bD_COMP',
  'cD_COMP',
  'dD_COMP',
  'aBFI_A',
  'aBFI_A6',
  'aBFI_C',
  'aBFI_E',
  'aBFI_HP',
  'aBFI_N',
  'aBFI_O',
  'bBFI_A',
  'bBFI_A6',
  'bBFI_C',
  'bBFI_E',
  'bBFI_HP',
  'bBFI_N',
  'bBFI_O',
  'cBFI_A',
  'cBFI_A6',
  'cBFI_C',
  'cBFI_E',
  'cBFI_HP',
  'cBFI_N',
  'cBFI_O',
  'dBFI_A',
  'dBFI_A6',
  'dBFI_C',
  'dBFI_E',
  'dBFI_HP',
  'dBFI_N',
  'dBFI_O',
  'aBFA_AC',
  'aBFA_AP',
  'aBFA_CI',
  'aBFA_CO',
  'aBFA_EA',
  'aBFA_EE',
  'aBFA_N9',
  'aBFA_NV',
  'aBFA_NW',
  'aBFA_OI',
  'aBFA_OO',
  'aBFA_PS',
  'aBFA_MT',
  'bBFA_AC',
  'bBFA_AP',
  'bBFA_CI',
  'bBFA_CO',
  'bBFA_EA',
  'bBFA_EE',
  'bBFA_N9',
  'bBFA_NV',
  'bBFA_NW',
  'bBFA_OI',
  'bBFA_OO',
  'bBFA_MT',
  'bBFA_PS',
  'cBFA_AC',
  'cBFA_AP',
  'cBFA_CI',
  'cBFA_CO',
  'cBFA_EA',
  'cBFA_EE',
  'cBFA_N9',
  'cBFA_NV',
  'cBFA_NW',
  'cBFA_OI',
  'cBFA_OO',
  'cBFA_MT',
  'cBFA_PS',
  'dBFA_AC',
  'dBFA_AP',
  'dBFA_CI',
  'dBFA_CO',
  'dBFA_EA',
  'dBFA_EE',
  'dBFA_N9',
  'dBFA_NV',
  'dBFA_NW',
  'dBFA_OI',
  'dBFA_OO',
  'dBFA_MT',
  'dBFA_PS',
  'aUSI',
  'bUSI',
  'cUSI',
  'dUSI',
  'aHRZ_IND',
  'bHRZ_IND',
  'cHRZ_IND',
  'dHRZ_IND',
  'aVRT_IND',
  'bVRT_IND',
  'cVRT_IND',
  'dVRT_IND',
  'aHRZ_COL',
  'bHRZ_COL',
  'cHRZ_COL',
  'dHRZ_COL',
  'aVRT_COL',
  'bVRT_COL',
  'cVRT_COL',
  'dVRT_COL',
  'aCOLLCTV',
  'bCOLLCTV',
  'cCOLLCTV',
  'dCOLLCTV',
  'aMVS',
  'bMVS',
  'cMVS',
  'dMVS',
  'aST',
  'bST',
  'cST',
  'dST',
  'aSD',
  'bSD',
  'cSD',
  'dSD',
  'aMVS_mc',
  'bMVS_mc',
  'cMVS_mc',
  'dMVS_mc',
  'aST_mc',
  'bST_mc',
  'cST_mc',
  'dST_mc',
  'aSD_mc',
  'bSD_mc',
  'cSD_mc',
  'dSD_mc',
  'bMEANING',
  'cMEANING',
  'dMEANING',
  'bMEAN_L',
  'cMEAN_L',
  'dMEAN_L',
  'bMEANNGc',
  'cMEANNGc',
  'dMEANNGc',
  'bMEANGlc',
  'cMEANGlc',
  'dMEANGlc',
  'bCLASSIC',
  'cCLASSIC',
  'dCLASSIC',
  'bCLASSCc',
  'cCLASSCc',
  'dCLASSCc',
  'bAGR_VAL',
  'cAGR_VAL',
  'dAGR_VAL',
  'bOPN_VAL',
  'cOPN_VAL',
  'dOPN_VAL',
  'bAGR_VLc',
  'cAGR_VLc',
  'dAGR_VLc',
  'bOPNVALc',
  'cOPNVALc',
  'dOPNVALc',
  'aMV_pomp',
  'bMV_pomp',
  'cMV_pomp',
  'dMV_pomp',
  'aST_pomp',
  'bST_pomp',
  'cST_pomp',
  'dST_pomp',
  'aSD_pomp',
  'bSD_pomp',
  'cSD_pomp',
  'dSD_pomp',
  'agoal_ec',
  'bgoal_ec',
  'cgoal_ec',
  'dgoal_ec',
  'aAspfinc',
  'bAspfinc',
  'cAspfinc',
  'dAspfinc',
  'abfi_hp8',
  'bbfi_hp8',
  'cbfi_hp8',
  'dbfi_hp8',
  'aP_S_BFI',
  'aP_D_BFI',
  'aECgoalc',
  'bECgoalC',
  'cECgoalC',
  'dECgoalC',
  'aaspfin',
  'baspfin',
  'caspfin',
  'daspfin',
  'aMVI_POMP',
  'bMVI_POMP',
  'cMVI_POMP',
  'dMVI_POMP'
)

baseMainDF <- read.table('../Data/LT_227.txt', sep='\t', header=F,
                         na.strings=-9999,
                         col.names=baseMainDFColNames)

vWaveVarNames <- lapply(c('a', 'b', 'c', 'd'), paste, 
                         c(names(vVarNames)), sep='') %>% 
  unlist

stderr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

#mean age is 36
summary_data_for_tables_w <- summary_data_for_tables %>%
  select(variable, param, est) %>%
  filter(param %in% c('i_mu', 's_mu'), variable %in% names(vVarNames)) %>%
  spread(param, est) %>%
  mutate(i_age_zero = i_mu - 36 * s_mu) %>%
  mutate(variable = factor(variable, levels = names(vVarNames), labels = vVarNames))

vDF <- baseMainDF %>% 
  filter(aage > 20 & aage < 56, Sample == 1) %>%
  select_(.dots = c(vWaveVarNames, 'aage', 'subjid')) %>%
  mutate(bage = aage + 1, cage = aage + 2, dage = aage + 3) %>%
  gather(key, value, -subjid) %>%
  extract(key, c('wave', 'var'), regex = '([abcd])([\\w_]+)')%>%
  spread(var, value) %>%
  gather(variable, value, -subjid, -wave, -age) %>%
  mutate(half_decade = (age - 1) %/% 5 * 5 + 2.5) %>%
  group_by(wave, half_decade, variable) %>%
  summarize(mean = mean(value, na.rm = T), 
            stderr = stderr(value), 
            n = length(na.omit(value)), 
            se_u = mean + stderr, 
            se_l = mean - stderr,
            ci_u = mean + qt(.975, df=n-1) * stderr,
            ci_l = mean - qt(.975, df=n-1) * stderr) %>%
  mutate(variable = factor(variable, levels = names(vVarNames), labels = vVarNames))

bordergray <- '#dddddd'
meangray <- '#777777'

v_plot <- vDF %>%
  filter(wave == 'a') %>%
  left_join(summary_data_for_tables_w) %>%
  ggplot(aes(x = half_decade, y = mean)) +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u),
                width = 0, color = meangray,
                position = position_dodge(width = 2)) + 
  geom_point(position = position_dodge(width = 2), color = meangray) + 
  # geom_line(position = position_dodge(width = 2))+
  geom_abline(aes(intercept = i_age_zero, slope = s_mu)) + 
  facet_wrap(~variable, ncol = 2) + 
  scale_x_continuous(breaks = seq(25,50,5)) +
  scale_y_continuous(breaks = c(25,50,75)) + 
  theme(panel.border = element_rect(fill = NA, color = bordergray, size = 2, linetype = 1),
        strip.background = element_rect(fill=bordergray, color = bordergray, size = 1, linetype = 1),
        axis.line.x = element_line(color = NA, size = .5, linetype = 1),
        axis.line.y = element_line(color = NA, size = .5, linetype = 1),
        panel.spacing = unit(0, units = 'in')) + 
  labs(y="Scale Score (in POMP units)",x="Age")

print(v_plot)

ggsave(plot = v_plot, filename = 'v_plot.png', width = 7.5, height = 9.5, units = 'in', dpi = 300)
