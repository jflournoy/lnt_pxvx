#'---
#'title: "Life and Time - PxVx - Results Summary"
#'output:
#'  pdf_document
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
library(data.table)
library(tables)
library(cowplot)
opts_chunk$set(echo=F, message=F, warning=F, dev='pdf')

I2 <- function(x){
	if (length(x)==0){
		''
	} else {
		I(x)
	}
}


jftheme <- theme_cowplot()+
	theme(axis.line=element_line(size=0),
	      strip.background=element_rect(fill='white'))
theme_set(jftheme)

loadBiFN<-'../Rez/biMods.RData'
load(loadBiFN)

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
#' # What models are these results from?
#'
#' ## National
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
#' All models are full linear -> linear
#'

#'
#' ## College
#'

load('../Rez/winningUniModels.RData')

winnersByCriterion <- winnersByCriterion %>% ungroup %>%
	mutate(sample=ifelse(sample=='Inf', 'Nat', sample))

winNames <- c('AIC', 'BIC', 'LL')
names(winNames) <- paste0(winNames,'_P')
winnersByCriterionP <- winnersByCriterion %>% rename_(.dots=winNames)
names(winNames) <- paste0(winNames,'_V')
winnersByCriterionV <- winnersByCriterion %>% rename_(.dots=winNames)


#'
#' Only the full model testing HRZ_IND with D_SCALE didn't converge. We can use
#' univariate fit statistics to determine that we should choose to use the model 
#' with restricted slope variance for HRZ_IND.
#'

#'
#' # Parameter Summaries
#'
#' The tables summarize the results of the models.
#'

paramsummaries <- biModelOut_df %>% rowwise %>%
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
		} else if(length(varsS)==0 | 
			  any(!.$modelCombo == 'Lin_Lin')){
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

pVarInfNames <- c(I_A="BFI_A6",
		  I_H="bfi_hp8",
		  I_C="BFI_C",
		  I_D="D_SCALE",
		  I_E="BFI_E",
		  I_N="BFI_N",
		  I_O="BFI_O",
		  I_S="S_SCALE")

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

XLparams_w <- paramsummaries %>% as.data.table %>% 
	filter(bivPathType=='Across Var',
	       paramgroup=='B ON A',
	       ifelse(pVar=='D_SCALE' & vVar=='HRZ_IND' & sample=='Col',
		      modelCombo=='Lin_MeanOnly',
		      modelCombo=='Lin_Lin')) %>%
	mutate(sample=ifelse(str_detect(pVar, '^I_'),
			     'Inf',
			     sample),
	       pVar=ifelse(str_detect(pVar, '^I_'),
			   pVarInfNames[pVar],
			   pVar),
	       ScaleName=factor(pVarNames[pVar], levels=pVarNames)) %>%
	select(ScaleName, vVar, sample, bivPathDir, N, est, se, pval) %>% 
	gather(parameter, value, -(ScaleName:bivPathDir)) %>%
	unite(Sample_EfDir_Param, sample, bivPathDir, parameter, sep=' ') %>%
	spread(Sample_EfDir_Param, value) %>%
	arrange(ScaleName) 


IIparams_w <- paramsummaries %>% as.data.table %>% 
	filter(bivPathType=='Across Var',
	       paramgroup=='I WITH I',
	       ifelse(pVar=='D_SCALE' & vVar=='HRZ_IND' & sample=='Col',
		      modelCombo=='Lin_MeanOnly',
		      modelCombo=='Lin_Lin')) %>%
	mutate(sample=ifelse(str_detect(pVar, '^I_'),
			     'Inf',
			     sample),
	       pVar=ifelse(str_detect(pVar, '^I_'),
			   pVarInfNames[pVar],
			   pVar),
	       ScaleName=factor(pVarNames[pVar], levels=pVarNames)) %>%
	select(ScaleName, vVar, sample, N, est, se, pval) %>% 
	gather(parameter, value, -(ScaleName:sample)) %>%
	unite(Sample_EfDir_Param, sample, parameter, sep=' ') %>%
	spread(Sample_EfDir_Param, value) %>%
	arrange(ScaleName) 

# allParams <- paramsummaries %>% as.data.table %>% 
# 	filter(bivPathType=='Across Var',
# 	       paramgroup %in% c('B ON A','I WITH I', 'I WITH I STD'),
# 	       ifelse(pVar=='D_SCALE' & vVar=='HRZ_IND' & sample=='Col',
# 		      modelCombo=='Lin_MeanOnly',
# 		      modelCombo=='Lin_Lin')) %>%
# 	mutate(sample=ifelse(str_detect(pVar, '^I_'),
# 			     'Inf',
# 			     sample),
# 	       pVar=ifelse(str_detect(pVar, '^I_'),
# 			   pVarInfNames[pVar],
# 			   pVar),
# 	       ScaleName=factor(pVarNames[pVar], levels=pVarNames),
# 	       colName=ifelse(is.na(bivPathDir),
# 			      str_replace_all(paramgroup, 
# 					      c('^I WITH I STD$'='rPiVi',
# 						'^I WITH I$'='covPiVi')), 
# 			      str_replace_all(bivPathDir, 
# 					      c('Target: Pers'='VtoP',
# 						'Target: Val'='PtoV')))) %>%
# 	select(ScaleName, vVar, sample, colName, Estimator, N, est, se, pval) 
# 
# allParams_w <- allParams %>% 
# 	gather(parameter, value, -(ScaleName:colName)) %>%
# 	unite(Sample_EfDir_Param, sample, colName, parameter, sep=' ') %>%
# 	spread(Sample_EfDir_Param, value) %>%
# 	arrange(ScaleName) 
# 

allParams <- paramsummaries %>% as.data.table %>% 
	filter(bivPathType=='Across Var',
	       paramgroup %in% c('B ON A','I WITH I', 'I WITH I STD',
				 'S WITH S', 'S WITH S STD'),
	       ifelse(pVar=='D_SCALE' & vVar=='HRZ_IND' & sample=='Col',
		      modelCombo=='Lin_MeanOnly',
		      modelCombo=='Lin_Lin')) %>%
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
			     sprintf('*%.2f*', est),
			     sprintf('%.2f', est)),
	       est.bf=ifelse(pval<.05, 
			     sprintf('\\textbf{%.2f}', est),
			     sprintf('%.2f', est)),
	       ci.u=est+1.96*se,
	       ci.l=est-1.96*se) %>%
	select(ScaleName, vVar, sample, colName, 
	       Estimator, N, est, est.bf, est.stars,  se, 
	       ci.u, ci.l, pval, pVar) 
allParams_w_sampleLong  <- allParams %>% 
	gather(parameter, value, -(ScaleName:colName)) %>%
	unite(EfDir_Param, colName, parameter, sep=' ') %>%
	spread(EfDir_Param, value) %>%
	arrange(ScaleName) 

latexLevels <- str_replace_all(levels(allParams_w_sampleLong$ScaleName),
			   c(' (BFI|BFAS)'='\\\\textsubscript{\\1}',
			     ` `='\\\\ '))

allParams_w_sampleLongLatex <- allParams_w_sampleLong %>%
	mutate(ScaleNameLatex=factor(str_replace_all(ScaleName,
						 c(' (BFI|BFAS)'='\\\\textsubscript{\\1}',
						 ` `='\\\\ ')),
				   levels=latexLevels))

table_options(justification='r')
nada <- booktabs()
vVarNames <- c('aspfin'='Financial Aspirations',
	       'BFA_MT'='Materialism',
	       'HRZ_COL'='Horizontal Collectivism',
	       'HRZ_IND'='Horizontal Individualism',
	       'MVI_POMP'='the Mature Values Index',
	       'USI'='Unmitigated Self-Interest',
	       'VRT_COL'='Vertical Collectivism',
	       'VRT_IND'='Vertical Individualism')

#+'thing4', results='asis'
nada <- allParams_w_sampleLongLatex %>% 
	group_by(vVar) %>%
	do({
		atable <- tabular(Heading()*(scale=Factor(ScaleNameLatex, texify=F))~
				  Heading()*I2*
				  Heading()*Justify(c)*
				  (sample=factor(sample, 
						 levels=c('Nat', 'Col', 'Inf'),
						 labels=c('National Sample',
							  'Student Sample',
							  'Informant Sample')))*
				  Justify(r)*
				  ((`$P\\rightarrow V$`=`PtoV est.bf`)+
				   (`$V\\rightarrow P$`=`VtoP est.bf`)+
# 				   (`$\\text{Cov}_{P_{i}V_{i}}$`=`covPiVi est.bf`)+
				   (`$\\text{r}_{P_{i}V_{i}}$`=`rPiVi est.bf`)+
				   (`$\\text{r}_{P_{s}V_{s}}$`=`rPsVs est.bf`)), 
				  data=.) # %>% cat #%>% latex()
		cat('\n\\begin{table}')
		cat(paste0('\n\\caption{Auto-Regressive Associations Between \\textbf{',
			  vVarNames[unique(.$vVar)],
			  '} and Personality Scales, Accounting for Age}\n'))
		cat('\\begin{adjustbox}{max width=\\columnwidth, min width=\\columnwidth}\n')
		latex(atable)
		cat('\\end{adjustbox}\n')
		cat('\\end{table}\n')
		data_frame(aHTMLTable=list(atable))
	})

nada <- allParams_w_sampleLong %>% 
	group_by(vVar) %>%
	do({
		atable <- tabular(Heading()*(scale=Factor(ScaleName))~
				  Heading()*I2*
				  Heading()*(sample=factor(sample, 
							   levels=c('Nat', 'Col', 'Inf'),
							   labels=c('National Sample',
								    'Student Sample',
								    'Informant Sample')))*
				  Justify(r)*
				  ((N=`PtoV N`)+
				   (`P -> V`=`PtoV est.stars`)+
				   (`V -> P`=`VtoP est.stars`)+
# 				   (`cov PV`=`covPiVi est.stars`)+
				   (`r PV`=`rPiVi est.stars`)), 
				  data=.) # %>% cat #%>% latex()
		csvFilename <- paste0('../Rez/csv/', unique(.$vVar), '.csv')
		write.csv.tabular(atable, file=csvFilename, leftpad=F)
		data_frame(aHTMLTable=list(atable))
	})


# 
# allParams %>%
# 	filter(vVar=='USI', colName %in% c('VtoP', 'PtoV')) %>%
# 	mutate(colName=factor(colName, levels=c('VtoP', 'PtoV')),
# 	       sample=factor(sample, levels=rev(c('Nat', 'Col', 'Inf')))) %>%
# 	ggplot(aes(x=factor(ScaleName, levels=rev(levels(ScaleName))), 
# 		   y=est, ymin=ci.l, ymax=ci.u)) +
# 	geom_hline(yintercept=0, color='black', alpha=.5)+
# 	geom_errorbar(width=0, position=position_dodge(width=.5), aes(group=sample))+
# 	geom_point(color='black', aes(shape=sample), position=position_dodge(width=.5), size=2)+
# 	facet_wrap(~colName, ncol=2)+
# 	scale_shape_discrete(breaks=c('Nat', 'Col', 'Inf'))+
# 	labs(y='Estimate with 95% CI', x='Personality Variable', title='thing thing')+
# 	coord_flip()

#'
#' # Figures
#'

maxCI <- allParams %>% as_data_frame %>% ungroup %>%
	filter(colName %in% c('VtoP', 'PtoV')) %>%
	do({data_frame(value=apply(cbind(abs(.$est+1.96*.$se), abs(.$est-1.96*.$se)),
			   1,
			   max))})

# ggplot(maxCI, aes(x=value))+geom_histogram(binwidth=.1)+coord_cartesian(x=c(0, 1))

allParamsWithMeanOnly <- paramsummaries %>% as.data.table %>% 
	filter(bivPathType=='Across Var',
	       paramgroup %in% c('B ON A','I WITH I', 'I WITH I STD'),
	       ifelse(pVar=='D_SCALE' & vVar=='HRZ_IND' & sample=='Col',
		      modelCombo=='Lin_MeanOnly' | modelCombo=='MeanOnly_MeanOnly',
		      modelCombo=='Lin_Lin' | modelCombo=='MeanOnly_MeanOnly')) %>%
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
						'^I WITH I$'='covPiVi')), 
			      str_replace_all(bivPathDir, 
					      c('Target: Pers'='VtoP',
						'Target: Val'='PtoV'))),
	       est.stars=ifelse(pval<.05, 
			     sprintf('*%.2f*', est),
			     sprintf('%.2f', est)),
	       est.bf=ifelse(pval<.05, 
			     sprintf('\\textbf{%.2f}', est),
			     sprintf('%.2f', est)),
	       ci.u=est+1.96*se,
	       ci.l=est-1.96*se) %>%
	select(ScaleName, vVar, sample, colName, 
	       Estimator, N, est, est.bf, est.stars,  se, 
	       ci.u, ci.l, pval, modelCombo) 

#+fig.width=7, fig.height=9
theForestPlots <- allParams %>% as_data_frame %>%
	group_by(vVar) %>%
	filter(colName %in% c('VtoP', 'PtoV')) %>%
	mutate(colNameFac=factor(colName, levels=c('VtoP', 'PtoV'), labels=c('V to P', 'P to V')),
	       sampleFac=factor(sample, levels=rev(c('Nat', 'Col', 'Inf')))) %>%
	do({
		aPlot <- ggplot(., aes(x=factor(ScaleName, levels=rev(levels(ScaleName))), 
			   y=est, ymin=ci.l, ymax=ci.u)) +
		   geom_hline(yintercept=0, color='black', alpha=.25, size=.25)+
		   geom_errorbar(width=0, position=position_dodge(width=.5), aes(group=sampleFac))+
		   geom_point(aes(shape=sampleFac), 
			      color='black', position=position_dodge(width=.5), size=2)+
		   facet_wrap(~colNameFac, ncol=2)+
		   scale_shape_discrete(breaks=c('Nat', 'Col', 'Inf'), 
					labels=c('National', 'College', 'Informant'))+
		   labs(y='Estimate with 95% CI', x='Personality Variable', 
			shape='Sample',
			title=vVarNames[.$vVar[[1]]])+
		   coord_flip(y=c(-.5, .5))+
		   theme(axis.text.x=element_text(angle=360-45, hjust=0))
	        print(aPlot)
		cat('\n\n\n')
		data_frame(plot=list(aPlot))
	})



# #+fig.width=7, fig.height=15
# theForestPlotsMoreModels <- allParamsWithMeanOnly %>% as_data_frame %>%
# 	filter(colName %in% c('VtoP', 'PtoV')) %>%
# 	mutate(colNameFac=factor(colName, levels=c('VtoP', 'PtoV'), labels=c('V to P', 'P to V')),
# 	       sampleFac=factor(sample, levels=rev(c('Nat', 'Col', 'Inf')))) %>%
# 	unite(sampleModel, sampleFac, modelCombo, remove=F) %>%
# 	group_by(vVar) %>%
# 	do({
# 		aPlot <- ggplot(., aes(x=factor(ScaleName, levels=rev(levels(ScaleName))), 
# 			   y=est, ymin=ci.l, ymax=ci.u)) +
# 		   geom_hline(yintercept=0, color='black', alpha=.25, size=.25)+
# 		   geom_errorbar(aes(group=sampleModel, color=modelCombo), 
# 				 width=0, position=position_dodge(width=.5))+
# 		   geom_point(aes(shape=sampleFac, color=modelCombo, group=sampleModel), 
# 			      position=position_dodge(width=.5), size=2)+
# 		   facet_wrap(~colNameFac, ncol=2)+
# 		   scale_shape_discrete(breaks=c('Nat', 'Col', 'Inf'), 
# 					labels=c('National', 'College', 'Informant'))+
# 		   scale_color_discrete(breaks=c('Lin_Lin', 'Lin_MeanOnly', 'MeanOnly_MeanOnly'), 
# 					labels=c('Bi-model Slope Var',
# 						 'P-model Slope Var',
# 						 'No Slope Var'))+
# 		   labs(y='Estimate with 95% CI', x='Personality Variable', 
# 			shape='Sample',
# 			title=vVarNames[.$vVar[[1]]],
# 			color='Model Type')+
# 		   coord_flip(y=c(-.5, .5))+
# 		   theme(axis.text.x=element_text(angle=360-45, hjust=0))
# 	        print(aPlot)
# 		cat('\n\n\n')
# 		data_frame(plot=list(aPlot))
# 	})

theHeatMapsI <- allParams %>% as_data_frame %>%
	filter(colName %in% c('rPiVi')) %>%
	mutate(sampleFac=factor(sample, levels=c('Nat', 'Col', 'Inf'),
				labels=c('National', 'College', 'Informant')),
	       VvarName=vVarNames[vVar],
	       ScaleName=factor(ScaleName, levels=rev(levels(ScaleName)))) %>% 
# 	filter(sampleFac=='Nat') %>%
	group_by(sampleFac) %>%
	do({
		aPlot <- ggplot(., aes(x=VvarName, y=ScaleName))+
			geom_raster(aes(fill=est))+
			geom_text(aes(label=round(est, 2)), size=3, alpha=.2)+
			scale_fill_gradient2()+
			theme(axis.text.x=element_text(angle=360-45, hjust=0))+
			labs(x='', y='', fill=expression(italic(r)[italic(i)]),
			     title=paste0('Intercept to Intercept Correlations: ',
					  unique(.$sampleFac), ' Sample'))
		print(aPlot)
		cat('\n\n\n')
		data_frame(plot=list(aPlot))
	})

theHeatMapsS <- allParams %>% as_data_frame %>%
	filter(colName %in% c('rPsVs')) %>%
	mutate(sampleFac=factor(sample, levels=c('Nat', 'Col', 'Inf'),
				labels=c('National', 'College', 'Informant')),
	       VvarName=vVarNames[vVar],
	       ScaleName=factor(ScaleName, levels=rev(levels(ScaleName)))) %>% 
# 	filter(sampleFac=='Nat') %>%
	group_by(sampleFac) %>%
	do({
		aPlot <- ggplot(., aes(x=VvarName, y=ScaleName))+
			geom_raster(aes(fill=est))+
			geom_text(aes(label=round(est, 2)), size=3, alpha=.2)+
			scale_fill_gradient2()+
			theme(axis.text.x=element_text(angle=360-45, hjust=0))+
			labs(x='', y='', fill=expression(italic(r)[italic(i)]),
			     title=paste0('Slope to Slope Correlations: ',
					  unique(.$sampleFac), ' Sample'))
		print(aPlot)
		cat('\n\n\n')
		data_frame(plot=list(aPlot))
	})

