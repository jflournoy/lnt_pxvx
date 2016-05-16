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
