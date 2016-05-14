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

loadUniFN<-'../Rez/stability.RData'

# Create Models
createModels('Code/value_stability_invariance_template.inp')

# setwd('E:/Projects/lnt_pxvx/Rez/univariate')
# runModels(recursive = T)

# # Read models
# setwd('../Rez/univariate')
# saveUniFN<-'../uniMods.RData'
# uniModelOut<-readModels(recursive = T,
# 			filefilter='ALT.*')
# uniModelOut_df <- data_frame(model=uniModelOut)
# save(uniModelOut_df,file=saveUniFN)
# setwd('../../Code')

load(loadUniFN)
