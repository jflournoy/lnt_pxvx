setwd('E:/Projects/LnT_Values/')
library(MplusAutomation)
library(knitr)
library(dplyr)
library(tidyr)

saveUniFN<-'E:/Projects/LnT_Values/Rez/uniMods.RData'

# # Create Models
# createModels('code/PxVx_UniTemplate.inp')
# 
# setwd('E:/Projects/LnT_Values/Rez/univariate')
# runModels(recursive = T)

# # Read models
# setwd('E:/Projects/LnT_Values/Rez/model_syntax')
# uniModelOut<-readModels(recursive = T,
#                         filefilter='ALT.*')
# uniModelOut_df <- data_frame(model=uniModelOut)
# save(uniModelOut_df,file=saveUniFN)

load(saveUniFN)

str(uniModelOut_df[1,1][[1]][[1]]$summaries, max.level=2)

summaries <- uniModelOut_df %>% rowwise %>%
  do({
    aSummary <- .[[1]]$summaries
    as_data_frame(aSummary)
  }) %>%
  extract(Title, 
          c('sample', 'variable', 'modelType'),
          'PxVx Univariate - (\\w+) ([\\w_]+) ([\\w_]+)')

modelComparisonResults <- summaries %>% group_by(sample, variable) %>%
  do({
    L0.Int <- .$LL[.$]
  })
