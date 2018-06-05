#'---
#'title: "Life and Time - PxVx - Post MI models"
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

loadUniFN <- '~/code_new/lnt_pxvx/Rez/uniLCMSRMods_postmi.RData'

# # Create Models
# createModels('Code/PxVx_UniLCMSR_nat_mi_template.inp')
# 
# setwd('E:/Projects/lnt_pxvx/Rez/univariate')
# setwd("~/Documents/lnt_pxvx/Rez/univariate-lcmsr")
# runModels(target = '~/code_new/lnt_pxvx/Rez/univariate-lcmsr-post_mi/', 
#           recursive = T,
#           Mplus_command = '/opt/mplus/8/mplus')

# # Read models
# setwd("~/Documents/lnt_pxvx/Rez/univariate-lcmsr")
# saveUniFN<-'Rez/uniLCMSRMods_postmi.RData'
# uniModelOut<-readModels(
#   target = '~/code_new/lnt_pxvx/Rez/univariate-lcmsr-post_mi/',
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
          'PxVx Univariate LCM-SR - post mi - ([\\w_]+)')

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
          'PxVx Univariate LCM-SR - post mi - ([\\w_]+)') %>%
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

baseMainDFColNames <- c(
  'subjid',
  'aage',
  'aaspfin',
  'aaspfin_d',
  'abfa_mt',
  'abfa_mt_d',
  'abfas_ac',
  'abfas_ap',
  'abfas_ci',
  'abfas_co',
  'abfas_ea',
  'abfas_ee',
  'abfas_nv',
  'abfas_nv9',
  'abfas_nw',
  'abfas_oi',
  'abfas_oo',
  'abfi_a',
  'abfi_a6',
  'abfi_c',
  'abfi_d_scale',
  'abfi_e',
  'abfi_hp8',
  'abfi_n',
  'abfi_o',
  'abfi_s_scale',
  'ahrz_col',
  'ahrz_col_d',
  'ahrz_ind',
  'ahrz_ind_d',
  'amvi',
  'ausi',
  'ausi_d',
  'avrt_col',
  'avrt_col_d',
  'avrt_ind',
  'avrt_ind_d',
  'bage',
  'baspfin',
  'baspfin_d',
  'bbfa_mt',
  'bbfa_mt_d',
  'bbfas_ac',
  'bbfas_ap',
  'bbfas_ci',
  'bbfas_co',
  'bbfas_ea',
  'bbfas_ee',
  'bbfas_nv',
  'bbfas_nv9',
  'bbfas_nw',
  'bbfas_oi',
  'bbfas_oo',
  'bbfi_a',
  'bbfi_a6',
  'bbfi_c',
  'bbfi_d_scale',
  'bbfi_e',
  'bbfi_hp8',
  'bbfi_n',
  'bbfi_o',
  'bbfi_s_scale',
  'bhrz_col',
  'bhrz_col_d',
  'bhrz_ind',
  'bhrz_ind_d',
  'bmvi',
  'busi',
  'busi_d',
  'bvrt_col',
  'bvrt_col_d',
  'bvrt_ind',
  'bvrt_ind_d',
  'cage',
  'caspfin',
  'caspfin_d',
  'cbfa_mt',
  'cbfa_mt_d',
  'cbfas_ac',
  'cbfas_ap',
  'cbfas_ci',
  'cbfas_co',
  'cbfas_ea',
  'cbfas_ee',
  'cbfas_nv',
  'cbfas_nv9',
  'cbfas_nw',
  'cbfas_oi',
  'cbfas_oo',
  'cbfi_a',
  'cbfi_a6',
  'cbfi_c',
  'cbfi_d_scale',
  'cbfi_e',
  'cbfi_hp8',
  'cbfi_n',
  'cbfi_o',
  'cbfi_s_scale',
  'chrz_col',
  'chrz_col_d',
  'chrz_ind',
  'chrz_ind_d',
  'cmvi',
  'cusi',
  'cusi_d',
  'cvrt_col',
  'cvrt_col_d',
  'cvrt_ind',
  'cvrt_ind_d',
  'dage',
  'daspfin',
  'daspfin_d',
  'dbfa_mt',
  'dbfa_mt_d',
  'dbfas_ac',
  'dbfas_ap',
  'dbfas_ci',
  'dbfas_co',
  'dbfas_ea',
  'dbfas_ee',
  'dbfas_nv',
  'dbfas_nv9',
  'dbfas_nw',
  'dbfas_oi',
  'dbfas_oo',
  'dbfi_a',
  'dbfi_a6',
  'dbfi_c',
  'dbfi_d_scale',
  'dbfi_e',
  'dbfi_hp8',
  'dbfi_n',
  'dbfi_o',
  'dbfi_s_scale',
  'dhrz_col',
  'dhrz_col_d',
  'dhrz_ind',
  'dhrz_ind_d',
  'dmvi',
  'dusi',
  'dusi_d',
  'dvrt_col',
  'dvrt_col_d',
  'dvrt_ind',
  'dvrt_ind_d'
)

baseMainDF <- read.table('~/code_new/lnt_pxvx/Data/lnt_nat_recalc.tsv', sep='\t', header=F,
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
  filter(aage > 20 & aage < 56) %>%
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
  filter(wave == 'a', variable != 'Mature Values Index') %>%
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

v_plot_mvi <- vDF %>%
  filter(wave == 'a', variable == 'Mature Values Index') %>%
  left_join(summary_data_for_tables_w) %>%
  ggplot(aes(x = half_decade, y = mean)) +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u),
                width = 0, color = meangray,
                position = position_dodge(width = 2)) + 
  geom_point(position = position_dodge(width = 2), color = meangray) + 
  # geom_line(position = position_dodge(width = 2))+
  geom_abline(aes(intercept = i_age_zero, slope = s_mu)) + 
  scale_x_continuous(breaks = seq(25,50,5)) +
  scale_y_continuous(breaks = c(25,50,75)) +
  theme(panel.border = element_rect(fill = NA, color = bordergray, size = 2, linetype = 1),
        strip.background = element_rect(fill=bordergray, color = bordergray, size = 1, linetype = 1),
        axis.line.x = element_line(color = NA, size = .5, linetype = 1),
        axis.line.y = element_line(color = NA, size = .5, linetype = 1),
        panel.spacing = unit(0, units = 'in')) + 
  coord_cartesian(y = c(20,80)) + 
  labs(y="Scale Score\n(in POMP units)",x="Age")

ggsave(plot = v_plot, filename = 'v_plot_mi.png', width = 7.5, height = 9.5, units = 'in', dpi = 300)
ggsave(plot = v_plot_mvi, filename = 'v_plot_mi_mvi.png', width = 3.5, height = 2.5, units = 'in', dpi = 300)

#' ![](v_plot_mi.png)
#' 
#' **MVI**
#' 
#' ![](v_plot_mi_mvi.png)