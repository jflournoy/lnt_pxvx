library(future)
library(future.batchtools)

plan(batchtools_slurm,
     template = '/home/flournoy/otherhome/code/lnt_pxvx/batchtools/batchtools.slurm.tmpl',
     resources = list(ncpus = 1, walltime = 60*24-1, memory = '100M',
                      partitions = 'short,fat,long,longfat,preempt'))

rmarkdown::render("national_sample_values_measurement.Rmd")