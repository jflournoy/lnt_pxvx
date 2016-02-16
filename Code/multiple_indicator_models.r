setwd('C:/Users/jackflibb/Documents/UO/Life and Times/Work and Concientiousness/multilevel')
source(file='load_data.r', echo=T)

bfiOnly<-theScales[grep('bfi',theScales)]

combinedPower$female<-(as.numeric(factor(combinedPower$gender,levels=c('Male','Female')))-1)
combinedPower$collegeSample<-(as.numeric(factor(combinedPower$subsample,levels=c('national','college')))-1)

combinedPowerBrief<-combinedPower[,c('subjid','timepoint','SourceID','waveindex','collegeSample',bfiOnly,'incrJobResp','jobIncrStateW1','age_samp_c','female')]
combinedPowerBrief<-as.data.frame(sapply(combinedPowerBrief,as.character),stringsAsFactors=F)
combinedPowerBrief[duplicated(combinedPowerBrief[,c('subjid','timepoint','waveindex')]),c('subjid','timepoint','waveindex','SourceID')]
combinedPowerBrief[combinedPowerBrief$subjid %in% c('1064','2233','2688','3815'),c('subjid','timepoint','waveindex','SourceID')]
combinedPowerBrief[duplicated(combinedPowerBrief[,c('subjid','timepoint','waveindex')]),'waveindex']<-c('o2','o3','o3','o2')
combinedPowerBrief<-combinedPowerBrief[!(combinedPowerBrief$subjid %in% 3815 & combinedPowerBrief$waveindex %in% 'o2'),]

combinedPowerBrief$waveindex<-gsub('[mnop]([1-9]).*','\\1',as.character(combinedPowerBrief$waveindex))
combinedPowerBrief$waveindex[combinedPowerBrief$SourceID %in% 0] <- '0'
head(combinedPowerBrief$waveindex)



library(reshape2)
combinedPowerBrief_l<-melt.data.frame(
	combinedPowerBrief,
	id.vars=c('subjid','timepoint','waveindex','collegeSample','female'))
head(combinedPowerBrief_l)
combinedPowerBrief_w<-dcast(combinedPowerBrief_l,subjid+collegeSample+female~timepoint+waveindex+...)
head(combinedPowerBrief_w)

library(plyr)
names(combinedPowerBrief_w)<-llply(
	names(combinedPowerBrief_w),
	function(aName){
		if(aName %in% c('subjid','collegeSample','female')) return(aName)
		num1<-sub('^([1-4])_.*','\\1',aName)
		num2<-sub('^[1-4]_([0-9])_.*','\\1',aName)
		therest<-sub('^[1-4]_[0-9]_(.*)','\\1',aName)
		num1Letter<-c('A','B','C','D')[as.numeric(num1)]
		newName<-paste(therest,num1Letter,num2,sep='')
	})
names(combinedPowerBrief_w)

jobIncEvents<-grep('(incrJobResp[A-D]0|jobIncrStateW1[A-D]0)',names(combinedPowerBrief_w),value=T)
ageCols<-grep('age_samp_c[A-D]0',names(combinedPowerBrief_w),value=T)
bfiOnly_w<-grep('bfi',names(combinedPowerBrief_w),value=T)
combinedPowerBrief_w_subset<-combinedPowerBrief_w[combinedPowerBrief_w$collegeSample %in% 0,c('subjid','collegeSample','female',jobIncEvents,bfiOnly_w,ageCols)]
head(combinedPowerBrief_w_subset)

setwd('C:/Users/jackflibb/Documents/UO/Life and Times/Work and Concientiousness/SEM/multipleIndicator')
write.table(combinedPowerBrief_w_subset,file='mmData.csv',col.names = F,sep = ',',na = '-99999',quote = F,row.names=F)
abvNames<-abbreviate(names(combinedPowerBrief_w_subset))
abvNames<-sub('bf_','bfi_',abbreviate(names(combinedPowerBrief_w_subset)))
abvNamesMat<-cbind(abvNames,names(combinedPowerBrief_w_subset))
write(paste(abvNamesMat,collapse='\n'),'mmVarList.txt')

library(dplyr)
library(knitr)
mean.rmna<-function(x) mean(as.numeric(x),na.rm=TRUE)
select(combinedPowerBrief_w_subset,starts_with('bfi_c')) %>% 
	summarise_each(funs(mean.rmna)) %>% 
	t() %>% kable(col.names='Mean')

#|        |      Mean|
#|:-------|---------:|
#|bfi_cA0 |  68.55982|
#|bfi_cA1 |  82.37028|
#|bfi_cA2 |  82.09951|
#|bfi_cA3 |  85.15625|
#|bfi_cA4 |  90.62500|
#|bfi_cA5 |  91.34615|
#|bfi_cA6 | 100.00000|
#|bfi_cB0 |  69.00579|
#|bfi_cB1 |  82.85040|
#|bfi_cB2 |  82.81991|
#|bfi_cB3 |  83.78713|
#|bfi_cB4 |  79.72973|
#|bfi_cB5 |  86.71875|
#|bfi_cB6 |  68.75000|
#|bfi_cC0 |  69.02129|
#|bfi_cC1 |  81.80147|
#|bfi_cC2 |  83.93750|
#|bfi_cC3 |  83.09659|
#|bfi_cC4 |  81.25000|
#|bfi_cC5 |  95.00000|
#|bfi_cC6 | 100.00000|
#|bfi_cD0 |  69.52544|
#|bfi_cD1 |  83.08157|
#|bfi_cD2 |  82.83046|
#|bfi_cD3 |  82.67045|
#|bfi_cD4 |  91.87500|
#|bfi_cD5 |  82.14286|
#|bfi_cD6 | 100.00000|

group_by(theDataDF,subsample,timepoint) %>% 
	summarize(
		count=sum(Agree_1,na.rm=T),
		minAge=min(age,na.rm=T),
		maxAge=max(age,na.rm=T),
		mean=mean(age,na.rm=T),
		sd=sd(age,na.rm=T))