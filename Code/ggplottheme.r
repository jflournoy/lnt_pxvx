library(cowplot)
jftheme <- theme_cowplot()+
	theme(axis.line=element_line(size=0),
	      strip.background=element_rect(fill='white'))
theme_set(jftheme)
