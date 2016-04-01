library(dplyr)
library(tidyr)
library(ggplot2)

scores <- expand.grid(Pd=-50:50, Vd=-50:50)

theDat <- as_data_frame(scores) %>%
	mutate(id=1:n(), p0=50, v0=50,
	       p1=p0+Pd,
	       v1=v0+Vd,
	       p2=p1+(v1-v0)*-.11,
	       v2=v1+(p1-p0)*-.07,
	       p3=p2+(v2-v1)*-.11,
	       v3=v2+(p2-p1)*-.07,
	       p4=p3+(v3-v2)*-.11,
	       v4=v3+(p3-p2)*-.07,
	       p5=p4+(v4-v3)*-.11,
	       v5=v4+(p4-p3)*-.07,
	       VdT=v5-v2,
	       PdT=p5-p2)
theDat.l <- theDat %>%
	gather(key, value, -id, -Pd, -Vd, -VdT, -PdT) %>%
	extract(key, c('var', 'wave'), '([pv])([0-5])')

theDat.l %>% 
	filter(Pd %in% -5:5, Vd %in% -5:5) %>%
ggplot(aes(x=wave, y=value, color=Pd))+
	geom_line(aes(group=id), alpha=.1)+
	facet_wrap(~var)

theDat %>%
ggplot(aes(x=Pd, y=Vd))+
geom_segment(aes(xend = Pd+PdT, yend=Vd+VdT, color=Vd*Pd), arrow=arrow(length=unit(0.05, "cm")))+
scale_color_gradient2()+
theme(panel.background=element_rect(fill='white'))
