library(tidyverse)
library(dplyr)
library(ggplot2)

dat2015 <- read_csv("may data_clean_2015.csv",guess_max = 2000) %>%
  filter(complete.cases(data.frame(chla_ugl,phyco_ugl)))%>%
  filter(chla_ugl > 0) %>% 
  filter(phyco_ugl > 0.1) %>%
  filter(dilution == "1:1") %>%
  filter(analysis_rep == "Primary") %>%
  filter(fluorometer_type == "Beagle") %>%
  filter(sample_temp_c > 20 & sample_temp_c < 24)
  filter(sample_method==("Intergrated Sampler"))


Cyanobac_WT<- dat2015 %>%
  select(waterbody_name,water_temp_c,state,phyco_ugl,chla_ugl)%>%
  group_by(waterbody_name,water_temp_c,phyco_ugl,chla_ugl)%>%
  summarize(max_watertemp=max(water_temp_c))
  

head(Cyanobac_WT)
############################################################################

maxtemporder<- Cyanobac_WT[order(-Cyanobac_WT$max_watertemp),]

############################################################################
Cyanobac_WT_Wide <- Cyanobac_WT %>%
  spread(max_watertemp,phyco_ugl,chla_ugl)

Cyanobac_wt_max<-Cyanobac_WT%>%
  gather(phyco_ugl,chla_ugl,-mean_watertemp)

Cyanobac_wt_max
###########################################################################

t.test(maxtemporder$max_watertemp~maxtemporder$phyco_ugl)

#############################################################################

?indexing
dat2015$chla_ugl %>%
  subset(variable_a > x) %>%
  transform(variable_c = variable_a/variable_b) %>%
  head(100)
help.start()
##############################################################################

head(maxtemporder)

(maxtemporder[c("phyco_ugl","chla_ugl")])

??transform
##############################################################################

names(maxtemporder)


#####################################################################################
#1. Make a wide frame with waterbody_name,state,max_
#(rows), phyco_ugl,chla_ugl (cols), values = number of 
#max water temps
#hint: n_distinct gives the number
#of unique types

#get number of max water temps for phyco and chla/waterbody
 Cyanobac_WT<- dat2015 %>%
  +   select(waterbody_name,water_temp_c,state,phyco_ugl,chla_ugl)%>%
  +   group_by(waterbody_name,water_temp_c,phyco_ugl,chla_ugl)%>%
  +   summarize(max_watertemp=max(water_temp_c))

maxtemporder<- Cyanobac_WT[order(-Cyanobac_WT$max_watertemp),]

max_water_temp<- maxtemporder %>%
  +   group_by(waterbody_name,state,phyco_ugl,chla_ugl) %>%
  +   summarise(ntemp = n_distinct(max_watertemp))

max_water_temp<- maxtemporder %>%
  group_by(waterbody_name,phyco_ugl,chla_ugl) %>%
  summarise(waterbody_name = n_distinct(waterbody_name))

### holds no value written this way. 





