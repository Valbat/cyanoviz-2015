library(tidyverse)
library(dplyr)
library(ggplot2)

dat2015 <- read_csv("may data_clean_2015.csv",guess_max = 2000) %>%
  filter(complete.cases(data.frame(chla_ugl,phyco_ugl))) %>%
  filter(chla_ugl > 0) %>% 
  filter(phyco_ugl > 0.1) %>%
  arrange(desc(chla_ugl), desc(phyco_ugl)) %>%
  filter(dilution == "1:1") %>%
  filter(analysis_rep == "Primary") %>%
  filter(fluorometer_type == "Beagle") %>%
  filter(sample_temp_c > 20 & sample_temp_c < 24)%>%
  filter(sample_method=="Intergrated Sampler")
         
##############################################################
#let's look at warwick pond by station type'

warwickpond <- filter(dat2015,waterbody_id=="RI0007024L-02") %>%
  select(waterbody_name,chla_ugl,phyco_ugl,sample_date,station_type)

phyco_warwickpond <- ggplot(data=warwickpond,
   aes(x=sample_date,y=phyco_ugl,color=station_type))+
  geom_point()

phyco_warwickpond

chla_warwickpond<-ggplot(data=warwickpond,
    aes(x=sample_date,y=chla_ugl,color=station_type))+
    geom_point()

chla_warwickpond

#################################################################
#let's look at lower bolton lake by station type
lwrbolton_lake<- filter(dat2015,waterbody_name=="Lower Bolton Lake") %>%
   select(waterbody_name,chla_ugl,phyco_ugl,sample_date,station_type)

table(lwrbolton_lake$station_type)


phyco_lwrbolton<- ggplot(data=lwrbolton_lake,
      aes(x=sample_date,y=phyco_ugl,color=station_type))+
      geom_point()

phyco_lwrbolton

chla_lwrbolton<-ggplot(data=lwrbolton_lake,
     aes(x=sample_date,y=chla_ugl,color=station_type))+
  geom_point()

chla_lwrbolton
#########################################################################
#lets take a broder look at station type and see if it make a difference  
