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
  filter(sample_method==("Intergrated Sampler)

#####################################################################
#convert to wide format (used open refine and found no duplicate waterbody names)
library(tidyr)
waterbody_loca <- dat2015 %>%
  spread(waterbody_id,waterbody_name,fill=0)

stem(dat2015$waterbody_id)
str(dat2015$waterbody_id)

#write.csv if I want to save new data set and look at it in open refine and SQL
#######################################################################
# How many samples taken at the same water temp?
dat2015 %>% 
  group_by(water_temp_c,phyco_ugl,chla_ugl) %>% 
  tally

water_temp <- dat2015 %>% 
  group_by(water_temp_c,phyco_ugl,chla_ugl)

#3. What was the water temp for the highest hit of phyco and chla?

highest_watertemp_phyco<- dat2015 %>%
  group_by(waterbody_name,phyco_ugl) %>%
    summarise(max_water_temp=max(water_temp_c)) 

#######################################################################
#look and see if water temp is correlated with high phyco & chla

phyco_watertemp <- ggplot(data=dat2015,
       aes(x=water_temp_c,y=phyco_ugl, color=state))+
        geom_point()
phyco_watertemp
# seeing NA's dat2015$phyco_ugl(na.rm=false)


chla_watertemp <- ggplot(data=dat2015,
       aes(x=water_temp_c,y=chla_ugl, color=state))+
  geom_point()

watertemp_timeline <- ggplot(dat2015,
      aes(x=sample_date,y=water_temp_c,color=phyco_ugl&chla_ugl))+
      geom_point()

watertemp_timeline <- ggplot(dat2015,
      aes(x=sample_date,y=water_temp_c,))+
      geom_point(alpha=.4,aes(color=phyco_ugl))

watertemp_timeline

#warning message removed 94 rows containing missing values (NA's)
###########################################################

#look at geom_bar and plotting phyco and chla over time by water temp.

ggplot(data=dat2015)+
  geom_bar(
  mapping = aes(x=sample_date,fill=phyco_ugl,color=state),
  position="dodge"  )

ggplot(data=dat2015)+
  stat_summary(
  mapping=aes(x=water_temp_c,y=phyco_ugl),
  fun.ymin=min,
  fun.ymax = max,
  fun.y = median)
  
ggplot(data=dat2015)+
  geom_bar(mapping=aes(x=water_temp_c,color=state))

ggplot(data=dat2015)+
  geom_bar(mapping=aes(x=water_temp_c,color=state))

#######################################################################
# what are the min, max and med of phyco and chla by water temp (how important is depth?)

average_by_watertemp <- dat2015 %>%
  group_by(water_temp_c,sample_date,phyco_ugl,state) %>%
  summarise(mean_phyco=mean(phyco_ugl, na.rm = TRUE),
            max_phyco=max(phyco_ugl, na.rm = TRUE),
            min_phyco=min(phyco_ugl, na.rm = TRUE))

phyco_by_watertemp <- ggplot(data=average_by_watertemp,
       aes(x=water_temp_c,y=phyco_ugl,color=state))+
  geom_point()

phyco_by_time<- ggplot(data=average_by_watertemp,
       aes(x=sample_date,y=water_temp_c,color=phyco_ugl))+
  geom_point()

#depends on what we want to show as to which one.
#############################################################
#let's try some t testing
#does water temp affect phyco or chla

t.test(dat2015$water_temp_c~dat2015$phyco_ugl)

wide_dat2015 <- spread(dat2015,water_temp_c,phyco_ugl)
names(wide_dat2015)[20:26] <- c("water_temp","phyco")
t.test(wide_dat2015$phyco,wide_dat2015$water_temp)

#how do you deal with duplicate identifiers error
#Error: Duplicate identifiers for rows (114, 115), (121, 122)

################################################################
#factors playing around with factors
str(dat2015)
levels(dat2015$water_temp_c)

levels(dat2015$water_temp_c)
dat2015$water_temp_c
plot(dat2015$water_temp_c)
levels(dat2015$water_temp_c)
dat2015$sample_depth_m
str(dat2015$sample_depth_m)
table(dat2015$sample_depth_m)
###############################################################
#Look at depth to see if any relationships exist
#row 359 has a depth of 22 and it's a deep water sample 
#range is 1-5
dat2015 %>%
  slice(358)

####################################################################
