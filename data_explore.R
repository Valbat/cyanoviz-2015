library(dplyr)

dat2015 <- read.csv("data_clean_2015.csv", stringsAsFactors = FALSE)

tbl_df(dat2015)
count_2015 <- dat2015 %>% 
  group_by(waterbody_id) %>% 
  count()
count_2015

#Try summarize()
ggplot(data=dat2015)+geom_point(mapping=aes(x=sample_date,y=chla_ugl))
ggplot(data=dat2015)+geom_point(mapping=aes(x=sample_date,y=phyco_ugl))


ggplot(data=dat2015,mapping=aes(x=sample_date,y=Chla_ugl))+geomp_point(mapping=aes(color=waterbody_id))+geom_smooth()
