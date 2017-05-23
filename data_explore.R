
library(dplyr)
library(ggplot2)

#removing chla NA's need to be smarter about this later
(dat2015 <- read.csv("data_clean_2015.csv", stringsAsFactors = FALSE) %>%filter(!is.na(chla_ugl))

summarysumtbl_df(dat2015)
count_2015 <- dat2015 %>% library(dplyr)
  group_by(waterbody_id) %>% 
  count()
count_2015

#Try summarize()




#scatter plots in ggplot
chla_gg <- ggplot(data=dat2015, mapping = aes(x=sample_date,y=chla_ugl)) + 
  geom_point() +
  facet_wrap(~ state)
  geom_smooth(method = "lm")
chla_gg

phyco_gg <-  ggplot(data=dat2015, ,mappping = aes(x=sample_date,y=phyco_ugl)) + 
  geom_point()

ggplot(dat2015,aes(station_longitude,station_latitude, color = state)) +
  geom_point()


ggplot(data=dat2015,mapping=aes(x=sample_date,y=Chla_ug))+geomp_point(mapping=aes(color=waterbody_id))+geom_smooth()

R version 3.2.2 (2015-08-14) -- "Fire Safety"
Copyright (C) 2015 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[Workspace loaded from C:/Users/VMARSHAL/projects/cyanoviz-2015/.RData]

> 
> 
  > dat2015 <- read.csv("data_clean_2015.csv", stringsAsFactors = FALSE)
>
Error: could not find function "%>%"
> View(dat2015)
> library(dplyr)

Attaching package: ‘dplyr’

The following objects are masked from ‘package:stats’:
  
  filter, lag

The following objects are masked from ‘package:base’:
  
  intersect, setdiff, setequal, union

Warning message:
  package ‘dplyr’ was built under R version 3.2.5 
> summary(dat2015)


#na's in station_id, station_descrip, sample-method, sample_depth,dilution, water_temp(1185),Chla 

#max for phyco seems high (687.86)

> mean(dat2015$phyco_ugp)
[1] NA
Warning message:
In mean.default(dat2015$phyco_ugp) :
argument is not numeric or logical: returning NA
plot(density(log1p(nla$NTL)))
> mean(dat2015$phyco_ugl)
[1] NA
> mean(dat2015$chla_ugl)
[1] NA
> hist(dat2015$chla_ugl)
> hist(log1p(dat2015$chla_ugl))

#couldn't fix Density plot error "Error density.default(log1p(dat2015$chla_ugl)) : 
  'x' contains missing values

boxplot(dat2015$chla_ugl)

mean(dat2015$chla_ugl, na.rm=TRUE)
[1] 0.7994273
max(dat2015$chla_ugl, na.rm=TRUE)
[1] 19.26
min(dat2015$chla_ugl, na.rm=TRUE)
mean(dat2015$phyco_ugl, na.rm=TRUE)
max(dat2015$phyco_ugl, na.rm=TRUE)
min(dat2015$phyco_ugl, na.rm=TRUE
    
summary(dat2015)
summarydat2015$datsummarysd(dat2015$chla_ugl,na.rm=TRUE)
IQR(dat2015$chla_ugl,na.rm=TRUE)
#rror in library(zoo) : there is no package called ‘zoo’
install.packages("magrittr")

?indexing
  read.csv(dat2015$chla_ugl) %>%
  subset(variable_a > x) %>%
  transform(variable_c = variable_a/variable_b) %>%
  head(100)
help.start()

mean(dat2015$phyco_ugl, na.rm=TRUE)
min(dat2015$phyco_ugl, na.rm=TRUE)

nonegphyco <- filter(dat2015,phyco_ugl>0.01)
mean(nonegphyco$phyco_ugl,na.rm=T)
min(nonegphyco$phyco_ugl, na.rm=TRUE)
hist(nonegphyco$phyco_ugl)
hist(log1p(dat2015$chla_ugl))
plot(density(log1p(nonegphyco$phyco_ugl)))
boxplot(nonegphyco$phyco_ugl)
boxplot(log1p(nonegphyco$phyco_ugl))
boxplot(log1p(nonegphyco$phyco_ugl)~nonegphyco$state)

plot(log1p(nonegphyco$PTL),log1p(nonegphyco$phyco_ugl)
     
summary(dat2015$sample_temp_c)
boxplot(log1p(dat2015$chla_ugl)~dat2015$chla_ugl$state)
boxplot(dat2015$chla_ugl)
boxplot(log1p(dat2015$chla_ugl))
boxplot(log1p(dat2015$chla_ugl)~dat2015$state)

table(dat2015$sample_temp_c)
table(dat2015$chla_ugl)
install.packages("reshape")
str(dat2015)
str(dat2015$chla_ugl)

?table

table(dat2015$sample_temp_c)
table(nonegphyco$phyco_ugl)

read.csv(nonegphyco$phyco_ugl) %>%
  subset(variable_a > x) %>%
  transform(variable_c = variable_a/variable_b) %>%
  head(100)
min(nonegphyco$phyco_ugl)

> stem(nonegphyco$phyco_ugl)
fivenum(nonegphyco$phyco_ugl)
str(nonegphyco)

install.packages("foreach")
library(foreach)
foreach(i=1:100)%do%rnorm(i)
phycororder <- nonegphyco[order(-phyco_ugl),]
;
> chlaorder <- dat2015[order(-dat2015$chla_ugl),]
> View(chlaorder)
> head(chlaorder)
chlast <- chlaorder[1:50,c(6,7,16,25)]

mergenonegphyco <- filter(dat2015,phyco_ugl>0.01)
phycoorder <- nonegphyco[order(-nonegphyco$phyco_ugl),]
phycost <- phycoorder[1:50,c(6,7,16,26)]
>merge(chlast,phycost,by="state")
library(tidyverse)
library(dplyr)
mrg <- merge(chlast,phycost,by="state")
phycost<-data.frame(t(phycost))
agphyco <- aggregate(nonegphyco$phyco_ugl,list(nonegphyco$state),mean)
agchla <- aggregate(dat2015$chla_ugl,list(dat2015$state),mean)
> mrgag <- merge(agchla,agphyco,by="Group.1")
> mrgag <- merge(agchla,agphyco,by="Group.1")
merge(phycoorder,)



