library(tidyverse)
library(dplyr)


dat2015 <- read_csv("may data_clean_2015.csv",guess_max = 2000) %>%
  filter(complete.cases(data.frame(chla_ugl,phyco_ugl)))%>%
  filter(chla_ugl > 0) %>% 
  filter(phyco_ugl > 0.1) %>%
  filter(dilution == "1:1") %>%
  filter(analysis_rep == "Primary") %>%
  filter(fluorometer_type == "Beagle") %>%
  filter(sample_temp_c > 20 & sample_temp_c < 24)

