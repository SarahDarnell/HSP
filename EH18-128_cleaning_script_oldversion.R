#HSP analysis - Part 1 - EH18-128
#written by Sarah Darnell, last edited 2.27.25

#load necessary packages
library(readr)
library(dplyr)
#import HSP variables from redcap, remove redcap event names
eh18 <- read_csv("EH18-128_HSP_redcap.csv", 
        col_types = cols(redcap_event_name = col_skip()))
View(eh18)  

#fix NA values due to redcap event formatting
eh18_new <- eh18 %>% group_by(record_number) %>% 
        mutate(q10c_bmi = first(na.omit(q10c_bmi)),
               fatiguetotal = first(na.omit(fatiguetotal)),
               sleeptotal = first(na.omit(sleeptotal)),
               eddep_total = first(na.omit(eddep_total)),
               edanx_total = first(na.omit(edanx_total))) %>% 
        ungroup()

#transform from long to wide format
#first filter function removes rows with only a single instance,
#second filter function removes first row when 2 instances or removes first
#and last row when 3 instances
eh18_wide <- eh18_new %>% group_by(record_number) %>%
          filter(n()>1) %>% 
          filter((n()==2 & row_number() == 2 | n()==3 & row_number()==2)) %>%
          ungroup()

#Removing record_numbers not included in HSP analysis
eh18_hsp <- filter(eh18_wide, record_number != 4, record_number != 14,
                   record_number != 20, record_number != 23, 
                   record_number != 35)