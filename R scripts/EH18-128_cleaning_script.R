#HSP analysis - Part 1 - EH18-128
#written by Sarah Darnell, began 2.27.25, last edited 8.5.25

library(readr)
library(dplyr)
library(tidyr)

setwd("C:/Users/Eli S/Documents/Sarah work stuff/2025 Data Projects/HSP")

#import HSP variables from redcap, remove redcap event names
eh18 <- read_csv("Raw files/EH18-128_HSP_redcap_NEW.csv", 
                 col_types = cols(redcap_event_name = col_skip()))

#copy bmi values across rows, rename 
eh18_new <- eh18 %>% 
            group_by(record_number) %>% 
            mutate(q10c_bmi = first(na.omit(q10c_bmi))) %>% 
            rename(bmi = 2) %>%
            ungroup()

#transform from long to wide format
eh18_wide <- eh18_new %>% 
              group_by(record_number) %>%
              filter(n()>1) %>% #removes row if only a single instance
              slice_tail(n=1) %>% #keeps last row if 2 instances
              ungroup()

#add new variable, named bodymap, that is the sum of all cmsi pain variables
eh18_wide <- eh18_wide %>%
  group_by(record_number) %>%
  mutate(bodymap = sum(cmsi_fibro1___1, cmsi_fibro1___2, cmsi_fibro1___3, 
                       cmsi_fibro1___4, cmsi_fibro1___5, cmsi_fibro1___6,
                       cmsi_fibro1___7, cmsi_fibro1___8, cmsi_fibro1___9,
                       cmsi_fibro1___10, cmsi_fibro1___11, cmsi_fibro1___12,
                       cmsi_fibro1___13, cmsi_fibro1___14, cmsi_fibro1___15,
                       cmsi_fibro1___16, cmsi_fibro1___17, cmsi_fibro1___18,
                       cmsi_fibro1___19)) %>%
  ungroup()

#Removing record_numbers not included in HSP analysis
eh18_hsp <- filter(eh18_wide, record_number != 4, record_number != 14,
                   record_number != 20, record_number != 23, 
                   record_number != 35)

#import HSP variables from redcap for EH13, remove redcap event names and recID
eh13 <- read_csv("Raw files/EH13-094_HSP_redcap_NEW.csv", 
                  col_types = cols(record_number = col_skip(), 
                                    redcap_event_name = col_skip()))

#Isolating record 24 from remaining records
eh18_oth <- eh18_hsp %>%
                filter(record_number != 24)

#removing all but first 2 columns from record 24
eh18_24 <- eh18_hsp %>%
            filter(record_number == 24) %>%
            select(1:2)

#merging values from EH13 onto record 24, preserving EH18 BMI
eh18_24 <- merge(eh18_24, eh13, all = TRUE)

#merging record 24 back with remaining HSP variables
eh18_hsp_new <- merge(eh18_oth, eh18_24, all = TRUE)

#recoding race categories
#adding variable for multi race
eh18_hsp_new <- eh18_hsp_new %>%
                mutate(multi_race = ifelse(rowSums(
                select(., mh3_race___1:mh3_race___5)) > 1, 1, 0))

#adding variable for other race, excluding those with multiple races
eh18_hsp_new <- eh18_hsp_new %>%
                mutate(other_race = ifelse((mh3_race___1 == "1") |
                                           (mh3_race___3 == "1") &
                                            (multi_race != 1), 1, 0))

#adding new variable for white race, excluding those with multiple races
eh18_hsp_new <- eh18_hsp_new %>%
  mutate(white_race = ifelse((mh3_race___5 == "1") &
                               (multi_race != 1), 1, 0))

#adding new variable for asian race, excluding those with multiple races
eh18_hsp_new <- eh18_hsp_new %>%
  mutate(asian_race = ifelse((mh3_race___2 == "1") &
                               (multi_race != 1), 1, 0))

#adding new variable for black race, excluding those with multiple races
eh18_hsp_new <- eh18_hsp_new %>%
  mutate(black_race = ifelse((mh3_race___4 == "1") &
                               (multi_race != 1), 1, 0))

#adding new variable for missing race, excluding those with multiple races
eh18_hsp_new <- eh18_hsp_new %>%
  mutate(missing_race = ifelse((mh3_race___1 != "1") & (mh3_race___2 != "1") &
                                 (mh3_race___3 != "1") & (mh3_race___4 != "1") &
                                 (mh3_race___5 != "1") & (multi_race != 1), 1, 0))

#renaming promis variables to match other datasets
eh18_hsp_new <- eh18_hsp_new %>%
  rename(total_fatigue_score = 47) %>%
  rename(sleep_disturbance_total = 48) %>%
  rename(total_anxiety_score = 50)

#adding 2000 to the record_number for merge later
eh18_hsp_new <- eh18_hsp_new %>%
  mutate(record_number = record_number + 2000)

#converting back to a tibble
eh18_hsp_clean <- as_tibble(eh18_hsp_new)

#saving file
write_csv(eh18_hsp_clean, "Edited files/EH18-128_HSP_cleaned.csv")