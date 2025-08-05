#HSP analysis - Part 2 - EH19-040
#written by Sarah Darnell, began 2.28.25, last edited 8.5.25

library(readr)
library(dplyr)
library(tidyr)

setwd("C:/Users/Eli S/Documents/Sarah work stuff/2025 Data Projects/HSP")

#import HSP variables from redcap, remove redcap event names
eh19 <- read_csv("Raw files/EH19-040_HSP_redcap_NEW.csv", 
                 col_types = cols(redcap_event_name = col_skip(), 
                                  redcap_repeat_instrument = col_skip(), 
                                  redcap_repeat_instance = col_skip()))

#copy bmi and promis values across rows, rename bmi
eh19_new <- eh19 %>% 
  group_by(ps_record_id) %>% 
  mutate(ps_bmi = first(na.omit(ps_bmi)),
         total_fatigue_score = first(na.omit(total_fatigue_score)),
         sleep_disturbance_total = first(na.omit(sleep_disturbance_total)),
         eddep_total = first(na.omit(eddep_total)),
         total_anxiety_score = first(na.omit(total_anxiety_score))) %>% 
  rename(bmi = 2) %>%
  ungroup()

#copy cmsi variables across rows
eh19_new <- eh19_new %>%
  group_by(ps_record_id) %>% 
  mutate(cmsi_fibro1___99 = first(na.omit(cmsi_fibro1___99)),
         cmsi_fibro1___1 = first(na.omit(cmsi_fibro1___1)),
         cmsi_fibro1___2 = first(na.omit(cmsi_fibro1___2)),
         cmsi_fibro1___3 = first(na.omit(cmsi_fibro1___3)),
         cmsi_fibro1___4 = first(na.omit(cmsi_fibro1___4)),
         cmsi_fibro1___5 = first(na.omit(cmsi_fibro1___5)),
         cmsi_fibro1___6 = first(na.omit(cmsi_fibro1___6)),
         cmsi_fibro1___7 = first(na.omit(cmsi_fibro1___7)),
         cmsi_fibro1___8 = first(na.omit(cmsi_fibro1___8)),
         cmsi_fibro1___9 = first(na.omit(cmsi_fibro1___9)),
         cmsi_fibro1___10 = first(na.omit(cmsi_fibro1___10)),
         cmsi_fibro1___11 = first(na.omit(cmsi_fibro1___11)),
         cmsi_fibro1___12 = first(na.omit(cmsi_fibro1___12)),
         cmsi_fibro1___13 = first(na.omit(cmsi_fibro1___13)),
         cmsi_fibro1___14 = first(na.omit(cmsi_fibro1___14)),
         cmsi_fibro1___15 = first(na.omit(cmsi_fibro1___15)),
         cmsi_fibro1___16 = first(na.omit(cmsi_fibro1___16)),
         cmsi_fibro1___17 = first(na.omit(cmsi_fibro1___17)),
         cmsi_fibro1___18 = first(na.omit(cmsi_fibro1___18)),
         cmsi_fibro1___19 = first(na.omit(cmsi_fibro1___19))) %>%
  ungroup()

#removing subjects who didn't come in for an MRI
eh19_MRI <- eh19_new %>% 
  group_by(ps_record_id) %>%
  filter(n()>2) %>% #removes rows if less than 3 instances
  ungroup()

#isolate MRI 2 variables
eh19_MRI2 <- eh19_MRI %>%
  group_by(ps_record_id) %>%
  filter(n()>3) %>%
  slice_tail %>%
  select(53:56) %>%
  ungroup()

#renaming MRI 2 variables
eh19_MRI2 <- eh19_MRI2 %>% 
  rename(menses_q1_bs_cramp_MRI2 = 2) %>%
  rename(menses_q1_max_cramp_MRI2 = 3) %>%
  rename(menses_q4_5_baseline_MRI2 = 4) %>%
  rename(menses_q4_5_max_MRI2 = 5)

#removing phone screen and MRI2 variables from larger dataset
eh19_MRI <- eh19_MRI %>%
  group_by(ps_record_id) %>% 
  filter(n() < 4 | row_number() < 4) %>% #checks if 4 lines, removes 4th
  filter(row_number() > 1) %>% #removes first line of all groups
  ungroup()

#fix NA formatting of MRI 1 menses variables, and remove second line
eh19_MRI1 <- eh19_MRI %>% 
  group_by(ps_record_id) %>% 
  mutate(menses_q1_bs_cramp = first(na.omit(menses_q1_bs_cramp)),
         menses_q1_max_cramp = first(na.omit(menses_q1_max_cramp)),
         menses_q4_5_baseline = first(na.omit(menses_q4_5_baseline)),
         menses_q4_5_max = first(na.omit(menses_q4_5_max))) %>% 
  slice_head %>%
  ungroup()

#Join MRI2 variables to rest of dataset, in wide form
eh19_wide <- merge(eh19_MRI1, eh19_MRI2, all = TRUE)

#add new variable, named bodymap, that is the sum of all cmsi pain variables
eh19_wide <- eh19_wide %>%
  group_by(ps_record_id) %>%
  mutate(bodymap = sum(cmsi_fibro1___1, cmsi_fibro1___2, cmsi_fibro1___3, 
                       cmsi_fibro1___4, cmsi_fibro1___5, cmsi_fibro1___6,
                       cmsi_fibro1___7, cmsi_fibro1___8, cmsi_fibro1___9,
                       cmsi_fibro1___10, cmsi_fibro1___11, cmsi_fibro1___12,
                       cmsi_fibro1___13, cmsi_fibro1___14, cmsi_fibro1___15,
                       cmsi_fibro1___16, cmsi_fibro1___17, cmsi_fibro1___18,
                       cmsi_fibro1___19)) %>%
  ungroup()

#Removing record_numbers not included in HSP analysis
eh19_hsp <- filter(eh19_wide, ps_record_id != 1, ps_record_id != 2,
                   ps_record_id != 25, ps_record_id != 28, 
                   ps_record_id != 46, ps_record_id != 51,
                   ps_record_id != 75, ps_record_id != 85,
                   ps_record_id != 100, ps_record_id != 114,
                   ps_record_id != 138, ps_record_id != 190,
                   ps_record_id != 200, ps_record_id != 203,
                   ps_record_id != 213, ps_record_id != 240,
                   ps_record_id != 305, ps_record_id != 315,
                   ps_record_id != 46, ps_record_id != 318,
                   ps_record_id != 322, ps_record_id != 323,
                   ps_record_id != 326, ps_record_id != 330,
                   ps_record_id != 332,  ps_record_id != 336,
                   ps_record_id != 338,  ps_record_id != 339,
                   ps_record_id != 340,  ps_record_id != 341,
                   ps_record_id != 349,  ps_record_id != 351,
                   ps_record_id != 356,  ps_record_id != 357,
                   ps_record_id != 360)

#recoding race categories
#adding variable for multi race
eh19_hsp_new <- eh19_hsp %>%
  mutate(multi_race = ifelse(rowSums(
    select(., mh3_race___1:mh3_race___5, mh3_race___7)) > 1, 1, 0))

#adding varibale for other race, excluding those with multiple races
eh19_hsp_new <- eh19_hsp_new %>%
  mutate(other_race = ifelse((mh3_race___1 == "1") |
                               (mh3_race___3 == "1") |
                               (mh3_race___7 == "1") &
                               (multi_race != 1), 1, 0))

#adding new variable for white race, excluding those with multiple races
eh19_hsp_new <- eh19_hsp_new %>%
  mutate(white_race = ifelse((mh3_race___5 == "1") &
                               (multi_race != 1), 1, 0))

#adding new variable for asian race, excluding those with multiple races
eh19_hsp_new <- eh19_hsp_new %>%
  mutate(asian_race = ifelse((mh3_race___2 == "1") &
                               (multi_race != 1), 1, 0))

#adding new variable for black race, excluding those with multiple races
eh19_hsp_new <- eh19_hsp_new %>%
  mutate(black_race = ifelse((mh3_race___4 == "1") &
                               (multi_race != 1), 1, 0))

#adding new variable for missing race, excluding those with multiple races
eh19_hsp_new <- eh19_hsp_new %>%
  mutate(missing_race = ifelse((mh3_race___6 == "1") &
                               (multi_race != 1), 1, 0))

#adding 3000 to the record_number for merge later, renaming variable
eh19_hsp_new <- eh19_hsp_new %>%
  mutate(ps_record_id = ps_record_id + 3000) %>%
  rename(record_number = 1)

#converting back to a tibble
eh19_hsp_clean <- as_tibble(eh19_hsp_new)

#saving file
write_csv(eh19_hsp_clean, "Edited files/EH19-040_HSP_cleaned.csv")