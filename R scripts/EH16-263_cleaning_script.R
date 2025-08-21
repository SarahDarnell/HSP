#HSP analysis - Part 3 - EH16-263
#written by Sarah Darnell, began 3.3.25, last edited 8.21.25

library(readr)
library(dplyr)
library(tidyr)

setwd("C:/Users/Eli S/Documents/Sarah work stuff/2025 Data Projects/HSP")

#import HSP variables from redcap, remove redcap event names
eh16 <- read_csv("Raw files/EH16-263_HSP_redcap_NEW.csv", 
                 col_types = cols(redcap_event_name = col_skip()))

#copy bmi values across rows, rename 
eh16_new <- eh16 %>% 
  group_by(record_number) %>% 
  mutate(q4_bmi = first(na.omit(q4_bmi))) %>% 
  rename(bmi = 2) %>%
  ungroup()

#transform from long to wide format
eh16_wide <- eh16_new %>% 
  group_by(record_number) %>%
  filter(n()>1) %>% #removes row if only a single instance
  slice_tail(n=1) %>% #keeps last row if 2 instances
  ungroup()

#add new variable, named bodymap, that is the sum of all cmsi pain variables
eh16_wide <- eh16_wide %>%
  group_by(record_number) %>%
  mutate(bodymap = sum(cmsi_fibro1___1, cmsi_fibro1___2, cmsi_fibro1___3, 
                       cmsi_fibro1___4, cmsi_fibro1___5, cmsi_fibro1___6,
                       cmsi_fibro1___7, cmsi_fibro1___8, cmsi_fibro1___9,
                       cmsi_fibro1___10, cmsi_fibro1___11, cmsi_fibro1___12,
                       cmsi_fibro1___13, cmsi_fibro1___14, cmsi_fibro1___15,
                       cmsi_fibro1___16, cmsi_fibro1___17, cmsi_fibro1___18,
                       cmsi_fibro1___19)) %>%
  ungroup()

#import menses visit information from tracking log
eh16_menses <- read_csv("Raw files/EH16_263_MRI_pain_fixed.csv", 
                        col_types = cols(...6 = col_skip(), ...7 = col_skip(), 
                                         ...8 = col_skip()))

#merge menses variables with rest of dataset
eh16_all <- merge(eh16_wide, eh16_menses, all = TRUE)

#Removing record_numbers not included in HSP analysis
eh16_hsp <- filter(eh16_all, record_number != 1, record_number !=2,
                   record_number != 3, record_number != 4, 
                   record_number != 5, record_number != 6, 
                   record_number != 12, record_number != 13, 
                   record_number != 16, record_number != 17, 
                   record_number != 18, record_number != 32, 
                   record_number != 5, record_number != 37, 
                   record_number != 70, record_number != 89)

#recoding race categories
#adding variable for multi race
eh16_hsp_new <- eh16_hsp %>%
  mutate(multi_race = ifelse(rowSums(
    select(., mh3_race___1:mh3_race___5)) > 1, 1, 0))

#adding variable for other race, excluding those with multiple races
eh16_hsp_new <- eh16_hsp_new %>%
  mutate(other_race = ifelse(multi_race != 1 & 
                               (mh3_race___1 == 1|
                                  mh3_race___3 == 1), 1, 0))

#adding new variable for white race, excluding those with multiple races
eh16_hsp_new <- eh16_hsp_new %>%
  mutate(white_race = ifelse((mh3_race___5 == 1) &
                               (multi_race != 1), 1, 0))

#adding new variable for asian race, excluding those with multiple races
eh16_hsp_new <- eh16_hsp_new %>%
  mutate(asian_race = ifelse((mh3_race___2 == 1) &
                               (multi_race != 1), 1, 0))

#adding new variable for black race, excluding those with multiple races
eh16_hsp_new <- eh16_hsp_new %>%
  mutate(black_race = ifelse((mh3_race___4 == 1) &
                               (multi_race != 1), 1, 0))

#adding new variable for missing race, excluding those with multiple races
eh16_hsp_new <- eh16_hsp_new %>%
  mutate(missing_race = ifelse((mh3_race___1 != 1) & (mh3_race___2 != 1) &
                                 (mh3_race___3 != 1) & (mh3_race___4 != 1) &
                                 (mh3_race___5 != 1) & (multi_race != 1), 1, 0))

#adding 1000 to the record_number for merge later
eh16_hsp_new <- eh16_hsp_new %>%
  mutate(record_number = record_number + 1000)

#converting back to a tibble
eh16_hsp_clean <- as_tibble(eh16_hsp_new)

#saving file
write_csv(eh16_hsp_clean, "Edited files/EH16-263_HSP_cleaned.csv")
