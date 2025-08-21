#HSP analysis - Part 4 - merged dataset and table 1
#written by Sarah Darnell, began 3.3.25, last edited 8.21.25

library(readr)
library(tableone)
library(Hmisc)
library(dplyr)
library(flextable)
library(officer)

setwd("C:/Users/Eli S/Documents/Sarah work stuff/2025 Data Projects/HSP")

#import cleaned datasets
eh16 <- read_csv("Edited files/EH16-263_HSP_cleaned.csv")
eh18 <- read_csv("Edited files/EH18-128_HSP_cleaned.csv")
eh19 <- read_csv("Edited files/EH19-040_HSP_cleaned.csv")

#import chandru values and groups
chandru <- read_csv("Raw files/hsp_chandru_values.csv")

#merge datasets
hsp <- merge(eh16, eh18, all = TRUE)
hsp <- merge(hsp, eh19, all = TRUE)
hsp <- merge(hsp, chandru, all = TRUE)

#remove na values from fibroid and endo groups
hsp <- hsp %>% filter(!is.na(gp96))

#adding variable for study identifier, for group comparisons
hsp <- hsp %>%
  mutate(study = if_else(record_number < 2000, "EH16", 
                         if_else(record_number > 3000, "EH19", "EH18"))) 

#converting back to a tibble
hsp <- as_tibble(hsp)

#Adding new column for race as a categorical variable, with new names
hsp <- hsp %>%
  mutate(Race = case_when(
    white_race == 1 ~ "White",
    black_race == 1 ~ "Black", 
    asian_race == 1 ~ "Asian", 
    other_race == 1 ~ "Other", 
    missing_race == 1 ~ "Missing", 
    multi_race == 1 ~ "Multiple Race"
  ))

#Adding new column for ethnicity as a categorical variable
hsp <- hsp %>%
  mutate(Ethnicity = case_when(
    mh4_ethnicity == 1 ~ "Hispanic or Latino/a/x", 
    mh4_ethnicity == 2 ~ "Not Hispanic or Latino/a/x"
  ))

#Adding new column for education as a categorical variable
hsp <- hsp %>%
  mutate(Education = case_when(
    mh5_education == 1 ~ "Grade School", 
    mh5_education == 2 ~ "Completed High School",
    mh5_education == 3 ~ "Some College", 
    mh5_education == 4 ~ "Associate's Degree", 
    mh5_education == 5 ~ "Bachelor's Degree", 
    mh5_education == 6 ~ "Postgraduate Degree"
  ))

#Adding new column for employment as a categorical var
#including all employment options, so %s are correct, but will only report
#unemployment in the table
hsp <- hsp %>%
  mutate(Unemployment = case_when(
    mh6_employment___7 == 1 ~ "Unemployed",
    mh6_employment___1 == 1 ~ "Work Full-time", #not in table
    mh6_employment___2 == 1 ~ "Work Part-time", #not in table
    mh6_employment___3 == 1 ~ "Homemaker", #not in table
    mh6_employment___4 == 1 ~ "Retired", #not in table
    mh6_employment___5 == 1 ~ "Disabled", #not in table
    mh6_employment___6 == 1 ~ "Student" #not in table
  ))

#Adding new column for cigarette usage as a cat var
hsp <- hsp %>%
  mutate('Do you smoke cigarettes?' = case_when(
    mh9a_cigs_yn == 1 ~ "Yes", 
    mh9a_cigs_yn == 0 ~ "No", 
    mh9a_cigs_yn == 2 ~ "Not currently, but I used to"
  ))

#Adding new column for alcohol consumption as a cat var
hsp <- hsp %>%
  mutate('Do you drink alcohol?' = case_when(
    mh9b_alcohol_yn == 1 ~ "Yes", 
    mh9b_alcohol_yn == 0 ~ "No", 
    mh9b_alcohol_yn == 2 ~ "Not currently, but I used to"
  ))

#Adding new column for regular menstrual cycles as a cat var
hsp <- hsp %>%
  mutate('Menstrual Cycle Regularity' = case_when(
    mh24 == 2 ~ "Always regular (22-34 days)", 
    mh24 == 1 ~ "Sometimes irregular", 
    mh24 == 0 ~ "Usually or always irregular"
  ))

#Adding new column for current bc usage as a cat var
hsp <- hsp %>%
  mutate('Current usage of birth control pills' = case_when(
    mh16_bcps___1 == 1 ~ "Yes", 
    mh16_bcps___1 == 0 ~ "No"
  ))

#Adding new column for past bc usage as a cat var
hsp <- hsp %>%
  mutate('Past usage of birth control pills' = case_when(
    mh17_bcps___1 == 1 ~ "Yes", 
    mh17_bcps___1 == 0 ~ "No"
  ))

#renaming variables for ease of reading in the table
hsp <- hsp %>%
  rename(Age = "mh2_age") %>%
  rename('Average menstrual pain (last 90 days without pain relievers)' = 
           "mh23") %>%
  rename('Average menstrual pain (last 90 days with use of NSAIDs)' = 
           "mh23a") %>%
  rename('Days of missed work/school/activities due to menstrual pain (last 90 days)' = 
           "mh21") %>%
  rename('Average length of menstrual cycle' = "mh25") %>%
  rename('Average length of menstrual period' = "mh27") %>%
  rename('Number of pregnancies' = "mh13_ofpregs") %>%
  rename('Number of deliveries' = "mh14_deliveries") %>%
  rename('Number of vaginal births' = "mh15_vagbirths") %>%
  rename(BMI = "bmi") %>%
  rename('Painful Bladder Syndrome or Interstitial Cystitis' = "have_you_ever_been_diagnos___1") %>%
  rename('Chronic Pelvic Pain' = "have_you_ever_been_diagnos___2") %>%
  rename(Fibroids = "have_you_ever_been_diagnos___3") %>%
  rename(Endometriosis = "have_you_ever_been_diagnos___4") %>%
  rename('Ovarian Cysts' = "have_you_ever_been_diagnos___5") %>%
  rename('Pelvic Inflammatory Disease' = "have_you_ever_been_diagnos___6") %>%
  rename(Dysmenorrhea = "have_you_ever_been_diagnos___7") %>%
  rename('Kidney Stones' = "have_you_ever_been_diagnos___8") %>%
  rename('Inflammatory Bowel Disease' = "have_you_ever_been_diagnos___9") %>%
  rename('Irritable Bowel Disease' = "have_you_ever_been_diagnos___10") %>%
  rename('Chronic Constipation' = "have_you_ever_been_diagnos___11") %>%
  rename('Chronic Diarrhea' = "have_you_ever_been_diagnos___12") %>%
  rename('Migraine Headaches' = "have_you_ever_been_diagnos___13") %>%
  rename(Hypertension = "have_you_ever_been_diagnos___14") %>%
  rename(Arthritis = "have_you_ever_been_diagnos___15") %>%
  rename('Lower Back Pain' = "have_you_ever_been_diagnos___16") %>%
  rename(Cancer = "have_you_ever_been_diagnos___17") %>%
  rename(Diabetes = "have_you_ever_been_diagnos___18") %>%
  rename(Fibromyalgia = "have_you_ever_been_diagnos___19") %>%
  rename(None = "have_you_ever_been_diagnos___0") 

#saving file
write_csv(hsp, "Edited files/hsp_final.csv")

#Creating demographics table, using tableone()
vars <- c("Race", "Age", "Ethnicity", "Education", "Unemployment", 
          "Do you smoke cigarettes?", "Do you drink alcohol?", 
          "Average menstrual pain (last 90 days without pain relievers)", 
          "Average menstrual pain (last 90 days with use of NSAIDs)", 
          "Days of missed work/school/activities due to menstrual pain (last 90 days)", 
          "Average length of menstrual cycle", "Average length of menstrual period", 
          "Menstrual Cycle Regularity", "Current usage of birth control pills", 
          "Past usage of birth control pills", "Number of pregnancies", 
          "Number of deliveries", "Number of vaginal births", "BMI",
          "Painful Bladder Syndrome or Interstitial Cystitis", "Chronic Pelvic Pain", 
          "Fibroids", "Endometriosis", "Ovarian Cysts", "Pelvic Inflammatory Disease", 
          "Dysmenorrhea", "Kidney Stones", "Inflammatory Bowel Disease", 
          "Irritable Bowel Disease", "Chronic Constipation", "Chronic Diarrhea", 
          "Migraine Headaches", "Hypertension", "Arthritis", "Lower Back Pain", 
          "Cancer", "Diabetes", "Fibromyalgia")
factors <- c("Race", "Ethnicity", "Education", "Unemployment", 
             "Do you smoke cigarettes?", "Do you drink alcohol?", 
             "Current usage of birth control pills", "Past usage of birth control pills", 
             "Painful Bladder Syndrome or Interstitial Cystitis", "Chronic Pelvic Pain", 
             "Fibroids", "Endometriosis", "Ovarian Cysts", "Pelvic Inflammatory Disease", 
             "Dysmenorrhea", "Kidney Stones", "Inflammatory Bowel Disease", 
             "Irritable Bowel Disease", "Chronic Constipation", "Chronic Diarrhea", 
             "Migraine Headaches", "Hypertension", "Arthritis", "Lower Back Pain", 
             "Cancer", "Diabetes", "Fibromyalgia")

demo <- CreateTableOne(vars, data = hsp, factorVars = factors, strata = "study") 
#change strata argument to "study" or "group" based on what is needed

print(demo, nonnormal = c("Age", "BMI", 
                          "Average menstrual pain (last 90 days without pain relievers)", 
                          "Average menstrual pain (last 90 days with use of NSAIDs)", 
                          "Days of missed work/school/activities due to menstrual pain (last 90 days)")
      , showAllLevels = TRUE)

#to save file for exporting
demo_df <- as.data.frame(print(demo, 
                 nonnormal = c("Age", "BMI", 
                               "Average menstrual pain (last 90 days without pain relievers)", 
                               "Average menstrual pain (last 90 days with use of NSAIDs)", 
                               "Days of missed work/school/activities due to menstrual pain (last 90 days)"),
                 printToggle = FALSE,
                 quote = FALSE,
                 noSpaces = TRUE,
                 showAllLevels = TRUE))

#chat gpt wrote this part
# Remove p-value/test columns
cols_to_remove <- c("p", "test")
demo_df <- demo_df[, !colnames(demo_df) %in% cols_to_remove]

#Step 1: save rownames as a column
demo_df <- data.frame(rowname = rownames(demo_df), demo_df, row.names = NULL)

# Step 2: Create an empty output data frame
restructured_df <- data.frame()

# Step 3: Loop through rows and insert variable name before its levels
current_var <- NA

for (i in seq_len(nrow(demo_df))) {
  row_label <- demo_df$rowname[i]
  if (!startsWith(row_label, "  ")) {
    # Continuous variable row — use as is
    current_var <- row_label
    new_row <- demo_df[i, ]
    new_row$Variable <- current_var
    restructured_df <- bind_rows(restructured_df, new_row)
  } else {
    # Categorical level — insert variable name row if not already added
    if (!identical(tail(restructured_df$Variable, 1), current_var)) {
      var_row <- demo_df[i, ]
      var_row[2:ncol(var_row)] <- ""  # clear values
      var_row$Variable <- current_var
      restructured_df <- bind_rows(restructured_df, var_row)
    }
    level_row <- demo_df[i, ]
    level_row$Variable <- ""
    restructured_df <- bind_rows(restructured_df, level_row)
  }
}

# Step 4: Drop original rowname and reorder
restructured_df <- restructured_df[, c("Variable", setdiff(names(restructured_df), c("rowname", "Variable")))]

#building flextable
ft <- flextable(demo_df) %>%
  bold(i = which(demo_df$Variable != ""), j = 1) %>%            # Bold variable rows
  align(align = "left", part = "all") %>%                        # Align left
  fontsize(size = 9, part = "all") %>%                           # Reduce font size
  set_table_properties(layout = "fixed", width = 1) %>%          # Fixed width layout
  width(j = 1, width = 2.25) %>%                                 # Widen first column for variable names
  width(j = 2:ncol(demo_df), width = 1.25) %>%                   # Narrow group columns
  theme_vanilla()

read_docx() %>%
  body_add_flextable(ft) %>%
  print(target = "Tables/Table1_study.docx")
