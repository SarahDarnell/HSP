#HSP analysis - Part 5 - table 2
#written by Sarah Darnell, began 4.8.25, last edited 8.21.25

#load necessary packages
library(dplyr)
library(tidyr)
library(officer)
library(flextable)
library(readr)

setwd("C:/Users/Eli S/Documents/Sarah work stuff/2025 Data Projects/HSP")

#import final merged dataset
hsp <- read_csv("Edited files/hsp_final.csv")

#create new variable for depression t score
lookup_table_depression <- data.frame(
  eddep_total = 8:40,
  Dep_Tscore = c(
    37.1, 43.3, 46.2, 48.2, 49.8, 51.2, 52.3, 53.4, 54.3, 55.3,
    56.2, 57.1, 57.9, 58.8, 59.7, 60.7, 61.6, 62.5, 63.5, 64.4,
    65.4, 66.4, 67.4, 68.3, 69.3, 70.4, 71.4, 72.5, 73.6, 74.8,
    76.2, 77.9, 81.1))
hsp <- hsp %>%
  left_join(lookup_table_depression, by = "eddep_total")

#create new variable for anxiety t score
lookup_table_anxiety <- data.frame(
  total_anxiety_score = 7:35,
  anxiety_Tscore = c(
    36.3, 42.1, 44.7, 46.7, 48.4, 49.9, 51.3, 52.6, 53.8,
    55.1, 56.3, 57.6, 58.8, 60.0, 61.3, 62.6, 63.8, 65.1,
    66.4, 67.7, 68.9, 70.2, 71.5, 72.9, 74.3, 75.8, 77.4,
    79.5, 82.7))
hsp <- hsp %>%
  left_join(lookup_table_anxiety, by = "total_anxiety_score")

#create new variable for fatigue t score
lookup_table_fatigue <- data.frame(
  total_fatigue_score = 7:35,
  Tscore = c(
    29.4, 33.4, 36.9, 39.6, 41.9, 43.9, 45.8, 47.6, 49.2,
    50.8, 52.2, 53.7, 55.1, 56.4, 57.8, 59.2, 60.6, 62.0,
    63.4, 64.8, 66.3, 67.8, 69.4, 71.1, 72.9, 74.8, 77.1,
    79.8, 83.2))
hsp <- hsp %>%
  left_join(lookup_table_fatigue, by = "total_fatigue_score")

#create new variable for sleep disturbance t score
lookup_table_sleep <- data.frame(
  sleep_disturbance_total = 8:40,
  Tscore = c(
    28.9, 33.1, 35.9, 38.0, 39.8, 41.4, 42.9, 44.2, 45.5, 
    46.7, 47.9, 49.0, 50.1, 51.2, 52.2, 53.3, 54.3, 55.3, 
    56.3, 57.3, 58.3, 59.4, 60.4, 61.5, 62.6, 63.7, 64.9, 
    66.1,67.5, 69.0, 70.8, 73.0, 76.5))
hsp <- hsp %>%
  left_join(lookup_table_sleep, by = "sleep_disturbance_total")

#renaming continuous variables
hsp <- hsp %>%
  rename(Depression = 109) %>%
  rename(Anxiety = 110) %>%
  rename('Number of Body Pain Sites (0-19)' = 75) %>%
  rename('Sleep Disturbance' = 112) %>%
  rename(Fatigue = 111)

#Defining continuous variables
median_vars <- c("Depression", "Anxiety", "Number of Body Pain Sites (0-19)",
                 "Sleep Disturbance",
                 "Number of pregnancies",
                 "Fatigue")

#Creating table of continuous variables, with median [IQR]
table_median <- hsp %>%
  select(all_of(median_vars)) %>%
  dplyr::summarize(across(everything(), ~ sprintf("%.1f [%.1f-%.1f]", 
                                           median(., na.rm = TRUE), 
                                           quantile(., 0.25, na.rm = TRUE),
                                           quantile(., 0.75, na.rm = TRUE)))) %>%
  pivot_longer(cols = everything(), names_to = "Item", values_to = "Median [IQR] or %")

#Creating table of continuous variables, with Spearman's correlations and p-values

#gp96 spearman's calculations
gp96_results <- lapply(median_vars, function(var) {
  test_result <- cor.test(hsp$gp96, hsp[[var]], method = "spearman", exact = FALSE)
  rho <- test_result$estimate
  p_value <- test_result$p.value
  data.frame(
    Item = var,
    GP96 = rho,
    p_value = p_value, 
    stringsAsFactors = FALSE
  )
})

gp96_df <- do.call(rbind, gp96_results)
rownames(gp96_df) <- NULL

#grp78 spearman's calculations
grp78_results <- lapply(median_vars, function(var) {
  test_result <- cor.test(hsp$grp78, hsp[[var]], method = "spearman", exact = FALSE)
  rho <- test_result$estimate
  p_value <- test_result$p.value
  data.frame(
    Item = var,
    GRP78 = rho,
    p_value = p_value, 
    stringsAsFactors = FALSE
  )
})

grp78_df <- do.call(rbind, grp78_results)
rownames(grp78_df) <- NULL

#combine gp96 and grp78 tables
hsp_results <- merge(grp78_df, gp96_df, by = "Item")

#combine spearman's with median values
table_median_full <- merge(table_median, hsp_results, by = "Item")


#renaming yes/no variables
hsp <- hsp %>%
  rename(Migraines = 32) %>%
  rename('PBS/IC' = 20) %>%
  rename('Chronic Pelvic Pain' = 21) %>%
  rename(IBD = 28) %>%
  rename(Diabetes = 37) %>%
  rename(`Current use of birth control pills` = 43) %>%
  rename(`Past use of birth control pills` = 44)

#Defining yes/no variables
yes_vars <- c("Migraines", "PBS/IC", "Chronic Pelvic Pain", 
              "IBD", "Diabetes", "Current use of birth control pills", 
              "Past use of birth control pills")

#Creating table of yes/no variables, with %yes
table_yes <- hsp %>%
  select(all_of(yes_vars)) %>%
  dplyr::summarize(across(everything(), ~ {
    count_yes <- sum(., na.rm = TRUE)  
    total <- length(na.omit(.))  
    percent_yes <- (count_yes / total) * 100  # Percentage of "Yes"
    sprintf("%.1f%%", percent_yes)  # Format percentage to one decimal place
  })) %>%
  pivot_longer(cols = everything(), names_to = "Item", values_to = "Median [IQR] or %")

#Creating table of continuous variables, with kendall tau's and p-values

#gp96 kendall tau's calculations
gp96_results_yes <- lapply(yes_vars, function(var) {
  test_result <- cor.test(hsp$gp96, hsp[[var]], method = "kendall", exact = FALSE)
  tau <- test_result$estimate
  p_value <- test_result$p.value
  data.frame(
    Item = var,
    GP96 = tau,
    p_value = p_value, 
    stringsAsFactors = FALSE
  )
})

gp96_df_yes <- do.call(rbind, gp96_results_yes)
rownames(gp96_df_yes) <- NULL

#grp78 kendall tau's calculations
grp78_results_yes <- lapply(yes_vars, function(var) {
  test_result <- cor.test(hsp$grp78, hsp[[var]], method = "kendall", exact = FALSE)
  tau <- test_result$estimate
  p_value <- test_result$p.value
  data.frame(
    Item = var,
    GRP78 = tau,
    p_value = p_value, 
    stringsAsFactors = FALSE
  )
})

grp78_df_yes <- do.call(rbind, grp78_results_yes)
rownames(grp78_df_yes) <- NULL

#combine gp96 and grp78 tables
hsp_results_yes <- merge(grp78_df_yes, gp96_df_yes, by = "Item")

#combine kendall tau's with %yes values
table_yes_full <- merge(table_yes, hsp_results_yes, by = "Item")

#combine continuous table with %yes table
table_2 <- bind_rows(table_median_full, table_yes_full)



#saving table 2 as a word file
ft_table <- flextable(table_2)

# Create a Word document
doc <- read_docx()

# Add a title (optional)
doc <- doc %>%
  body_add_par("Table 2: Summary Statistics", style = "heading 1")

# Add the flextable to the Word document
doc <- doc %>%
  body_add_flextable(ft_table)

# Save the Word document
print(doc, target = "Tables/table2.docx")

  