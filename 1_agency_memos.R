.libPaths("C:/Users/sara.brumfield2/OneDrive - City Of Baltimore/Documents/r_library")
library(tidyverse)
library(rio)
library(lubridate)
library(rmarkdown)
library(knitr)
library(kableExtra)
library(tinytex)
library(scales)

devtools::load_all("G:/Analyst Folders/Sara Brumfield/bbmR")

analysts <- import("G:/Analyst Folders/Sara Brumfield/_ref/Analyst Assignments.xlsx") %>%
  filter(!is.na(Analyst)) %>%
  select(`Agency ID`:Analyst)

#quasi-agencies
quasi <- import("G:/Analyst Folders/Sara Brumfield/_ref/Analyst Assignments.xlsx", sheet = "Quasi")

adjustments <- readxl::read_excel("G:/Fiscal Years/Fiscal 2024/Planning Year/1. CLS/1. Line Item Reports/line_items_2022-09-16_CLS_After_186.xlsx", 
                                  sheet = "Details" ) %>%
  mutate(`Agency ID` = as.numeric(`Agency ID`)) %>%
  left_join(analysts, by = "Agency ID")  %>%
  mutate(`Program ID` = as.character(`Program ID`)) %>%
  left_join(quasi, by = c("Agency ID", "Activity ID")) %>%
  mutate(`Program ID` = case_when(`Agency Name.x` %in% c("M-R: Art and Culture", "M-R: Civic Promotion", "M-R: Health and Welfare Grants", "M-R: Educational Grants") 
                                  ~ `Program ID.y`, TRUE ~ `Program ID.x`),
         `Agency Name` = case_when(`Agency Name.x` %in% c("M-R: Art and Culture", "M-R: Civic Promotion", "M-R: Health and Welfare Grants", "M-R: Educational Grants") 
                                   ~ `Agency Name`, TRUE ~ `Agency Name.x`)) %>%
  select(-ends_with(".x"), -ends_with(".y"))


#get list of agencies to make memos for
agencies <- unique(adjustments$`Agency Name`)
agencies <- agencies[!is.na(agencies)]

map(agencies, function(x) {
  
  knitr::knit_meta(class = NULL, clean = TRUE)
  if (x %in% c("M-R: Art and Culture", "M-R: Civic Promotion", "M-R: Health and Welfare Grants", "M-R: Educational Grants")) {
    agency_clean <- adjustments$`Agency Name`
    agency <- adjustments$`Agency Name`
  } else {
  agency_clean <- analysts$`Agency Name - Cleaned`[analysts$`Agency Name` == x]
  agency <- analysts$`Agency Name`[analysts$`Agency Name` == x]
  }
  type <- "agency"
  
  
  rmarkdown::render(
    'r/cls_memo.qmd',
    output_file = paste0(
      "FY24 CLS ", 
      agency_clean, 
      ".pdf"),
    output_dir = "outputs/_FY24 CLS")
} 
)

map(no_agencies, function(x) {
  
  knitr::knit_meta(class = NULL, clean = TRUE)
  agency_clean <- analysts$`Agency Name - Cleaned`[analysts$`Agency Name` == x]
  agency <- analysts$`Agency Name`[analysts$`Agency Name` == x]
  type <- "agency"
  
  
  rmarkdown::render(
    'r/cls_memo_no_cut.qmd',
    output_file = paste0(
      "FY24 CLS ", 
      agency_clean, 
      ".pdf"),
    output_dir = "outputs/_FY24 CLS")
} 
)

check %>% filter(Target != Result)