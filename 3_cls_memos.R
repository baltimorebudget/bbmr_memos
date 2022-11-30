.libPaths("C:/Users/sara.brumfield2/OneDrive - City Of Baltimore/Documents/r_library")
library(tidyverse)
library(rio)
library(lubridate)
library(rmarkdown)
library(knitr)
library(kableExtra)
library(tinytex)
library(scales)

devtools::load_all("G:/Analyst Folders/Sara Brumfield/_packages/bbmR")

source("G:/Budget Publications/automation/0_data_prep/bookHelpers/R/tables.R")

analysts <- import("G:/Analyst Folders/Sara Brumfield/_ref/Analyst Assignments.xlsx") %>%
  filter(!is.na(Analyst)) %>%
  select(`Agency ID`:Analyst)

adjustments <- readxl::read_excel("G:/Fiscal Years/Fiscal 2024/Planning Year/1. CLS/1. Line Item Reports/line_items_2022-11-2_CLS FINAL AFTER BPFS.xlsx", 
                                  sheet = "Details" ) %>%
  mutate(`Agency ID` = as.numeric(`Agency ID`)) %>%
  left_join(analysts, by = "Agency ID")  %>%
  mutate(`Program ID` = as.character(`Program ID`),
         `$ Change vs FY23 Adopted` = `FY24 CLS` - `FY23 Adopted`,
         `Program Name` = str_replace_all(`Program Name`, "&", "and")) %>%
  rename(`Agency Name` = `Agency Name.x`) %>%
  select(-starts_with("...")) %>%
  filter(!is.na(`Agency Name`)) %>%
  relocate(`$ Change vs FY23 Adopted`, .after = `FY24 CLS`)

agencies <- unique(adjustments$`Agency Name`)
# remaining_agencies <- agencies[length(agencies) - (1:25)]

map(agencies, function(x) {
  
  knitr::knit_meta(class = NULL, clean = TRUE)
  agency_clean <- analysts$`Agency Name - Cleaned`[analysts$`Agency Name` == x]
  agency <- analysts$`Agency Name`[analysts$`Agency Name` == x]
  type <- "agency"
  
  
  rmarkdown::render(
    'r/cls_memo_main.qmd',
    output_file = paste0(
      "FY24 CLS ", 
      agency_clean, 
      ".pdf"),
    output_dir = "outputs")
} 
)



##quasis ========================
#assign quasi-agencies
adjustments$`Agency Name`[adjustments$`Program ID` == "493" & adjustments$`Activity ID` == 1] <- "Baltimore Symphony Orchestra"
adjustments$`Program ID`[adjustments$`Program ID` == "493" & adjustments$`Activity ID` == 1] <- "493c"
adjustments$`Agency Name`[adjustments$`Program ID` == "493" & adjustments$`Activity ID` %in% c(10,11)] <- "Walters Art Museum"
adjustments$`Program ID`[adjustments$`Program ID` == "493" & adjustments$`Activity ID` %in% c(10,11)] <- "493b"
adjustments$`Agency Name`[adjustments$`Program ID` == "493" & adjustments$`Activity ID` %in% c(14,15)] <- "Baltimore Museum of Art"
adjustments$`Program ID`[adjustments$`Program ID` == "493" & adjustments$`Activity ID` %in% c(14,15)] <- "493a"
adjustments$`Agency Name`[adjustments$`Program ID` == "493" & adjustments$`Activity ID` == 42] <- "Maryland Zoo"
adjustments$`Program ID`[adjustments$`Program ID` == "493" & adjustments$`Activity ID` == 42] <- "493d"
adjustments$`Agency Name`[adjustments$`Program ID` == "824"] <- "Baltimore Office of Promotion and the Arts"
adjustments$`Agency Name`[adjustments$`Program ID` == "820"] <- "Visit Baltimore"
adjustments$`Agency Name`[adjustments$`Program ID` == "385"] <- "Legal Aid"
adjustments$`Agency Name`[adjustments$`Program ID` == "446"] <- "Family League"
adjustments$`Agency Name`[adjustments$`Program ID` == "590" & adjustments$`Activity ID` == 32] <- "Baltimore Heritage Area"
adjustments$`Program ID`[adjustments$`Program ID` == "590" & adjustments$`Activity ID` == 32] <- "590c"
adjustments$`Agency Name`[adjustments$`Program ID` == "590" & adjustments$`Activity ID` == 38] <- "Lexington Market"
adjustments$`Program ID`[adjustments$`Program ID` == "590" & adjustments$`Activity ID` == 38] <- "590b"
adjustments$`Agency Name`[adjustments$`Program ID` == "590" & adjustments$`Activity ID` == 44] <- "Baltimore Public Markets"
adjustments$`Program ID`[adjustments$`Program ID` == "590" & adjustments$`Activity ID` == 44] <- "590a"

quasi_agencies <- c("Legal Aid",                                         "Family League",                                    
                    "Baltimore Heritage Area",                           "Lexington Market",                                 
                    "Baltimore Public Markets",                          "Visit Baltimore",                                  
                    "Baltimore Symphony Orchestra",                      "Walters Art Museum",                               
                    "Baltimore Museum of Art",                           "Maryland Zoo",                                    
                    "Baltimore Office of Promotion & the Arts")

quasis <- c(                     
  # "Legal Aid",                                         "Family League",
                                  "Baltimore Heritage Area",                           "Lexington Market",                                 
                                  "Baltimore Public Markets",                          "Visit Baltimore",                                  
                                  "Baltimore Symphony Orchestra",                      "Walters Art Museum",                               
                                  "Baltimore Museum of Art",                           "Maryland Zoo",                                    
                                  "Baltimore Office of Promotion and the Arts")

map(quasis, function(x) {
  agency = x
  
  knitr::knit_meta(class = NULL, clean = TRUE)
  rmarkdown::render(
    'r/cls_memo_single_line.qmd',
    output_file = paste0(
      "FY24 CLS ", 
      agency, 
      ".pdf"),
    output_dir = "outputs/_FY24 CLS")
})