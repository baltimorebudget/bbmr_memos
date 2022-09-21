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

analysts <- import("G:/Analyst Folders/Sara Brumfield/_ref/Analyst Assignments.xlsx", sheet = "Agencies") %>%
  filter(!is.na(Analyst)) %>%
  select(`Agency ID`:Analyst)

#quasi-agencies
quasi <- import("G:/Analyst Folders/Sara Brumfield/_ref/Analyst Assignments.xlsx", sheet = "Quasi")

pm <- import("C:/Users/sara.brumfield2/OneDrive - City Of Baltimore/Scorecard PMs_Recommendations_BBMR.xlsx", which = "Scorecard MAP") %>%
  select(`Agency Name`, `Service.ID`, `Program Name`, `Scorecard Measure`, `BBMR Decision`) %>%
  filter(grepl("NO|MAYBE", `BBMR Decision`)) %>%
  rename("Service ID" = "Service.ID")

#separate agencies needing memos with tables and those without
agencies <- pm %>% left_join(adjustments, by = c("Agency Name"= "Agency Name.x")) %>% filter(!is.na(`Agency Name.y`))
agencies <- unique(agencies$`Agency Name.y`)
no_agencies <- adjustments %>% anti_join(pm, by = c("Agency Name.x"= "Agency Name")) %>% filter(!is.na(`Agency Name.x`))
no_agencies <- unique(no_agencies$`Agency Name.x`)

check <- import("C:/Users/sara.brumfield2/OneDrive - City Of Baltimore/Scorecard PMs_Recommendations_BBMR.xlsx", which = "Scorecard MAP") %>%
  filter(grepl("NO|MAYBE", `BBMR Decision`)) %>%
  rename("Service ID" = "Service.ID") %>%
  select(`Agency Name`, `Scorecard Measure`) %>%
  group_by(`Agency Name`) %>%
  summarise(Target = n()) %>%
  mutate(`Result` = 0) %>%
  filter(!is.na(`Agency Name`))

map(agencies, function(x) {
  
  knitr::knit_meta(class = NULL, clean = TRUE)
  agency_clean <- analysts$`Agency Name - Cleaned`[analysts$`Agency Name` == x]
  agency <- analysts$`Agency Name`[analysts$`Agency Name` == x]
  type <- "agency"
  
  
  rmarkdown::render(
    'r/pm_memo.qmd',
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
    'r/pm_memo_no_cut.qmd',
    output_file = paste0(
      "FY24 CLS ", 
      agency_clean, 
      ".pdf"),
    output_dir = "outputs/_FY24 CLS")
} 
)

check %>% filter(Target != Result)