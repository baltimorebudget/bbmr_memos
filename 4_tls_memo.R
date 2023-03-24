# Generate TLS Target Memos

################################################################################
.libPaths("C:/Users/sara.brumfield2/OneDrive - City Of Baltimore/Documents/r_library")
devtools::load_all("G:/Analyst Folders/Sara Brumfield/_packages/bbmR")
devtools::load_all("G:/Budget Publications/automation/0_data_prep/bookHelpers")

library(magrittr)
library(tidyverse)
library(rio)
library(lubridate)
library(rmarkdown)
library(knitr)
library(kableExtra)
library(tinytex)
library(janitor)
#tinytex::install_tinytex()
#remotes::install_github('yihui/tinytex')

#set colors
colors <- bbmR::colors

##metadata ======
info <- list(
  analysts = import("G:/Analyst Folders/Sara Brumfield/_ref/Analyst Assignments.xlsx", which = "Agencies") %>%
    distinct(`Analyst`) %>%
    extract2("Analyst"),
  agencies = import("G:/Analyst Folders/Sara Brumfield/_ref/Analyst Assignments.xlsx", which = "Agencies") %>%
    filter(Operational == TRUE) %>%
    distinct(`Agency ID`, `Agency Name`, `Agency Name - Cleaned`),
  quasis = import("G:/Analyst Folders/Sara Brumfield/_ref/Analyst Assignments.xlsx", which = "Quasi"))


tls <- import("G:/Fiscal Years/Fiscal 2024/Planning Year/3. TLS/1. Line Item Reports/line_items_2023-03-13.xlsx",
              which = "Details") %>%
  mutate_if(is.numeric, replace_na, 0) %>%
  rename(`Service ID` = `Program ID`, `Service Name` = `Program Name`, `FY24 Request` = `FY24 PROP`) %>%
  unite("Service", `Service ID`:`Service Name`, sep = ": ")%>%
  group_by(`Agency ID`, `Agency Name`, `Service`, `Fund ID`, `Fund Name`) %>%
  summarise_at(vars(`FY23 Adopted`, `FY24 CLS`, `FY24 Request`, `FY24 TLS`, `$ - Change vs Adopted`, `% - Change vs Adopted`),
               sum, na.rm = TRUE)

performance <- import("G:/Budget Publications/automation/0_data_prep/dist/scorecard/Scorecard Citywide.xlsx", which = "Performance Measures") %>%
  rename(`Agency Name` = `Agency Name - Cleaned`) %>%
  select(`Agency Name`, `Service ID`, `Service Name`, Measure, `Actual 2022`, `Target 2024`)

##old code? =======
# adjustments <- tls %>%
#   select(-starts_with("FY")) %>%
#   pivot_longer(
#     cols = c(`DGS Rent Reallocation`:`Other Adjustments 1`,
#              `Other Adjustments 2`, `Other Adjustments 3`, 
#              `Other Adjustments 4`,
#              `Other Global Adjustments`, `COVID CARES or FEMA Funding`),
#     names_to = "Update", values_to = "Amount")
# 
# adj_type <- adjustments %>%
#   select(`Agency Name`:`Other Global Adjustments Type`) %>%
#   pivot_longer(
#     cols = c(`Adjustment Type 1`:`Other Global Adjustments Type`),
#     names_to = "Update", values_to = "Notes") %>%
#   filter(Notes != "0") %>%
#   distinct() %>%
#   mutate(Update = case_when(
#     Update == "Adjustment Type 1" ~ "Other Adjustments 1",
#     Update == "Adjustment Type 2" ~ "Other Adjustments 2",
#     Update == "Adjustment Type 3" ~ "Other Adjustments 3",
#     Update == "Adjustment Type 4" ~ "Other Adjustments 4",
#     Update == "Other Global Adjustments Type" ~ "Other Global Adjustments"
#   ))
# 
# adjustments <- adjustments %>%
#   select(-(`Adjustment Type 1`:`Other Global Adjustments Type`)) 
# 
# adjustments <- adjustments %>%
#   left_join(adj_type) %>%
#   mutate(Update = ifelse(Update %in% c("Other Adjustments 1", "Other Adjustments 2", 
#                                        "Other Adjustments 3", "Other Adjustments 4"),
#                          "Other Adjustments", Update)) %>%
#   filter(Amount != 0) %>%
#   relocate(Amount)



##pdf export ==============================
for(x in info$agencies$`Agency ID`) {
  
  knitr::knit_meta(class = NULL, clean = TRUE)
  
  agency <- info$agencies$`Agency Name`[info$agencies$`Agency ID` == x]
  agency_clean <-info$agencies$`Agency Name - Cleaned`[info$agencies$`Agency ID` == x]
  data <- tls
  
  rmarkdown::render(
    'r/tls_memo.qmd',
    output_file = paste0(
      "FY24 TLS ", 
      info$agencies$`Agency Name - Cleaned`[info$agencies$`Agency ID` == x], 
      ".pdf"),
    output_dir = "outputs/_FY24 TLS")
}

###quasi agencies
quasi_agencies <- c("Legal Aid",                                         "Family League",                                    
                    "Baltimore Heritage Area",                           "Lexington Market",                                 
                    "Baltimore Public Markets",                          "Visit Baltimore",                                  
                    "Baltimore Symphony Orchestra",                      "Walters Art Museum",                               
                    "Baltimore Museum of Art",                           "Maryland Zoo",                                    
                    "Baltimore Office of Promotion & the Arts")

quasi_data <- assign_quasi_agency(import("G:/Fiscal Years/Fiscal 2024/Planning Year/3. TLS/1. Line Item Reports/line_items_2023-03-13.xlsx",
                                         which = "Details")) %>%
  filter(`Agency Name` %in% quasi_agencies) %>%
  mutate_if(is.numeric, replace_na, 0) %>%
  rename(`Service ID` = `Program ID`, `Service Name` = `Program Name`, `FY24 Request` = `FY24 PROP`) %>%
  unite("Service", `Service ID`:`Service Name`, sep = ": ")%>%
  group_by(`Agency ID`, `Agency Name`, `Service`, `Fund ID`, `Fund Name`) %>%
  summarise_at(vars(`FY23 Adopted`, `FY24 CLS`, `FY24 Request`, `FY24 TLS`, `$ - Change vs Adopted`, `% - Change vs Adopted`),
               sum, na.rm = TRUE)

for(x in quase_agencies) {
  
  knitr::knit_meta(class = NULL, clean = TRUE)
  
  agency <- (quasi_data$`Agency Name`[quasi_data$`Agency Name` == x])[1]
  data <- quasi_data
  
  rmarkdown::render(
    'r/tls_memo.qmd',
    output_file = paste0(
      "FY24 TLS ", 
      agency, 
      ".pdf"),
    output_dir = "outputs/_FY24 TLS")
}

##pdf workaround ==================
n = 3
agency = agencies[n]
cls = tls$`FY23 CLS`[n]
tls = tls$`FY23 TLS`[n]
change = tls$`FY23 CLS to FY23 TLS Change`[n]
numbers = data.frame(`FY23 CLS` = cls, `FY23 TLS` = tls, `Change` = change)

updates <- filter(adjustments, `Agency Name` == agency) %>%
  select(-`Agency Name`)

rmarkdown::render(input = "r/FY23_TLS_Agency_Memos.Rmd", output_file = paste0(
  "FY23 TLS Prelim ", agency, ".pdf"),output_dir = "outputs/_FY23 TLS")
