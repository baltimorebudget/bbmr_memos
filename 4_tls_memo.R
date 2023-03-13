# Generate TLS Target Memos

################################################################################
.libPaths("G:/Data/r_library")
library(bbmR)
library(bookHelpers)
library(magrittr)

.libPaths("C:/Users/sara.brumfield2/Anaconda3/envs/bbmr/Lib/R/library")
library(tidyverse)
library(rio)
library(lubridate)
library(rmarkdown)
library(knitr)
library(kableExtra)
library(tinytex)
#tinytex::install_tinytex()
#remotes::install_github('yihui/tinytex')

analysts <- import("G:/Analyst Folders/Sara Brumfield/_ref/Analyst Assignments.xlsx") %>%
  filter(!is.na(Analyst)) %>%
  select(`Agency Name`:Analyst)

tls <- import("inputs/FY22 TLS.xlsx", which = 1) %>%
  mutate_if(is.numeric, replace_na, 0)


adjustments <- tls %>%
  select(-starts_with("FY")) %>%
  pivot_longer(
    cols = c(`DGS Rent Reallocation`:`Other Adjustments 1`,
             `Other Adjustments 2`, `Other Adjustments 3`, 
             `Other Adjustments 4`,
             `Other Global Adjustments`, `COVID CARES or FEMA Funding`),
    names_to = "Update", values_to = "Amount")

adj_type <- adjustments %>%
  select(`Agency Name`:`Other Global Adjustments Type`) %>%
  pivot_longer(
    cols = c(`Adjustment Type 1`:`Other Global Adjustments Type`),
    names_to = "Update", values_to = "Notes") %>%
  filter(Notes != "0") %>%
  distinct() %>%
  mutate(Update = case_when(
    Update == "Adjustment Type 1" ~ "Other Adjustments 1",
    Update == "Adjustment Type 2" ~ "Other Adjustments 2",
    Update == "Adjustment Type 3" ~ "Other Adjustments 3",
    Update == "Adjustment Type 4" ~ "Other Adjustments 4",
    Update == "Other Global Adjustments Type" ~ "Other Global Adjustments"
  ))

adjustments <- adjustments %>%
  select(-(`Adjustment Type 1`:`Other Global Adjustments Type`)) 

adjustments <- adjustments %>%
  left_join(adj_type) %>%
  mutate(Update = ifelse(Update %in% c("Other Adjustments 1", "Other Adjustments 2", 
                                       "Other Adjustments 3", "Other Adjustments 4"),
                         "Other Adjustments", Update)) %>%
  filter(Amount != 0) %>%
  relocate(Amount)


agencies <- unique(tls$`Agency Name`)
#subagencies <- setdiff(unique(tls$`Subagency Name`), NA)

##pdf export ==============================
map(agencies, function(x) {
  
  knitr::knit_meta(class = NULL, clean = TRUE)
  
  type <- "agency"
  
  rmarkdown::render(
    'r/FY22/FY22_TLS_Target_Memos.Rmd',
    output_file = paste0(
      "FY23 TLS Prelim ", 
      analysts$`Agency Name - Cleaned`[analysts$`Agency Name` == x], 
      ".pdf"),
    output_dir = "outputs/_FY24 TLS")
  }
)

map(agencies, function(x) {
  
  knitr::knit_meta(class = NULL, clean = TRUE)
  
  type <- "agency"
  
  rmarkdown::render(
    'r/FY22/FY22_TLS_Target_Memos.Rmd',
    output_file = paste0(
      "FY23 TLS Prelim ", x, ".pdf"),
    output_dir = "outputs/_FY23 TLS")
}
)

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
