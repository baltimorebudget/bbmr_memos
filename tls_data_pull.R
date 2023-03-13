.libPaths("C:/Users/sara.brumfield2/Anaconda3/envs/bbmr/Lib/R/library")
library(tidyverse)
library(magrittr)

.libPaths("G:/Data/r_library")
library(bbmR)

#use for function development -------------------------
params <- list(phase = "TLS", fy = 23)

##connect to db, all schemas, tables available ----------------------------------
con <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", Server = Sys.getenv("DB_BPFS_SERVER"), 
                      Database = "Finance_BPFS", UID = Sys.getenv("DB_BPFS_USER"), 
                      PWD = Sys.getenv("DB_BPFS_PW"))

##gather data =========================
tls_prev <- readxl::read_xlsx("inputs/FY22 TLS.xlsx", sheet = "TLS Targets") %>%
  select(`Agency Name`, `DGS Rent Reallocation`, `Workers' Compensation Update`) %>%
  group_by(`Agency Name`)%>%
  summarise_at(c("DGS Rent Reallocation", "Workers' Compensation Update"), sum)

tls_query <- DBI::dbGetQuery(conn = con, 
                                     statement = "SELECT 
                                    --agency_id AS 'Agency ID', 
                                    agency_name AS 'Agency Name', 
                                    --program_id AS 'Program ID', 
                                    program_name AS 'Program Name', 
                                    --activity_id AS 'Activity ID', 
                                    activity_name AS 'Activity Name',
                                    --subactivity_id AS 'Subactivity ID',
                                    --subactivity_name AS 'Subactivity Name', fund_id AS 'Fund ID',
                                    fund_name AS 'Fund Name', 
                                    --detailed_fund_id AS 'Detail Fund ID', detailed_fund_name 'Detail Fund Name',
                                    --object_id AS 'Object ID', object_name AS 'Object Name', subobject_id AS 'Subobject ID',
                                    --subobject_name AS 'Subobject Name', objective_id AS 'Objective ID', objective_name AS 'Objective Name',
                                    SUM(current_adopted) AS 'FY22 Adopted', SUM(cls_request) AS 'FY23 CLS', SUM(proposal1) AS 'FY23 Proposal',
                                    SUM(tls_request) AS 'FY23 TLS', SUM(finance_recommendation) AS 'FY23 FinRec',
                                    SUM(change_vs_adopted) AS '$ - Change vs Adopted', 
                                    FORMAT(CAST(SUM(perc_change_vs_adopted) AS decimal), 'p') AS '% - Change vs Adopted'
                                    --, justification
                                    FROM [Finance_BPFS].[planningyear23].LINE_ITEM_REPORT
                                    WHERE fund_id = '1001'
                                    GROUP BY agency_name, program_name, activity_name, fund_name
                                    ORDER BY agency_name, program_name")

tls_bbmr <- readxl::read_xlsx("inputs/FY23 Ranking Sheet.xlsx") %>%
  select(`Agency`, `Service Name`, `FY23 CLS General Fund`, `BBMR Rec Total`,`BBMR Rec GF vs. CLS`, `Notes`)

tls <- tls_bbmr %>% left_join(tls_query, by = c("Agency" = "Agency Name", "Service Name" = "Program Name")) %>%
  mutate(`CLS Check` = ifelse(`FY23 CLS` != `FY23 CLS General Fund`, "Mismatch", "Match"))

##Data Check ================
tls_check <-tls %>% filter(`CLS Check` == "Mismatch")

bbmR::export_excel(tls_check, file_name = "inputs/FY23_TLS_Mismatches.xlsx", tab_name = "Mismatched CLS")


##Prep file for export =======================
tls_export <- tls %>%
  select(`Agency`, `FY22 Adopted`, `FY23 CLS`, `BBMR Rec Total`, `Notes`) %>%
  filter((`FY23 CLS` != 0) & (`BBMR Rec Total` != 0)) %>%
  rename(`FY23 TLS` = `BBMR Rec Total`, `Agency Name` = `Agency`) %>%
  group_by(`Agency Name`) %>%
  summarise_at(vars(`FY22 Adopted`:`FY23 TLS`), sum) %>%
  mutate(`FY23 CLS to FY23 TLS Change` = `FY23 CLS` - `FY23 TLS`) %>%
  left_join(tls_prev, by = c("Agency Name" = "Agency Name")) %>%
  mutate_if(is.numeric, replace_na, 0) %>%
  add_column(!!!list("Other Adjustments" = NA, "Adjustment Type" = NA, "Other Global Adjustments" = NA, 
                  "Other Global Adjustments Type" = "Fleet and OPC Updates; Office Supply and Travel Savings; Etc. or Neutral Agency Adjustments",
                     "COVID CARES or FEMA Funding"= NA))

bbmR::export_excel(tls_export, file_name = "inputs/FY23 TLS Corrections.xlsx", tab_name = "FY23TLSCorrections")
#saveRDS(tls_export, file = paste0("inputs/", params$phase, params$fy, ".Rds"))