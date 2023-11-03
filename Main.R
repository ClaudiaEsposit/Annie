library(renv)
renv::init
#---------------------------- a)Intro --------------------------------------####
cat('\014')
rm(list=ls())
setwd("~/Annie")
source("Library.R")
source("Functions.R")
source("Vanilla.R")
##----------------------------------------------------------------------------##
#----------------------------- b)Check -------------------------------------####
# Check if all the NDG from NDG are written in the Loans table
check_values_in_column(NDG, Loans, "NDG")
#Check where are NAs
Profile_LOANS <- ExpData(data=Loans,type=2) %>% as.data.frame()
Profile_LOANS <- Profile_LOANS %>%
  filter(!grepl("numeric", Variable_Type, ignore.case = TRUE))
Profile_LOANS <- Profile_LOANS %>% rename("Variable" = "Variable_Name", "Type" = "Variable_Type",
                                          "#_Entries" = "Sample_n", "NAs" = "Missing_Count", 
                                          "%_NAs" ="Per_of_Missing", "#_distinct_values" ="No_of_distinct_values")
Profile_LOANS$`%_NAs` <- paste0(Profile_LOANS$`%_NAs` *100, "%")
Profile_LOANS <- Profile_LOANS %>% select(-NAs)
#Check the values of total gbv and dates
Loans_check<- Loans%>%
  select(id.loans,id.bor,gbv.original,principal,interest,expenses,date.origination,date.status)%>%
  mutate(GBV_Check=expenses+principal+interest,
         check_gbv = ifelse(abs(GBV_Check - gbv.original) <= 0.01, "YES", "NO"),
         check_date_status= ifelse(date.status<=date.origination,"YES","NO"))
#It gives true if the number of yes is equal to the number of valid rows, so it is always confirmed
Loans_check %>%
  summarise(
    count_yes_check_gbv = sum(!is.na(check_gbv) & check_gbv == "YES"),
    non_na_rows_check_gbv = sum(!is.na(check_gbv)),
    count_yes_check_date_status = sum(!is.na(check_date_status) & check_date_status == "YES"),
    non_na_rows_check_date_status = sum(!is.na(check_date_status))
  ) %>%
  summarise(
    check_gbv_match = count_yes_check_gbv == non_na_rows_check_gbv,
    check_date_status_match = count_yes_check_date_status == non_na_rows_check_date_status
  )

##----------------------------------------------------------------------------##
#-------------------------- c)Normalization -------------------------------#####
primary_key(NDG)
primary_key(Loans)
# Functional dependence
#dependence_function(NDG, "NDG")
#dependence_function(Loans, "Loans")

##----------------------------------------------------------------------------##
#-------------------------- d)Loans_Table ---------------------------------#####
Loans_table <- Loans%>%
  mutate(originator=NA,ptf=NA,cluster.ptf=NA,penalties=NA,date.last.act=NA,
         flag.imputed=NA,id.loan=id.loans,status="Borrower",gbv.residual=NA,
         desc.type=`secured/unsecured`,desc.guarantors=role)%>%
  select(id.loan,id.bor,id.group,originator,ptf,cluster.ptf,type,status,
         gbv.original,gbv.residual,principal,interest,penalties,expenses,
         date.origination,date.status,date.last.act,flag.imputed,desc.type,desc.guarantors)%>%
  distinct()
##----------------------------------------------------------------------------##
#-------------------------- e)Entities_Table -------------------------------####
NDG[3, 5] <-"mrttbo64b19g565f , mrtmtn73e09d325h , mrtrfl68e13g565m ," 
# Split the name column and put "-" if it is empty
NDG_ent <- NDG %>%
  select(id.bor,name,cf.piva,city,province,region)%>%
  separate_rows(c(name,cf.piva), sep = ",| - ")%>%
  mutate(cf.piva = ifelse(cf.piva == "", "-", cf.piva))%>%
  rename_all(~ gsub(" ", "", .))%>%
  mutate_all(~ gsub(" {2,}", " ", trimws(.x)))%>%
  mutate_all(tolower)%>%
  distinct()

guarantors_ent <- Loans%>%
  select(id.bor,guarantorsname,taxcodeforguarantors)%>%
  separate_rows(c(guarantorsname,taxcodeforguarantors), sep = ",| - ")%>%
  rename_all(~ gsub(" ", "", .))%>%
  mutate_all(~ gsub(" {2,}", " ", trimws(.x)))%>%
  mutate_all(tolower)%>%
  distinct()

filtered_guarantors_ent <- guarantors_ent %>% filter(!is.na(guarantorsname))
num_rows_to_add <- nrow(filtered_guarantors_ent)
na_data <- data.frame(name = rep(NA, num_rows_to_add))
NDG_ent <- bind_rows(NDG_ent, na_data)

# Assign the "guarantorsname" values to the "name" column in the newly added rows
NDG_ent <- NDG_ent %>%
  mutate(name = ifelse(row_number() > (nrow(NDG_ent) - num_rows_to_add), filtered_guarantors_ent$guarantorsname, name),
         cf.piva = ifelse(row_number() > (nrow(NDG_ent) - num_rows_to_add), filtered_guarantors_ent$taxcodeforguarantors, cf.piva),
         id.bor = ifelse(row_number() > (nrow(NDG_ent) - num_rows_to_add), filtered_guarantors_ent$id.bor, id.bor)) %>%
  distinct()

Entities <- NDG_ent %>%
  mutate(type.subject = NA,dummy.info = NA,sex = NA,range.age = NA,age = NA,
         solvency.df = NA,income.df = NA,type.pg = NA,status.pg = NA,date.cessation = NA,area = NA,flag.imputed = NA) %>%
  select(name,cf.piva,type.subject,dummy.info,sex,range.age,age,solvency.df,income.df,type.pg,status.pg,date.cessation,city,
         province,region,area,flag.imputed) %>%
  distinct() %>%
  mutate(id.entity = paste("e_", 1:nrow(.), sep = "")) %>%
  select(id.entity, everything()) %>%
  add_age_column() %>%
  add_age_range_column() %>%
  add_type_column() %>%
  add_type_subject_column() %>%
  add_sex_column()

Entities$area <- Geo$area[match(Entities$region, Geo$region)]

Entities <- Entities%>%
  mutate(type.subject=as.factor(type.subject),dummy.info = as.integer(dummy.info),
         sex = as.factor(sex),age = as.integer(age),solvency.df = as.factor(solvency.df),
         income.df = as.numeric(income.df),type.pg = as.factor(type.pg),status.pg = as.factor(status.pg),
         province = as.factor(province),region = as.factor(region),area = as.factor(area),flag.imputed = as.integer(flag.imputed))
Entities$cf.piva <- ifelse(Entities$cf.piva=="","-", Entities$cf.piva)
#Entities$date.cessation <- as.date(Entities$date.cessation)

##----------------------------------------------------------------------------##
#------------------------ f)Counterparties_Table ---------------------------####
NDG_count <- NDG %>%
  select(id.bor,id.group,name,cf.piva,city,province,region)%>%
  mutate(cf.piva = ifelse(cf.piva == "", "-", cf.piva))%>%
  rename_all(~ gsub(" ", "", .))%>%
  mutate_all(~ gsub(" {2,}", " ", trimws(.x)))%>%
  mutate_all(tolower)%>%
  distinct()

guarantors_count <- Loans%>%
  select(id.bor,guarantorsname,taxcodeforguarantors)%>%
  rename_all(~ gsub(" ", "", .))%>%
  mutate_all(~ gsub(" {2,}", " ", trimws(.x)))%>%
  mutate_all(tolower)%>%
  distinct()

filtered_guarantors_count <- guarantors_count %>% filter(!is.na(guarantorsname))
num_rows_to_add <- nrow(filtered_guarantors_count)
na_data <- data.frame(name = rep(NA, num_rows_to_add))
NDG_count <- bind_rows(NDG_count, na_data)

# Assign the "GuarantorsName" values to the "name" column in the newly added rows
NDG_count$name[(nrow(NDG_count) - num_rows_to_add + 1):nrow(NDG_count)] <- filtered_guarantors_count$guarantorsname
NDG_count$cf.piva[(nrow(NDG_count) - num_rows_to_add + 1):nrow(NDG_count)] <- filtered_guarantors_count$taxcodeforguarantors
NDG_count$id.bor[(nrow(NDG_count) - num_rows_to_add + 1):nrow(NDG_count)] <- filtered_guarantors_count$id.bor
NDG_count<- NDG_count%>%
  mutate(role=NA)
NDG_count$role[(nrow(NDG_count) - num_rows_to_add + 1):nrow(NDG_count)] <- "Guarantor"
NDG_count$role <- ifelse(is.na(NDG_count$role), "Borrower", NDG_count$role)

NDG_count<-NDG_count%>%
  distinct()

Counterparties <- NDG_count%>%
  select(id.bor,role,id.group,name)%>%
  mutate(n.entities=NA,flag.imputed=NA)%>%
  distinct()
Counterparties$id.counterparty <- paste("c_",1:nrow(Counterparties), sep = "")
Counterparties$n.entities <- str_count(Counterparties$name, "[,-]") + 1
Counterparties <- Counterparties%>%
  select(id.counterparty,id.bor,id.group,role,name,n.entities,flag.imputed)%>%
  mutate(role = as.factor(role),n.entities = as.integer(n.entities),
         flag.imputed = as.integer(flag.imputed))

Counterparties$id.group <- ifelse(is.na(Counterparties$id.group),"-", Counterparties$id.group)

##----------------------------------------------------------------------------##
#--------------------------- g)Link_c_e_Table ------------------------------####
Counterparties_merge <- Counterparties %>%
  separate_rows(c(name), sep = ",| - ")%>%
  rename_all(~ gsub(" ", "", .))%>%
  mutate_all(~ gsub(" {2,}", " ", trimws(.x)))

merged_data <- merge(Counterparties_merge, Entities, by.x = "name", by.y = "name", all.x = TRUE)
link_c_e <- merged_data%>%
  select(id.counterparty,id.entity)%>%
  distinct()
#----------------------------h)Profiling------------------------------------####

#Loans table
Profile_loans <- ExpData(data=Loans_table,type=2) %>% as.data.frame()
Profile_loans <- Profile_loans %>%
  filter(!grepl("numeric", Variable_Type, ignore.case = TRUE))
Profile_loans <- Profile_loans %>% rename("Variable" = "Variable_Name", "Type" = "Variable_Type",
                                          "#_Entries" = "Sample_n", "NAs" = "Missing_Count", 
                                          "%_NAs" ="Per_of_Missing", "#_distinct_values" ="No_of_distinct_values")
Profile_loans$`%_NAs` <- paste0(Profile_loans$`%_NAs` *100, "%")
Profile_loans <- Profile_loans %>% select(-NAs)

Profile_numeric_loans <- ExpData(data=Loans_table,type=2, fun = c("mean", "median", "var")) %>% as.data.frame()
Profile_numeric_loans<-Profile_numeric_loans[complete.cases(Profile_numeric_loans),]
Profile_numeric_loans <- Profile_numeric_loans %>% rename("Variable" = "Variable_Name", "Type" = "Variable_Type",
                                              "#_Entries" = "Sample_n", "NAs" = "Missing_Count", 
                                              "%_NAs" ="Per_of_Missing", "#_distinct_values" ="No_of_distinct_values",
                                              "Mean (k)" = "mean", "Median (k)" = "median", "Var (M)" = "var")
Profile_numeric_loans$`% NAs` <- paste0(Profile_numeric_loans$`%_NAs` *100, "%")
Profile_numeric_loans$`Mean (k)` <- round(Profile_numeric_loans$`Mean (k)`/1000, 2)
Profile_numeric_loans$`Median (k)` <- round(Profile_numeric_loans$`Median (k)`/1000, 2)
Profile_numeric_loans$`Var (M)` <- round(Profile_numeric_loans$`Var (M)`/1000000, 2)
Profile_numeric_loans <- Profile_numeric_loans %>% select(-NAs)

#Counterparties table
Profile_Counterparties <- ExpData(data=Counterparties,type=2) %>% as.data.frame()
Profile_Counterparties <- Profile_Counterparties %>% rename("Variable" = "Variable_Name", "Type" = "Variable_Type",
                                                            "#_Entries" = "Sample_n", "NAs" = "Missing_Count", 
                                                            "%_NAs" ="Per_of_Missing", "#_distinct_values" ="No_of_distinct_values")
Profile_Counterparties$`%_NAs` <- paste0(Profile_Counterparties$`%_NAs` *100, "%")

#Entities table
Profile_Entities <- ExpData(data=Entities,type=2) %>% as.data.frame()
Profile_Entities <- Profile_Entities %>% rename("Variable" = "Variable_Name", "Type" = "Variable_Type",
                                                            "#_Entries" = "Sample_n", "NAs" = "Missing_Count", 
                                                            "%_NAs" ="Per_of_Missing", "#_distinct_values" ="No_of_distinct_values")
Profile_Entities$`%_NAs` <- paste0(Profile_Entities$`%_NAs` *100, "%")
#----------------------------i)Table--------------------------------------####
source("Tables_excel.R")
#------------------------------p)Excel-------------------------------------####

source("Excel_format.R")
wb <- createWorkbook()
addWorksheet(wb, "Tables")
writeDataTable(wb, 1, total_table, startRow = startRow, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("number","number","currency", "currency","currency")
addStyle(wb,1,style=createStyle(numFmt = "[>=1000] #,##0,\"K\";[=0]\"-\";#0"),rows=4, cols=2:6)
writeData(wb, 1, x = "Overview", startCol = startCol, startRow = startRow-1)
mergeCells(wb, 1, startCol:(startCol + ncol(total_table) - 1), rows = startRow-1)
addStyle(wb, 1, style = title_style,rows = startRow-1, cols = startCol:(startCol + ncol(total_table) - 1), gridExpand = TRUE)
addStyle(wb, 1, style = section_style,rows = startRow+nrow(total_table),
         cols = startCol:(startCol + ncol(total_table) - 1), gridExpand = TRUE,stack = TRUE)

#formatting sec unsec
startRow_updated <- startRow+nrow(total_table)+3
writeDataTable(wb, 1, updated_df, startRow = startRow_updated, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("general", "general", "number", "percentage", "currency", "currency", "percentage", "currency", "currency","percentage")
applyStylesToColumns(wb, updated_df, column_types, startRow_updated, startCol)
writeData(wb, 1, x = "Sec/Unsec", startCol = startCol, startRow = startRow_updated-1)
mergeCells(wb, 1, startCol:(startCol + ncol(updated_df) - 1), rows = startRow_updated-1)
applyCustomStyles(wb, updated_df, startRow_updated, startCol)

#formatting range
startRow_range <- startRow_updated+nrow(updated_df)+3
writeDataTable(wb,1, updated_range, startRow = startRow_range, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("general", "general", "number", "percentage", "currency", "currency", "percentage", "currency", "currency","percentage")
applyStylesToColumns(wb, updated_range, column_types, startRow_range, startCol)
writeData(wb, 1, x = "Sec/Unsec + Range GBV", startCol = startCol, startRow = startRow_range-1)
mergeCells(wb, 1, startCol:(startCol + ncol(updated_range) - 1), rows = startRow_range-1)
applyCustomStyles(wb, updated_range, startRow_range, startCol)

#formatting vintage
startRow_vintage <- startRow_range+nrow(updated_range)+3
writeDataTable(wb,1, updated_vintage, startRow = startRow_vintage, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("general", "general", "number", "percentage", "currency", "currency", "percentage", "currency", "currency","percentage")
applyStylesToColumns(wb, updated_range, column_types, startRow_vintage, startCol)
writeData(wb, 1, x = "Sec/Unsec + Range Vintage", startCol = startCol, startRow = startRow_vintage-1)
mergeCells(wb, 1, startCol:(startCol + ncol(updated_vintage) - 1), startRow_vintage-1)
applyCustomStyles(wb, updated_vintage, startRow_vintage, startCol)

#formatting loans
startRow_loans <- startRow_vintage+nrow(updated_vintage)+3
writeDataTable(wb,1, updated_loans, startRow = startRow_loans, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("general", "number", "percentage", "currency", "currency", "percentage", "currency", "currency","percentage")
applyStylesToColumns(wb, updated_loans, column_types, startRow_loans, startCol)
writeData(wb, 1, x = "Type Loans by Borrower", startCol, startRow_loans-1)
mergeCells(wb, 1, startCol:(startCol + ncol(updated_loans) - 1),startRow_loans-1)
applyCustomStyles(wb, updated_loans, startRow_loans, startCol)

#formatting guarantors
startRow_guarantors <- startRow_loans+nrow(updated_loans)+3
writeDataTable(wb,1, updated_guarantors, startRow = startRow_guarantors, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("general", "general", "number", "percentage", "currency","currency", "percentage")
applyStylesToColumns(wb, updated_guarantors, column_types, startRow_guarantors, startCol)
writeData(wb, 1, x = "Guarantors", startCol, startRow_guarantors-1)
mergeCells(wb, 1, startCol:(startCol + ncol(updated_guarantors) - 1), startRow_guarantors-1)
applyCustomStyles(wb, updated_guarantors, startRow_guarantors, startCol)

#formatting utp
startRow_utp <- startRow_guarantors+nrow(updated_guarantors)+3
writeDataTable(wb,1, updated_utp, startRow = startRow_utp, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("general", "number", "percentage", "currency", "currency", "percentage", "currency", "currency","percentage")
applyStylesToColumns(wb, updated_utp, column_types, startRow_utp, startCol)
writeData(wb, 1, x = "GBV by Status of Loan per Borrower", startCol, startRow_utp-1)
mergeCells(wb, 1, startCol:(startCol + ncol(updated_utp) - 1),startRow_utp-1)
applyCustomStyles(wb, updated_utp, startRow_utp, startCol)

#formatting entities
startRow_ent <- startRow_utp+nrow(updated_utp)+3
writeDataTable(wb,1, updated_ent, startRow = startRow_ent, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("general", "number", "percentage", "currency", "currency", "percentage", "currency", "currency","percentage")
applyStylesToColumns(wb, updated_ent, column_types, startRow_ent, startCol)
writeData(wb, 1, x = "GBV by number of Entities per Borrower", startCol, startRow_ent-1)
mergeCells(wb, 1, startCol:(startCol + ncol(updated_ent) - 1),startRow_ent-1)
applyCustomStyles(wb, updated_ent, startRow_ent, startCol)

#formatting province
startRow_pro <- startRow_ent+nrow(updated_ent)+3
writeDataTable(wb,1, updated_pro, startRow = startRow_pro, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("general", "number", "percentage", "currency", "currency", "percentage", "currency", "currency","percentage")
applyStylesToColumns(wb, updated_pro, column_types, startRow_pro, startCol)
writeData(wb, 1, x = "GBV by Province of Borrower", startCol, startRow_pro-1)
mergeCells(wb, 1, startCol:(startCol + ncol(updated_pro) - 1),startRow_pro-1)
applyCustomStyles(wb, updated_pro, startRow_pro, startCol)

#formatting type by loans
startRow_typ <- startRow_pro+nrow(updated_pro)+3
writeDataTable(wb,1, updated_type, startRow = startRow_typ, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("general", "number", "percentage", "currency", "currency", "percentage")
applyStylesToColumns(wb, updated_type, column_types, startRow_typ, startCol)
writeData(wb, 1, x = "GBV by Type of Loans", startCol, startRow_typ-1)
mergeCells(wb, 1, startCol:(startCol + ncol(updated_type) - 1),startRow_typ-1)
applyCustomStyles(wb, updated_type, startRow_typ, startCol)
saveWorkbook(wb,"Tables.xlsx", overwrite = TRUE)

#-----------------------------------------------------------------------------
renv::snapshot()