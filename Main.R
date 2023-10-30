library(renv)
renv::init
#---------------------------- a)INTRO --------------------------------------####
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
dependence_function(NDG, "NDG")
dependence_function(Loans, "Loans")

##----------------------------------------------------------------------------##
#-------------------------- d)Loans_Table ---------------------------------#####
Loans_table <- Loans%>%
  mutate(originator=NA,ptf=NA,cluster.ptf=NA,penalties=NA,date.last.act=NA,flag.imputed=NA,id.loan=id.loans,status="Borrower",gbv.residual=NA)%>%
  select(id.loan,id.bor,id.group,originator,ptf,cluster.ptf,type,status,gbv.original,gbv.residual,principal,interest,penalties,expenses,date.origination,date.status,date.last.act,flag.imputed)%>%
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
Counterparties$n.entities <- str_count(Counterparties$name, ",") + 1
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
renv::snapshot()