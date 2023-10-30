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
dependence_function(NDG, "NDG")
dependence_function(Loans, "Loans")

##----------------------------------------------------------------------------##
#-------------------------- d)Loans_Table ---------------------------------#####
Loans_table <- Loans%>%
  mutate(originator=NA,ptf=NA,cluster.ptf=NA,penalties=NA,date.last.act=NA,flag.imputed=NA,id.loan=id.loans,status="Borrower",gbv.residual=NA,desc.type=`secured/unsecured`)%>%
  select(id.loan,id.bor,id.group,originator,ptf,cluster.ptf,type,status,gbv.original,gbv.residual,principal,interest,penalties,expenses,date.origination,date.status,date.last.act,flag.imputed,desc.type)%>%
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
#----------------------------i)Table----------------------------------------####
#SEC/UNSEC
add_gbv_range_column <- function(data) {
  breaks <- c(0, 15000, 30000,50000, 100000, 250000,Inf)
  labels <- c("0-15k", "15-30k", "30-50k", "50-100k", "100-250k","250k+")
  result <- data %>%
    mutate(
      range.gbv = cut(gbv.original, breaks = breaks, labels = labels, right = FALSE)
    )
  return(result)
}
add_vintage_range_column <- function(data, date_col1, date_col2) {
  result <- data %>%
    mutate(
      date_diff = as.numeric(difftime(data[[date_col1]], data[[date_col2]], units = "days")),
      range.vintage = cut(date_diff, 
                          breaks = c(0, 365, 730, 1095, 1825, 3650, Inf), 
                          labels = c("0y", "1y", "2y", "3-5y", "6-10y", "11-20y"),
                          right = FALSE)
    ) %>%
    select(-date_diff)  # Optionally, remove the temporary date_diff column
  return(result)
}

NDG_gbv<-NDG%>%
  add_type_subject_column()%>%
  distinct()
merged_data2 <- merge(NDG_gbv, Loans_table, by.x = "id.bor", by.y = "id.bor", all.x = TRUE)
merged_data2 <- add_gbv_range_column(merged_data2)
merged_data2 <- add_vintage_range_column(merged_data2, "date.origination", "date.status")
sum.borr <-count(Loans_table)
sum.gbv <-sum(Loans_table$gbv.original)
sum.principal <-sum(Loans_table$principal)
gbv_help <- merged_data2 %>%
  select(desc.type, type.subject, gbv.original, principal) %>%
  group_by(desc.type, type.subject) %>%
  summarize(
    numb.borr = n(),perc.borr = sum(numb.borr) / sum(sum.borr),gbv.original = sum(gbv.original),
    mean.gbv = mean(gbv.original),perc.gbv = sum(gbv.original) / sum(sum.gbv),
    capital = sum(principal),mean.capital = mean(principal),
    perc.capital = sum(principal) / sum(sum.principal)
  )

total_row <- data.frame(
  desc.type = "Total",type.subject = "",
  gbv.original = sum(gbv_help$gbv.original),numb.borr = sum(gbv_help$numb.borr), perc.borr = sum(gbv_help$perc.borr),
  mean.gbv = sum(gbv_help$mean.gbv * gbv_help$numb.borr) / sum(gbv_help$numb.borr),
  perc.gbv = sum(gbv_help$perc.gbv),capital=sum(gbv_help$capital),
  mean.capital = sum(gbv_help$mean.capital * gbv_help$numb.borr) / sum(gbv_help$numb.borr),
  perc.capital = sum(gbv_help$perc.capital))

updated_df <- bind_rows(gbv_help, total_row)
updated_df<-updated_df%>%
  rename("Sec/Unsec per id.borr"=desc.type,"Type of Company"=type.subject,"N Bor"=numb.borr,
         "% Bor"=perc.borr,"GBV(k)"=gbv.original,"Mean GBV(k)"=mean.gbv,"% GBV"=perc.gbv,
         "Capital(k)"=capital,"Mean Capital(k)"=mean.capital,"% Capital"=perc.capital)
#updated_df$perc.gbv <- percent(updated_df$perc.gbv, scale=100)
#updated_df$mean.gbv <- round(updated_df$mean.gbv, digits=1)

#SEC/UNSEC + RANGE GBV
range <- merged_data2 %>%
  select(desc.type, range.gbv,gbv.original, principal) %>%
  group_by(desc.type,range.gbv) %>%
  summarize(
    numb.borr = n(),perc.borr = sum(numb.borr) / sum(sum.borr),gbv.original = sum(gbv.original),
    mean.gbv = mean(gbv.original),perc.gbv = sum(gbv.original) / sum(sum.gbv),
    capital = sum(principal),mean.capital = mean(principal),
    perc.capital = sum(principal) / sum(sum.principal)
  )

total_row_range <- data.frame(
  desc.type = "Total",range.gbv = "",
  gbv.original = sum(range$gbv.original),numb.borr = sum(range$numb.borr), perc.borr = sum(range$perc.borr),
  mean.gbv = sum(range$mean.gbv * range$numb.borr) / sum(range$numb.borr),
  perc.gbv = sum(range$perc.gbv),capital=sum(range$capital),
  mean.capital = sum(range$mean.capital * range$numb.borr) / sum(range$numb.borr),
  perc.capital = sum(range$perc.capital))

updated_range <- bind_rows(range, total_row_range)
updated_range<-updated_range%>%
  rename("Sec/Unsec per id.borr"=desc.type,"Range GBV"=range.gbv,"N Bor"=numb.borr,
         "% Bor"=perc.borr,"GBV(k)"=gbv.original,"Mean GBV(k)"=mean.gbv,"% GBV"=perc.gbv,
         "Capital(k)"=capital,"Mean Capital(k)"=mean.capital,"% Capital"=perc.capital)%>%
  arrange(factor(`Sec/Unsec per id.borr`, levels = c("secured", "unsecured", "Total")),
    factor(`Range GBV`,levels = c("0-15k", "15-30k", "30-50k","50-100k","100-250k","250k+")))

#SEC/UNSEC + RANGE Vintage
vintage <- merged_data2 %>%
  select(desc.type, range.vintage,gbv.original, principal) %>%
  group_by(desc.type,range.vintage) %>%
  summarize(
    numb.borr = n(),perc.borr = sum(numb.borr) / sum(sum.borr),gbv.original = sum(gbv.original),
    mean.gbv = mean(gbv.original),perc.gbv = sum(gbv.original) / sum(sum.gbv),
    capital = sum(principal),mean.capital = mean(principal),
    perc.capital = sum(principal) / sum(sum.principal)
  )

total_row_vintage <- data.frame(
  desc.type = "Total",range.vintage = "",
  gbv.original = sum(vintage$gbv.original),numb.borr = sum(vintage$numb.borr), perc.borr = sum(vintage$perc.borr),
  mean.gbv = sum(vintage$mean.gbv * vintage$numb.borr) / sum(vintage$numb.borr),
  perc.gbv = sum(vintage$perc.gbv),capital=sum(vintage$capital),
  mean.capital = sum(vintage$mean.capital * vintage$numb.borr) / sum(vintage$numb.borr),
  perc.capital = sum(vintage$perc.capital))

updated_vintage <- bind_rows(vintage, total_row_vintage)
updated_vintage<-updated_vintage%>%
  rename("Sec/Unsec per id.borr"=desc.type,"Range Vintage"=range.vintage,"N Bor"=numb.borr,
         "% Bor"=perc.borr,"GBV(k)"=gbv.original,"Mean GBV(k)"=mean.gbv,"% GBV"=perc.gbv,
         "Capital(k)"=capital,"Mean Capital(k)"=mean.capital,"% Capital"=perc.capital)%>%
  arrange(factor(`Sec/Unsec per id.borr`, levels = c("secured", "unsecured", "Total")),
          factor(`Range Vintage`,levels = c("0y", "1y", "2y", "3-5y", "6-10y","11-20y")))


# Type Loans
type_loans <- merged_data2 %>%
  select(type, gbv.original, principal) %>%
  group_by(type) %>%
  summarize(
    numb.borr = n(),perc.borr = sum(numb.borr) / sum(sum.borr),gbv.original = sum(gbv.original),
    mean.gbv = mean(gbv.original),perc.gbv = sum(gbv.original) / sum(sum.gbv),
    capital = sum(principal),mean.capital = mean(principal),
    perc.capital = sum(principal) / sum(sum.principal)
  )

total_row_loans <- data.frame(
  type = "Total",gbv.original = sum(type_loans$gbv.original),numb.borr = sum(type_loans$numb.borr), perc.borr = sum(type_loans$perc.borr),
  mean.gbv = sum(type_loans$mean.gbv * type_loans$numb.borr) / sum(type_loans$numb.borr),
  perc.gbv = sum(type_loans$perc.gbv),capital=sum(type_loans$capital),
  mean.capital = sum(type_loans$mean.capital * type_loans$numb.borr) / sum(type_loans$numb.borr),
  perc.capital = sum(type_loans$perc.capital))

updated_loans <- bind_rows(type_loans, total_row_loans)
updated_loans<-updated_loans%>%
  rename("Type of Credit"=type,"N Bor"=numb.borr,
         "% Bor"=perc.borr,"GBV(k)"=gbv.original,"Mean GBV(k)"=mean.gbv,"% GBV"=perc.gbv,
         "Capital(k)"=capital,"Mean Capital(k)"=mean.capital,"% Capital"=perc.capital)
#-----------------------------------------------------------------------------
renv::snapshot()