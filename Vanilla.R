##----------------------------------------------------------------------------##
#---------------------------- a)INTRO -----------------------------------------#
##----------------------------------------------------------------------------##
cat('\014')
rm(list=ls())
setwd("~/Custom Office Templates/Annia")
source("Library.R")
source("Functions.R")


##----------------------------------------------------------------------------##
#---------------------------- b)UPLOAD ----------------------------------------#
##----------------------------------------------------------------------------##
NDG <- read_excel("DATATAPE INVESTITORE  BCC ANNIA CUTOFF 25102022.xlsx")
Loans <- read_excel("DATATAPE INVESTITORE  BCC ANNIA CUTOFF 25102022.xlsx", sheet = 'LOANS')
Geo <- read_excel("Geo.xlsx")


##----------------------------------------------------------------------------##
#---------------------------- c)LOANS -----------------------------------------#
##----------------------------------------------------------------------------##
# Change the values of Default Date and format all
base_date <- as.Date("1899-12-30")
Loans<- Loans%>%
  `colnames<-`(tolower(colnames(.)))%>%
  rename_all(~ gsub(" ", "", .))%>%
  mutate(datedefault = ifelse( defaultdate== "UTP", NA, as.numeric(defaultdate)),
         utp = ifelse(defaultdate == "UTP",defaultdate, NA),
         datedefault = base_date + datedefault - 1)%>%
  rename(id.loans=idloans,id.bor=ndg,name=borrowername,id.group=group,type=typeofcredit,gbv.original=totalgbv,principal=gbvcapital,
         interest=gbvinterest,expenses=gbvexpenses,date.origination=databasedate,date.status=datedefault,role=guarantors)%>%
  select(-defaultdate)%>%
  mutate_all(~ gsub(" {2,}", " ", trimws(.x)))%>%
  mutate_all(tolower)%>%
  mutate(id.bor=as.character(id.bor,label=TRUE),
         type=as.factor(type),id.loans=as.character(id.loans,label=TRUE),
         utp=as.factor(utp),
         gbv.original=as.numeric(gbv.original,label=TRUE),
         interest=as.numeric(interest,label=TRUE),
         expenses=as.numeric(expenses,label=TRUE),
         principal=as.numeric(principal,label=TRUE),
         date.origination=as.Date(date.origination,label=TRUE),
         date.status=as.Date(date.status,label=TRUE),
         role=as.factor(role))
Loans<-change_type_credit(Loans)

##----------------------------------------------------------------------------##
#---------------------------- d)NDGs ------------------------------------------#
##----------------------------------------------------------------------------##

NDG <- NDG %>%
  `colnames<-`(tolower(colnames(.)))%>%
  rename_all(~ gsub(" ", "", .))%>%
  rename(id.bor=ndg,id.group=group,name=borrowername,cf.piva=taxid,region=`borrower'sregion`,city=town,province=city)%>%
  mutate_all(~ gsub(" {2,}", " ", trimws(.x)))%>%
  mutate_all(tolower)%>%
  mutate(id.bor=as.character(id.bor,label=TRUE),id.group=as.character(id.group,label=TRUE),
         name=as.character(name,label=TRUE),cf.piva=as.character(cf.piva,label=TRUE),
         city=as.character(city,label=TRUE),region=as.factor(region),province=as.factor(province))


##----------------------------------------------------------------------------##
#---------------------------- e)Geo -------------------------------------------#
##----------------------------------------------------------------------------##
Geo <- Geo %>%
  mutate_all(~ gsub(" {2,}", " ", trimws(.x)))%>%
  mutate_all(tolower)%>%
  rename_all(~ gsub(" ", "", .))
