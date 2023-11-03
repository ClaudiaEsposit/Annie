#---------------------------- a)Intro --------------------------------------####
cat('\014')
rm(list=ls())
setwd("~/Annie")
source("Library.R")
source("Functions.R")
source("Vanilla.R")
Loans_table <- Loans%>%
  mutate(originator=NA,ptf=NA,cluster.ptf=NA,penalties=NA,date.last.act=NA,
         flag.imputed=NA,id.loan=id.loans,status="Borrower",gbv.residual=NA,
         desc.type=`secured/unsecured`,desc.guarantors=role)%>%
  select(id.loan,id.bor,id.group,originator,ptf,cluster.ptf,type,status,
         gbv.original,gbv.residual,principal,interest,penalties,expenses,
         date.origination,date.status,date.last.act,flag.imputed,desc.type,desc.guarantors)%>%
  distinct()

#SEC/UNSEC
NDG_gbv<-NDG%>%
  add_type_subject_column()%>%
  distinct()
merged_data2 <- merge(NDG_gbv, Loans_table, by.x = "id.bor", by.y = "id.bor", all.x = TRUE)
sum.borr <- count(NDG_gbv)
numb.loans <- count(Loans_table)
sum.gbv <-sum(Loans_table$gbv.original)
sum.principal_tot <-sum(Loans_table$principal)
merged_data2 <- add_vintage_range_column(merged_data2, "date.origination", "date.status")
merged_data2 <- merged_data2 %>%
  group_by(id.bor, name) %>%
  mutate(has_secured = ifelse(any(desc.type == "secured"),"secured","unsecured"))%>%
  select(-desc.type)%>%
  rename(desc.type=has_secured)
filtered_data <- merged_data2 %>%
  group_by(id.bor, name, desc.type) %>%
  mutate(gbv = sum(gbv.original),principal_tot=sum(principal),
         n.entities=NA)%>%
  select(-gbv.original,-principal)%>%
  distinct(id.bor,.keep_all=TRUE)
filtered_data <-add_gbv_range_column(filtered_data)
filtered_data$n.entities <- str_count(filtered_data$name, "[,-]") + 1
gbv_help <- filtered_data %>%
  group_by(desc.type, type.subject) %>%
  summarize(
    numb.borr = n_distinct(id.bor),perc.borr = sum(numb.borr) / sum(sum.borr),gbv_tot = sum(gbv),
    mean.gbv = gbv_tot/numb.borr,perc.gbv = sum(gbv_tot) / sum(sum.gbv),
    capital = sum(principal_tot),mean.capital = capital/numb.borr,
    perc.capital = sum(principal_tot) / sum(sum.principal_tot)
  )

total_row <- data.frame(
  desc.type = "Total",type.subject = "",
  gbv_tot = sum(gbv_help$gbv_tot),numb.borr = sum(gbv_help$numb.borr), perc.borr = sum(gbv_help$perc.borr),
  mean.gbv = sum(gbv_help$mean.gbv * gbv_help$numb.borr) / sum(gbv_help$numb.borr),
  perc.gbv = sum(gbv_help$perc.gbv),capital=sum(gbv_help$capital),
  mean.capital = sum(gbv_help$mean.capital * gbv_help$numb.borr) / sum(gbv_help$numb.borr),
  perc.capital = sum(gbv_help$perc.capital))

total_secured <- gbv_help %>%
  group_by(desc.type) %>%
  summarize(    desc.type = if ("secured" %in% desc.type) "Secured Tot." else "Unsecured Tot.",
                type.subject = "",gbv_tot = sum(gbv_tot),
                numb.borr = sum(numb.borr),perc.borr = sum(perc.borr),
                mean.gbv = gbv_tot / numb.borr,
                perc.gbv = sum(perc.gbv),capital = sum(capital),
                mean.capital = capital / numb.borr,
                perc.capital = sum(perc.capital))
total_secured <- as.data.frame(total_secured)

updated_df <- bind_rows(gbv_help, total_row,total_secured)
updated_df<-updated_df%>%
  rename("Sec/Unsec per id.borr"=desc.type,"Type of Company"=type.subject,"N Bor"=numb.borr,
         "% Bor"=perc.borr,"GBV(€k)"=gbv_tot,"Mean GBV(€k)"=mean.gbv,"% GBV"=perc.gbv,
         "Capital(€k)"=capital,"Mean Capital(€k)"=mean.capital,"% Capital"=perc.capital)%>%
  arrange(factor(`Sec/Unsec per id.borr`, levels = c("Total","Secured Tot.", "secured","Unsecured Tot.","unsecured")),
          factor(`Type of Company`,levels = c("corporate","individual")))
updated_df$`Sec/Unsec per id.borr` <- str_to_title(updated_df$`Sec/Unsec per id.borr`)
updated_df$`Type of Company` <- str_to_title(updated_df$`Type of Company`)


#SEC/UNSEC + RANGE GBV
range <- filtered_data %>%
  group_by(desc.type,range.gbv) %>%
  summarize(
    numb.borr = n_distinct(id.bor),perc.borr = sum(numb.borr) / sum(sum.borr),gbv_tot = sum(gbv),
    mean.gbv = gbv_tot/numb.borr,perc.gbv = sum(gbv_tot) / sum(sum.gbv),
    capital = sum(principal_tot),mean.capital = mean(principal_tot),
    perc.capital = sum(principal_tot) / sum(sum.principal_tot)
  )

total_row_range <- data.frame(
  desc.type = "Total",range.gbv = "",
  gbv_tot = sum(range$gbv_tot),numb.borr = sum(range$numb.borr), perc.borr = sum(range$perc.borr),
  mean.gbv = sum(range$mean.gbv * range$numb.borr) / sum(range$numb.borr),
  perc.gbv = sum(range$perc.gbv),capital=sum(range$capital),
  mean.capital = sum(range$mean.capital * range$numb.borr) / sum(range$numb.borr),
  perc.capital = sum(range$perc.capital))

total_secured2 <- range %>%
  group_by(desc.type) %>%
  summarize(desc.type = if ("secured" %in% desc.type) "Secured Tot." else "Unsecured Tot.",
            range.gbv = "",gbv_tot = sum(gbv_tot),
            numb.borr = sum(numb.borr),perc.borr = sum(perc.borr),
            mean.gbv = gbv_tot / numb.borr,
            perc.gbv = sum(perc.gbv),capital = sum(capital),
            mean.capital = capital/numb.borr,
            perc.capital = sum(perc.capital))
total_secured2 <- as.data.frame(total_secured2)

updated_range <- bind_rows(range, total_row_range,total_secured2)
updated_range<-updated_range%>%
  rename("Sec/Unsec per id.borr"=desc.type,"Range GBV"=range.gbv,"N Bor"=numb.borr,
         "% Bor"=perc.borr,"GBV(€k)"=gbv_tot,"Mean GBV(€k)"=mean.gbv,"% GBV"=perc.gbv,
         "Capital(€k)"=capital,"Mean Capital(€k)"=mean.capital,"% Capital"=perc.capital)%>%
  arrange(factor(`Sec/Unsec per id.borr`, levels = c("Total","Secured Tot.", "secured","Unsecured Tot.","unsecured")),
          factor(`Range GBV`,levels = c("0-15k", "15-30k", "30-50k","50-100k","100-250k","250k+")))
updated_range$`Sec/Unsec per id.borr` <- str_to_title(updated_range$`Sec/Unsec per id.borr`)
updated_range$`Range GBV` <- str_to_title(updated_range$`Range GBV`)
new_row_r1 <- data.frame("Sec/Unsec per id.borr" = "Unsecured","Range GBV" = "50-100k","N Bor" = 0,
  "% Bor" = 0,"GBV(€k)" = 0,"Mean GBV(€k)" = 0,"% GBV" = 0,"Capital(€k)" = 0,"Mean Capital(€k)" = 0,
  "% Capital" = 0)
colnames(new_row_r1) <- colnames(updated_range)
insert_position_r1 <- 10
before_r1 <- updated_range[1:(insert_position_r1 - 1), ]
after_r1 <- updated_range[insert_position_r1:nrow(updated_range), ]
updated_range <- rbind(before_r1, new_row_r1, after_r1)
new_row_r2 <- data.frame("Sec/Unsec per id.borr" = "Unsecured","Range GBV" = "300-500k","N Bor" = 0,
                         "% Bor" = 0,"GBV(€k)" = 0,"Mean GBV(€k)" = 0,"% GBV" = 0,"Capital(€k)" = 0,"Mean Capital(€k)" = 0,
                         "% Capital" = 0)
colnames(new_row_r2) <- colnames(updated_range)
insert_position_r2 <- 12
before_r2 <- updated_range[1:(insert_position_r2 - 1), ]
updated_range <- rbind(before_r2, new_row_r2)
new_row_r3 <- data.frame("Sec/Unsec per id.borr" = "Unsecured","Range GBV" = "500k+","N Bor" = 0,
                         "% Bor" = 0,"GBV(€k)" = 0,"Mean GBV(€k)" = 0,"% GBV" = 0,"Capital(€k)" = 0,"Mean Capital(€k)" = 0,
                         "% Capital" = 0)
colnames(new_row_r3) <- colnames(updated_range)
insert_position_r3 <- 13
before_r3 <- updated_range[1:(insert_position_r3 - 1), ]
updated_range <- rbind(before_r3, new_row_r3)


#SEC/UNSEC + RANGE Vintage
vintage <- filtered_data %>%
  group_by(desc.type,range.vintage) %>%
  summarize(
    numb.borr=n_distinct(id.bor),perc.borr = sum(numb.borr) / sum(sum.borr),gbv_tot = sum(gbv),
    mean.gbv = gbv_tot/numb.borr,perc.gbv = sum(gbv_tot) / sum(sum.gbv),
    capital = sum(principal_tot),mean.capital = mean(principal_tot),
    perc.capital = sum(principal_tot) / sum(sum.principal_tot)
  )

total_row_vintage <- data.frame(
  desc.type = "Total",range.vintage = "",
  gbv_tot = sum(vintage$gbv_tot),numb.borr = sum(vintage$numb.borr), perc.borr = sum(vintage$perc.borr),
  mean.gbv = sum(vintage$mean.gbv * vintage$numb.borr) / sum(vintage$numb.borr),
  perc.gbv = sum(vintage$perc.gbv),capital=sum(vintage$capital),
  mean.capital = sum(vintage$mean.capital * vintage$numb.borr) / sum(vintage$numb.borr),
  perc.capital = sum(vintage$perc.capital))

total_secured3 <- vintage %>%
  group_by(desc.type) %>%
  summarize(desc.type = if ("secured" %in% desc.type) "Secured Tot." else "Unsecured Tot.",
            range.vintage = "",gbv_tot = sum(gbv_tot),
            numb.borr = sum(numb.borr),perc.borr = sum(perc.borr),
            mean.gbv =gbv_tot / numb.borr,
            perc.gbv = sum(perc.gbv),capital = sum(capital),
            mean.capital = capital/numb.borr,
            perc.capital = sum(perc.capital))
total_secured3 <- as.data.frame(total_secured3)
updated_vintage <- bind_rows(vintage, total_row_vintage,total_secured3)
updated_vintage$range.vintage[is.na(updated_vintage$range.vintage)] <- "utp"

updated_vintage<-updated_vintage%>%
  rename("Sec/Unsec per id.borr"=desc.type,"Range Vintage"=range.vintage,"N Bor"=numb.borr,
         "% Bor"=perc.borr,"GBV(€k)"=gbv_tot,"Mean GBV(€k)"=mean.gbv,"% GBV"=perc.gbv,
         "Capital(€k)"=capital,"Mean Capital(€k)"=mean.capital,"% Capital"=perc.capital)

custom_levels <- c("Total", "Secured Tot.", "secured", "Unsecured Tot.", "unsecured", "Unsecured")
updated_vintage$`Sec/Unsec per id.borr` <- factor(updated_vintage$`Sec/Unsec per id.borr`, levels = custom_levels)
updated_vintage <- updated_vintage %>%
  arrange(`Sec/Unsec per id.borr`, `Range Vintage`)
updated_vintage$`Sec/Unsec per id.borr` <- str_to_title(updated_vintage$`Sec/Unsec per id.borr`)
updated_vintage$`Range Vintage` <- str_to_title(updated_vintage$`Range Vintage`)
new_row <- data.frame(
  "Sec/Unsec per id.borr" = "Unsecured",
  "Range Vintage" = "6-10y",
  "N Bor" = 0,
  "% Bor" = 0,
  "GBV(€k)" = 0,
  "Mean GBV(€k)" = 0,
  "% GBV" = 0,
  "Capital(€k)" = 0,
  "Mean Capital(€k)" = 0,
  "% Capital" = 0
)
colnames(new_row) <- colnames(updated_vintage)
insert_position <- 8
before <- updated_vintage[1:(insert_position - 1), ]
after <- updated_vintage[insert_position:nrow(updated_vintage), ]
updated_vintage <- rbind(before, new_row, after)


#loans
type_loans <- filtered_data %>%
  group_by(type) %>%
  summarize(
    numb.borr = n_distinct(id.bor),perc.borr = sum(numb.borr) / sum(sum.borr),gbv_tot = sum(gbv),
    mean.gbv = gbv_tot/numb.borr,perc.gbv = sum(gbv_tot) / sum(sum.gbv),
    capital = sum(principal_tot),mean.capital = mean(principal_tot),
    perc.capital = sum(principal_tot) / sum(sum.principal_tot)
  )

total_row_loans <- data.frame(
  type = "Total",gbv_tot = sum(type_loans$gbv_tot),numb.borr = sum(type_loans$numb.borr), perc.borr = sum(type_loans$perc.borr),
  mean.gbv = sum(type_loans$mean.gbv * type_loans$numb.borr) / sum(type_loans$numb.borr),
  perc.gbv = sum(type_loans$perc.gbv),capital=sum(type_loans$capital),
  mean.capital = sum(type_loans$mean.capital * type_loans$numb.borr) / sum(type_loans$numb.borr),
  perc.capital = sum(type_loans$perc.capital))

updated_loans <- bind_rows(type_loans, total_row_loans)
updated_loans<-updated_loans%>%
  rename("Type of Credit"=type,"N Bor"=numb.borr,
         "% Bor"=perc.borr,"GBV(€k)"=gbv_tot,"Mean GBV(€k)"=mean.gbv,"% GBV"=perc.gbv,
         "Capital(€k)"=capital,"Mean Capital(€k)"=mean.capital,"% Capital"=perc.capital)%>%
  arrange(factor(`Type of Credit`, levels = c("Total","Bank Accounts", "Mortgages","Other")))

#guarantors
guarantors <- filtered_data %>%
  group_by(desc.type,desc.guarantors) %>%
  summarize(
    numb.borr = n_distinct(id.bor),perc.borr = sum(numb.borr) / sum(sum.borr),gbv_tot = sum(gbv),
    mean.gbv = gbv_tot/numb.borr,perc.gbv = sum(gbv_tot) / sum(sum.gbv)
  )

total_row_guarantors <- data.frame(
  desc.type = "Total",desc.guarantors = "",
  gbv_tot = sum(guarantors$gbv_tot),numb.borr = sum(guarantors$numb.borr), perc.borr = sum(guarantors$perc.borr),
  mean.gbv = sum(guarantors$mean.gbv * guarantors$numb.borr) / sum(guarantors$numb.borr),
  perc.gbv = sum(guarantors$perc.gbv))
total_guarantors <- guarantors %>%
  group_by(desc.type) %>%
  summarize(    desc.type = if ("secured" %in% desc.type) "Secured Tot." else "Unsecured Tot.",
                desc.guarantors = "",gbv_tot = sum(gbv_tot),
                numb.borr = sum(numb.borr),perc.borr = sum(perc.borr),
                mean.gbv = gbv_tot / numb.borr,
                perc.gbv = sum(perc.gbv))
total_guarantors <- as.data.frame(total_guarantors)

updated_guarantors <- bind_rows(guarantors, total_row_guarantors,total_guarantors)
updated_guarantors<-updated_guarantors%>%
  rename("Sec/Unsec per id.borr"=desc.type,"Guarantors"=desc.guarantors,"N Bor"=numb.borr,
         "% Bor"=perc.borr,"GBV(€k)"=gbv_tot,"Mean GBV(€k)"=mean.gbv,"% GBV"=perc.gbv)%>%
  arrange(factor(`Sec/Unsec per id.borr`, levels = c("Total","Secured Tot.", "secured","Unsecured Tot.","unsecured")),
          factor(`Guarantors`,levels = c("Yes","No")))
updated_guarantors$`Sec/Unsec per id.borr` <- str_to_title(updated_guarantors$`Sec/Unsec per id.borr`)
updated_guarantors$Guarantors <- str_to_title(updated_guarantors$Guarantors)


# total
col_tot <- c("N Borrowers" ,"N Loans","GBV(€k)",  "Avg GBV per Borrower(€k)","Avg GBV per Loans(€k)")
total_table <- data.frame(
  "N Borrowers" = sum.borr,
  "N Loans" = numb.loans,
  "GBV(€k)" = sum.gbv,
  "Avg GBV per Borrower(€k)" = sum.gbv / sum.borr,
  "Avg GBV per Loans(€k)" = sum.gbv / numb.loans
)
colnames(total_table) <- col_tot


#utp/bad
utp_bad <- filtered_data %>%
  group_by(category) %>%
  summarize(
    numb.borr = n_distinct(id.bor),perc.borr = sum(numb.borr) / sum(sum.borr),gbv_tot = sum(gbv),
    mean.gbv = gbv_tot/numb.borr,perc.gbv = sum(gbv_tot) / sum(sum.gbv)
  )

total_row_utp <- data.frame(
  category = "Total",gbv_tot = sum(type_loans$gbv_tot),numb.borr = sum(type_loans$numb.borr), perc.borr = sum(type_loans$perc.borr),
  mean.gbv = sum(type_loans$mean.gbv * type_loans$numb.borr) / sum(type_loans$numb.borr),
  perc.gbv = sum(type_loans$perc.gbv))

updated_utp <- bind_rows(utp_bad, total_row_utp)
updated_utp<-updated_utp%>%
  rename("Status"=category,"N Bor"=numb.borr,
         "% Bor"=perc.borr,"GBV(€k)"=gbv_tot,"Mean GBV(€k)"=mean.gbv,"% GBV"=perc.gbv)%>%
  arrange(factor(Status, levels = c("Total","soff.", "utp")))
updated_utp$Status <- str_to_title(updated_utp$Status)


#entities
b_entities <- filtered_data %>%
  group_by(n.entities) %>%
  summarize(
    numb.borr = n_distinct(id.bor),perc.borr = sum(numb.borr) / sum(sum.borr),gbv_tot = sum(gbv),
    mean.gbv = gbv_tot/numb.borr,perc.gbv = sum(gbv_tot) / sum(sum.gbv)
  )
total_row_ent <- data.frame(
  n.entities = NA,numb.borr = sum(b_entities$numb.borr), perc.borr = sum(b_entities$perc.borr),gbv_tot = sum(b_entities$gbv_tot),
  mean.gbv = sum(b_entities$mean.gbv * b_entities$numb.borr) / sum(b_entities$numb.borr),
  perc.gbv = sum(b_entities$perc.gbv))
updated_ent <- bind_rows(b_entities, total_row_ent)
updated_ent<-updated_ent%>%
  rename("N Entities"=n.entities,"N Bor"=numb.borr,
         "% Bor"=perc.borr,"GBV(€k)"=gbv_tot,"Mean GBV(€k)"=mean.gbv,"% GBV"=perc.gbv)
updated_ent$`N Entities`<- as.character(updated_ent$`N Entities`)
updated_ent$`N Entities`[is.na(updated_ent$`N Entities`)] <- "Total"
updated_ent<-updated_ent%>%
  arrange(factor(`N Entities`, levels = c("Total","4", "2","1")))

# province
province_dif <- filtered_data %>%
  group_by(province) %>%
  summarize(
    numb.borr = n_distinct(id.bor),perc.borr = sum(numb.borr) / sum(sum.borr),gbv_tot = sum(gbv),
    mean.gbv = gbv_tot/numb.borr,perc.gbv = sum(gbv_tot) / sum(sum.gbv)
  )
total_row_pro <- data.frame(
  province = "Total",numb.borr = sum(province_dif$numb.borr), perc.borr = sum(province_dif$perc.borr),gbv_tot = sum(province_dif$gbv_tot),
  mean.gbv = sum(province_dif$mean.gbv * province_dif$numb.borr) / sum(province_dif$numb.borr),
  perc.gbv = sum(province_dif$perc.gbv))
updated_pro <- bind_rows(province_dif, total_row_pro)
updated_pro<-updated_pro%>%
  rename("Province"=province,"N Bor"=numb.borr,
         "% Bor"=perc.borr,"GBV(€k)"=gbv_tot,"Mean GBV(€k)"=mean.gbv,"% GBV"=perc.gbv)%>%
  arrange(factor(`Province`, levels = c("Total","venezia", "padova","rovigo","vicenza")))
updated_pro$Province <- str_to_title(updated_pro$Province)


# type based on loans
type_loans2 <- merged_data2 %>%
  group_by(type) %>%
  summarize(
    numb.loan = n_distinct(id.loan),perc.loan = sum(numb.loan) / sum(numb.loans),gbv_tot = sum(gbv.original),
    mean.gbv = gbv_tot/numb.loan,perc.gbv = sum(gbv_tot) / sum(sum.gbv)
  )
total_row_typ <- data.frame(
  type = "Total",numb.loan = sum(type_loans2$numb.loan), perc.loan = sum(type_loans2$perc.loan),gbv_tot = sum(type_loans2$gbv_tot),
  mean.gbv = sum(type_loans2$mean.gbv * type_loans2$numb.loan) / sum(type_loans2$numb.loan),
  perc.gbv = sum(type_loans2$perc.gbv))
updated_type <- bind_rows(type_loans2, total_row_typ)
updated_type<-updated_type%>%
  rename("Type of Loans"=type,"N Loans"=numb.loan,
         "% Loans"=perc.loan,"GBV(€k)"=gbv_tot,"Mean GBV(€k)"=mean.gbv,"% GBV"=perc.gbv)%>%
  arrange(factor(`Type of Loans`, levels = c("Total","Bank Accounts", "Mortgages","Other")))
updated_type$`Type of Loans` <- str_to_title(updated_type$`Type of Loans`)
