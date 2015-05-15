library(dplyr)
library(magrittr)

dfTransfer = read.csv(
  textConnection(
    "EMPLID,From_DeptCode,FromDept,To_DeptCode,To_Dept,TransactionTypeCode,TransactionType,EffectiveDate,ChangeType
    0239583290,21,Sales,43,CustomerService,10,Promotion,12/12/2012
    1230495829,21,Sales,21,Sales,10,Promotion,9/1/2013
    4059503918,93,Operations,93,Operations,13,Demotion,11/18/2014
    3040593021,19,Headquarters,23,International,11,Reorg,12/13/2011
    7029406920,15,Marketing,84,Development,19,Reassignment,1/5/2010
    2039052819,19,Headquarters,19,Headquarters,10,Promotion,4/15/2015"), header = TRUE, stringsAsFactors = FALSE)

dfTransfer2 = dfTransfer %>%
  mutate(ChangeType = ifelse(From_DeptCode == To_DeptCode, "No Change", 
                             ifelse(TransactionType == "Reorg", "Reorg", "Transfer"))) %>%
  dplyr::filter(ChangeType != "No Change")
dfTransfer2


Transfers <- read.csv(file="Transfers.csv", head=TRUE, sep=",",colClasses=c(NA,NA,NA,NA,NA,NA,NA,"Date",NA))

Transfers$ChangeType <- ifelse(Transfers$From_DeptCode == Transfers$To_DeptCode, "No Change", ifelse(Transfers$TransactionType == "Reorg", "Reorg", "Transfer")) 

Transfers2 <- subset(Transfers, ChangeType != "No Change")

print(Transfers2)