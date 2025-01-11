library(tidyverse)
## load data
dat<-read.csv("R:/Students/Jessy/Osteoporosis Treatments/Datasets/20240917_OsteoporosisStudyPopulation.csv")
## convert variable type
str(dat)
DateCol <- grep("Date",names(dat)) 
dat[DateCol] <- lapply(dat[DateCol], as.Date, "%m/%d/%Y")
dat1<-dat
## variable re-categorization
## Patient characteristics at/before cohort entry date
dat1$Age <- year(dat1$CohortEntryDate)-year(dat1$BirthDate)
dat1$AgeCat <- cut(dat1$Age,
                   breaks = c(39, 49, 59, 69, 79, 89, Inf),
                   labels = c("40-49", "50-59", "60-69", "70-79", "80-89", "90+"),
                   right = TRUE)
dat1$AgeCat[is.na(dat1$Age)] <- NA

dat1$CEY <- year(dat1$CohortEntryDate)
dat1$CEYCat <- cut(dat1$CEY,
                   breaks = c(-Inf, 1999, 2004, 2009, 2014, 2020, Inf),
                   labels = c("NA", "2000-2004", "2005-2009", "2010-2014", "2015-2020", "NA"),
                   right = TRUE)
dat1$CEYCat[is.na(dat1$CEY)] <- NA

dat1$RecordBeforeCE <- as.numeric(difftime(dat1$CohortEntryDate, dat1$StartDate, "days")/365.24)
dat1$RecordBeforeCECat <- cut(dat1$RecordBeforeCE,
                              breaks = c(0, 2, 5, 10, 20, Inf),
                              labels = c("1 to <2",  "2 to <5", "5 to <10", "10 to <20", "20+"),
                              right = FALSE)
dat1$RecordBeforeCECat[is.na(dat1$RecordBeforeCE)] <- NA

dat1$RecordAfterCE <- as.numeric(difftime(dat1$EndDate, dat1$CohortEntryDate, "days")/365.24)
dat1$RecordAfterCECat <- cut(dat1$RecordAfterCE,
                             breaks = c(0, 1, 2, 5, 10, 20, Inf),
                             labels = c("<1", "1 to <2",  "2 to <5", "5 to <10", "10 to <20", "20+"),
                             right = FALSE)
dat1$RecordAfterCECat[is.na(dat1$RecordAfterCE)] <- NA

# 10<BMI<100 are considered outliers
bmi_levels <- c(100, 1, 2, 3, 4)
bmi_labels <- c("Unknown", "<18.5", "18.5 - <25", "25 - <30", "30+")
dat1$BMICat <- factor(dat1$BMICat, levels = bmi_levels, labels = bmi_labels)
smoke_levels <- c(1000, 1, 2, 3)
smoke_labels <- c("Unknown", "Current Smoker", "Former Smoker", "Non-smoker")
dat1$SmokeCat <- factor(dat1$Smoking, levels = smoke_levels, labels = smoke_labels)
dat1$OsteoporosisCatCE <- as.factor(dat1$OsteoporosisCatCE)

# Function for medical history
MHxOnCED <- function(FirstDxDate, CohortEntryDate) {
  FirstDxDate <- as.numeric(FirstDxDate) 
  FirstDxDate <- ifelse(is.na(FirstDxDate), Inf, FirstDxDate) 
  result <- as.numeric(CohortEntryDate >= FirstDxDate)
  result <- as.logical(result)
}

## Treated HTN and DM
dat1$MHxHTN <- MHxOnCED(dat1$FirstTreatHTNDate, dat1$CohortEntryDate) | 
  !is.na(dat1$FirstHTNDate) & dat1$FirstHTNDate< dat1$StartDate & !is.na(dat1$FirstAntiHTNDate) & (dat1$FirstAntiHTNDate - dat1$StartDate) <= 90
dat1$MHxDM <- MHxOnCED(dat1$FirstTreatDMDate, dat1$CohortEntryDate) | 
  !is.na(dat1$FirstDMDate) & dat1$FirstDMDate< dat1$StartDate & !is.na(dat1$FirstAntiDMDate) & (dat1$FirstAntiDMDate - dat1$StartDate) <= 90

## CVD risk factors present on or before cohort entry date
dat1$MHxLipid <- MHxOnCED(dat1$FirstLipidDate, dat1$CohortEntryDate)
dat1$MHxPVD <- MHxOnCED(dat1$FirstPVDrelatDate, dat1$CohortEntryDate)

## Other comorbidities and conditions on or before cohort entry date
dat1$MHxMenopause <- MHxOnCED(dat1$FirstMenopauseDate, dat1$CohortEntryDate)
# Menopause symptoms
dat1$MHxHysterectomy <- MHxOnCED(dat1$FirstHysterectomyDate, dat1$CohortEntryDate)
dat1$MHxCKD <-  MHxOnCED(dat1$FirstCKDDate, dat1$CohortEntryDate)
dat1$MHxAnemia <- MHxOnCED(dat1$FirstAnemiaDate, dat1$CohortEntryDate)
dat1$MHxPaget <- MHxOnCED(dat1$FirstPagetDate, dat1$CohortEntryDate)
dat1$MHxGout <- MHxOnCED(dat1$FirstGoutDate, dat1$CohortEntryDate)
dat1$MHxPolymyalgia <- MHxOnCED(dat1$FirstPolymyalgiaDate, dat1$CohortEntryDate)
dat1$MHxAutoimmune <- MHxOnCED(dat1$FirstAutoimmuneDate, dat1$CohortEntryDate)
dat1$MHxAsthma <- MHxOnCED(dat1$FirstAsthmaDate, dat1$CohortEntryDate)
dat1$MHxCOPD <- MHxOnCED(dat1$FirstCOPDDate, dat1$CohortEntryDate)
dat1$MHxDepression <- MHxOnCED(dat1$FirstDepressionDate, dat1$CohortEntryDate)
dat1$MHxAlcoholAbuse <- MHxOnCED(dat1$FirstAlcoholAbuseDate, dat1$CohortEntryDate)

# Function for medication history 
# required package 
library(lubridate)
RxOnCED1yr <- function(LastRxDate, CohortEntryDate) {
  LastRxDate <- as.numeric(LastRxDate) 
  LastRxDate <- ifelse(is.na(LastRxDate), Inf, LastRxDate)
  OneYearBeforeCED <- CohortEntryDate %m-% years(1)
  result <- as.numeric(CohortEntryDate >= LastRxDate & LastRxDate > OneYearBeforeCED)
  result <- as.logical(result)
}

## Treatments in year before cohort entry date
dat1$RxVitD <- RxOnCED1yr(dat1$LastVitDCEDate, dat1$CohortEntryDate)
dat1$RxCalcium <- RxOnCED1yr(dat1$LastCalciumCEDate, dat1$CohortEntryDate)
dat1$RxSteroid <- RxOnCED1yr(dat1$LastSteroidCEDate, dat1$CohortEntryDate)
dat1$RxHRT <- RxOnCED1yr(dat1$LastHRTCEDate, dat1$CohortEntryDate)
dat1$RxTibolone <- RxOnCED1yr(dat1$LastTiboloneCEDate, dat1$CohortEntryDate)
dat1$RxHRTorTibolone <- ifelse(is.na(dat1$RxHRT) & is.na(dat1$RxTibolone), NA, 
                               ifelse(dat1$RxHRT==T|dat1$RxTibolone==T, T, F))
dat1$RxStatin <- RxOnCED1yr(dat1$LastStatinCEDate, dat1$CohortEntryDate)
dat1$RxAntiDM <- RxOnCED1yr(dat1$LastAntiDMCEDate, dat1$CohortEntryDate)
dat1$RxAntiHTN <- RxOnCED1yr(dat1$LastAntiHTNCEDate, dat1$CohortEntryDate)
dat1$RxAntiCoag <- RxOnCED1yr(dat1$LastAntiCoagCEDate, dat1$CohortEntryDate)

## Final Dataset
CatCol <- grep("Cat",names(dat1)) 
MHxCol <- grep("MHx",names(dat1))
RxCol <- grep("Rx",names(dat1))
df <- dat1 %>% select(CEDrug, Age, RecordBeforeCE, RecordAfterCE, c(CatCol,MHxCol,RxCol))
colnames(df)

# Add labels
library(labelled)
var_label(df$AgeCat) <- "Age at cohort entry date"
var_label(df$CEYCat) <- "Cohort Entry Year"
var_label(df$RecordBeforeCECat) <- "Length of record before cohort entry date (years)"
var_label(df$RecordAfterCECat) <- "Length of record after cohort entry date (years)"
var_label(df$BMICat) <- "Body mass index (kg/m2)"
var_label(df$SmokeCat) <- "Smoking status"
var_label(df$OsteoporosisCatCE) <- "Osteoporotic disease severity"
var_label(df$MHxHTN) <- "Treated HTN"
var_label(df$MHxDM) <- "Treated DM"
var_label(df$MHxLipid) <- "Hyperlipidemia/dyslipidemia"
var_label(df$MHxPVD) <- "PVD related conditions"
var_label(df$MHxMenopause) <- "Menopause recorded by GP"
var_label(df$MHxHysterectomy) <- "Hysterectomy"
var_label(df$MHxCKD ) <- "Chronic kidney disease"
var_label(df$MHxAnemia) <- "Anemia"
var_label(df$MHxPaget) <- "Paget"
var_label(df$MHxGout) <- "Gout"
var_label(df$MHxPolymyalgia) <- "Polymyalgia rheumatica"
var_label(df$MHxAutoimmune) <- "Autoimmune disorders"
var_label(df$MHxAsthma) <- "Asthma"
var_label(df$MHxCOPD) <- "COPD"
var_label(df$MHxDepression) <- "Depression"
var_label(df$MHxAlcoholAbuse) <- "Alcohol Abuse"
var_label(df$RxVitD) <- "Vitamin D (by prescription)"
var_label(df$RxCalcium) <- "Calcium (by prescription)"
var_label(df$RxSteroid) <- "Steroids"
var_label(df$RxHRT) <- "HRT"
var_label(df$RxTibolone) <- "Tibolone"
var_label(df$RxHRTorTibolone) <- "HRT or Tibolone"
var_label(df$RxStatin) <- "Statins"
var_label(df$RxAntiDM) <- "Type 2 Diabetes treatments"
var_label(df$RxAntiHTN) <- "Antihypertensives"
var_label(df$RxAntiCoag) <- "Anticoagulants"

# Summary 
library(gtsummary)
Table_byDrug <- df %>% tbl_summary(by = CEDrug, 
                                   missing = "no", 
                                   digits = everything() ~ 0, 
                                   type = all_continuous() ~ "continuous2", 
                                   statistic = list(all_continuous2() ~ c("{median} ({p25}, {p75})", "{mean} ({sd})"))) 
Table_Total <- df %>% tbl_summary(include = -CEDrug,
                                  missing = "no", 
                                  digits = everything() ~ 0, 
                                  type = all_continuous() ~ "continuous2", 
                                  statistic = list(all_continuous2() ~ c("{median} ({p25}, {p75})", "{mean} ({sd})"))) 
Table1 <- tbl_merge(tbls = list(Table_Total, Table_byDrug),
                    tab_spanner = c("**Total**", "**By Drug**"))

# Export to excel file
library(openxlsx)
write.xlsx(df, "Osteoporosis_BaselineChar_PatientLevel.xlsx")
library(huxtable)
Table1 %>% as_hux_xlsx("Osteoporosis_Table1_20240917.xlsx")
