library(tidyverse)
## load data for case
dat <- read.csv("R:/Students/Jessy/Osteoporosis Treatments/Datasets/20241001_OsteoporosisStudyPopulation.csv")
dat<- dat %>% select(PID, FirstAutoimmuneDate)
case1<-read.csv("R:/Students/Jessy/Osteoporosis Treatments/Datasets/Aurum_Case_Exposure_CVD.csv")
case1<-case1 %>% select(PID, BirthDate, CohortEntryDate, CensorDate, ExpCensor7, VitDOnly, 
                        FirstOsteoporosisFinalDate, FirstOsteopeniaFinalDate, FirstCKDDate, 
                        LastSteroidCensorDate, LastCalciumCensorDate, 
                        LastHRTCensorDate, LastTiboloneCensorDate, SmokingCensor)
case1 <- merge(case1, dat, by.x=c("PID"), all.x = TRUE)

# correct Exp for VitD group
case1$ExpCensor7 <- ifelse(case1$VitDOnly==1, "U", case1$ExpCensor7)
## convert variable type
DateCol1 <- grep("Date",names(case1)) 
case1[DateCol1] <- lapply(case1[DateCol1], as.Date, "%m/%d/%Y")

## variable re-categorization
case1$Age <- year(case1$CensorDate)-year(case1$BirthDate)
case1$Age60 <- ifelse(case1$Age<60, "<60", ">=60")
case1$Age65 <- ifelse(case1$Age<65, "<65", ">=65")
case1$Age70 <- ifelse(case1$Age<70, "<70", ">=70")
case1$AgeGrp <- cut(case1$Age,
                   breaks = c(39, 44, 49, 54, 59, 64, 69, 74, 79, 84, Inf),
                   labels = c("40", "45", "50", "55", "60", "65", "70", "75", "80", "85"),
                   right = TRUE)
case1$AgeGrp[is.na(case1$Age)] <- NA

# index year
case1$CensorY <- year(case1$CensorDate)
case1$Year <- cut(case1$CensorY,
                        breaks = c(1999, 2004, 2009, 2014, 2020),
                        labels = c("2000-2004", "2005-2009", "2010-2014", "2015-2020"),
                        right = TRUE)
case1$Year2 <- ifelse(case1$Year=="2000-2004"|case1$Year=="2005-2009", "2000-2009", "2010-2020")
# Osteoporosis severity
case1$Severity <- 3
case1$Severity <- ifelse(!is.na(case1$FirstOsteopeniaFinalDate) &
                           case1$CensorDate >= case1$FirstOsteopeniaFinalDate, 2, case1$Severity)
case1$Severity <- ifelse(!is.na(case1$FirstOsteoporosisFinalDate) &
                           case1$CensorDate >= case1$FirstOsteoporosisFinalDate, 1, case1$Severity)
# Smoking status at censor
case1$Smoking <- as.factor(case1$SmokingCensor)
# Function for MHx at index
MHxOnCensor <- function(FirstDxDate, CensorDate) {
  FirstDxDate <- as.numeric(FirstDxDate) 
  FirstDxDate <- ifelse(is.na(FirstDxDate), Inf, FirstDxDate) 
  result <- as.numeric(CensorDate >= FirstDxDate)
  result <- as.logical(result)
}
case1$CKD <- MHxOnCensor(case1$FirstCKDDate, case1$CensorDate)
case1$AutoImmune <- MHxOnCensor(case1$FirstAutoimmuneDate, case1$CensorDate)
                                
# Function for CM within x days
CMbeforeCensor <- function(LastCmDate, CensorDate, days) {
  LastCmDate <- as.numeric(LastCmDate) 
  LastCmDate <- ifelse(is.na(LastCmDate), Inf, LastCmDate) 
  XDayCensor <- as.numeric(CensorDate)-days
  result <- as.numeric(XDayCensor <= LastCmDate & LastCmDate < CensorDate)
  result <- as.logical(result)
}
case1$StrdExp <- CMbeforeCensor(case1$LastSteroidCensorDate, case1$CensorDate, 30)
case1$Calcium <- CMbeforeCensor(case1$LastCalciumCensorDate, case1$CensorDate, 30)
case1$HRT <- CMbeforeCensor(case1$LastHRTCensorDate, case1$CensorDate, 90)
case1$Tibolone <- CMbeforeCensor(case1$LastTiboloneCensorDate, case1$CensorDate, 90)
case1$HRTTib <- ifelse(is.na(case1$HRT) & is.na(case1$Tibolone), NA, 
                                ifelse(case1$HRT==T|case1$Tibolone==T, T, F))

## load data for exposure 
exp<-read.csv("R:/Students/Jessy/Osteoporosis Treatments/Datasets/Aurum_Cohort_Exposure_CVD_partb.csv")
exp$AgeGrp <- ifelse(exp$AgeGrp==39, 40, exp$AgeGrp)
exp$AgeGrp <- ifelse(exp$AgeGrp==90, 85, exp$AgeGrp)
exp$Age60 <- ifelse(exp$AgeGrp<60, "<60", ">=60")
exp$Age65 <- ifelse(exp$AgeGrp<65, "<65", ">=65")
exp$Age70 <- ifelse(exp$AgeGrp<70, "<70", ">=70")
colnames(exp)[1] <- "ExpCensor7"
year_levels <- c(1, 2, 3, 4)
year_labels <- c("2000-2004", "2005-2009", "2010-2014", "2015-2020")
exp$Year <- factor(exp$Year, levels = year_levels, labels = year_labels)
exp$Year2 <- ifelse(exp$Year=="2000-2004"|exp$Year=="2005-2009", "2000-2009", "2010-2020")
exp$HRTTib <- ifelse(exp$HRT==1|exp$Tibolone==1, 1, 0)
exp$Smoking <- ifelse(exp$Smoking=="Z", "U", exp$Smoking)
exp$Smoking <- as.factor(exp$Smoking)

# grouping multiple treatments
multitrt <- c('J','K','L','M','N')
case1 <- case1 %>% mutate(ExpCensor7=ifelse(ExpCensor7 %in% multitrt, "M", ExpCensor7))
exp <- exp %>% mutate(ExpCensor7=ifelse(ExpCensor7 %in% multitrt, "M", ExpCensor7))

# merge data for case group (MI/Stroke/AFib)
caseMI <- read.csv("R:/Students/Jessy/Osteoporosis Treatments/Datasets/20240913_Osteoporosis_MI_All.csv")
caseStroke <- read.csv("R:/Students/Jessy/Osteoporosis Treatments/Datasets/20240918_Osteoporosis_Stroke_All.csv")
caseAFib <- read.csv("R:/Students/Jessy/Osteoporosis Treatments/Datasets/20241028_Osteoporosis_AFib_All.csv")
caseMI <- select(caseMI, -c(CohortEntryDate,CensorDate))
caseStroke <- select(caseStroke, -c(CohortEntryDate,CensorDate))
caseAFib <- select(caseAFib, -c(CohortEntryDate,CensorDate))
case <- reduce(.x = list(case1, caseMI, caseStroke,caseAFib), merge, by = "PID", all = T)
case <- case[, -c(2, 7:16, 21)]

# MI cases
MIcase <- case %>% subset(MIEvent!=0) %>% select(-c(StrokeCase, StrokeCaseSens))
# excluding manual review patients
NonCVproc_incl <- c('20232~19758880','20251~3608025','20290~10403007','20292~4554016','20861~20849046', '20867~24453809', '21086~32113886','21230~59393992','21359~66340447')
NonCVproc_excl <- MIcase %>% filter(MIReview=='Non-CV Proc prior 30 days' & !PID %in% NonCVproc_incl) %>% pull(PID)
HES2nd_excl <- MIcase %>% filter(MIReview=='Secondary HES Dx') %>% pull(PID)
MIcase_final <- MIcase %>% filter(!PID %in% c(NonCVproc_excl,HES2nd_excl))

# Stroke 
Strokecase <- case %>% subset(StrokeCase==1) %>% select(-c(MIEvent, MIReview))
Strokecase_sens <- case %>% subset(StrokeCaseSens==1) %>% select(-c(MIEvent, MIReview))

# AFib
AFibcase <- case %>% subset(AFibEvent==1)%>% select(-c(MIEvent,MIReview,StrokeCase,StrokeCaseSens))
AFibcase_sens <- case %>% subset(AFibEvent==1 & AFibReview!="clot-related CV condition") %>% select(-c(MIEvent,MIReview,StrokeCase,StrokeCaseSens))

# exporting data set
write.csv(exp, "20240918_Osteoporosis_exp.csv", row.names = F, na = "")
write.csv(MIcase_final, "20240918_Osteoporosis_MIcase.csv", row.names = F, na = "")
write.csv(Strokecase, "20240918_Osteoporosis_Strokecase.csv", row.names = F, na = "")
write.csv(Strokecase_sens, "20240918_Osteoporosis_StrokecaseSens.csv", row.names = F, na = "")
write.csv(AFibcase, "20241028_Osteoporosis_AFibcase.csv", row.names = F, na = "")
write.csv(AFibcase_sens, "20241028_Osteoporosis_AFibcaseSens.csv", row.names = F, na = "")

# re-run from here
exp <- read.csv("20240918_Osteoporosis_exp.csv")
MIcase_final <- read.csv("20240918_Osteoporosis_MIcase.csv")
Strokecase <- read.csv("20240918_Osteoporosis_Strokecase.csv")
Strokecase_sens <- read.csv("20240918_Osteoporosis_StrokecaseSens.csv")
FatalStrokecase <- read.csv("20240930_Osteoporosis_FatalStrokecase.csv")
AFibcase <- read.csv("20241028_Osteoporosis_AFibcase.csv")
AFibcase_sens <- read.csv("20241028_Osteoporosis_AFibcaseSens.csv")

# Incidence Rates (IRs)
source("R:/Students/Jessy/Osteoporosis Treatments/Datasets/case/IR functions.R")

# stratified IR tables
strataIR_Table(AFibcase_sens, exp, "ExpCensor7")
strataIR_Table(AFibcase_sens, exp, "ExpCensor7", "Year")
strataIR_Table(AFibcase_sens, exp, "ExpCensor7", "AgeGrp")
strataIR_Table(AFibcase_sens, exp, "ExpCensor7", "Age65")
strataIR_Table(AFibcase_sens, exp, "ExpCensor7", "Age70")
strataIR_Table(AFibcase_sens, exp, "ExpCensor7", "Severity")
strataIR_Table(AFibcase_sens, exp, "ExpCensor7", "CKD")
strataIR_Table(AFibcase_sens, exp, "ExpCensor7", "AutoImmune")
strataIR_Table(AFibcase_sens, exp, "ExpCensor7", "StrdExp")
strataIR_Table(AFibcase_sens, exp, "ExpCensor7", "Calcium")
strataIR_Table(AFibcase_sens, exp, "ExpCensor7", "HRTTib")
strataIR_Table(AFibcase_sens, exp, "ExpCensor7", "Smoking")
IR_byAge70_Table <- IR_byAge70_Table[, c(2,4,3,5,7,6,8)]

# long IR table
StratifiedIR_longTable <- bind_rows(long_IR_Crude, long_IR_byYear, long_IR_byAgeGrp, long_IR_byAge70, long_IR_bySeverity, long_IR_byCKD, 
                                    long_IR_byAutoImmune, long_IR_byStrdExp, long_IR_byCalcium, long_IR_byHRTTib, long_IR_bySmoking)

drug_list <- c('A'='Alendronate', 'B'='Ibandronate', 'C'='Risedronate', 'D'='ZoledronicAcid', 'E'='Etidronate', 
               'F'='Denosumab', 'G'='Raloxifene', 'H'='Teriparatide', 'I'='Calcitonin', 'M'='2+ study drugs', 'U'='VitD Only', 'Z'='Past Use')
StratifiedIR_longTable$Exposure <- drug_list[StratifiedIR_longTable$ExpCensor7]

# exporting 
write.csv(StratifiedIR_longTable, "StratifiedIR_MI_20250108.csv")
write.csv(StratifiedIR_longTable, "StratifiedIR_Stroke_20250108.csv")
write.csv(StratifiedIR_longTable, "StratifiedIR_StrokeSens_20250108.csv")
write.csv(StratifiedIR_longTable, "StratifiedIR_FatalStroke_20250108.csv")
write.csv(StratifiedIR_longTable, "StratifiedIR_AFib_20250108.csv")
write.csv(StratifiedIR_longTable, "StratifiedIR_AFibSens_20250108.csv")



# bar plots
exp <- exp %>% filter(!ExpCensor7 %in% c('H',"I","M"))
MIcase_final <- MIcase_final %>% filter(!ExpCensor7 %in% c('H',"I","M"))
Strokecase <- Strokecase %>% filter(!ExpCensor7 %in% c('H',"I","M"))
AFibcase <- AFibcase %>% filter(!ExpCensor7 %in% c('H',"I","M"))

# Age group by decade
exp$AgeGrp10 <- ifelse(exp$AgeGrp<50, '40-49', 
                       ifelse(exp$AgeGrp<60, '50-59', 
                              ifelse(exp$AgeGrp<70, '60-69', 
                                     ifelse(exp$AgeGrp<80, '70-79', '80-89'))))
AFibcase$AgeGrp10 <- ifelse(AFibcase$AgeGrp<50, '40-49', 
                       ifelse(AFibcase$AgeGrp<60, '50-59', 
                              ifelse(AFibcase$AgeGrp<70, '60-69', 
                                     ifelse(AFibcase$AgeGrp<80, '70-79', '80-89'))))

# re-arrange smoking strata
smoke_levels <- c('N', 'S', 'X', 'U')
smoke_labels <- c("Non-smoker","Current Smoker", "Former Smoker", "Unknown")
exp$Smoking <- factor(exp$Smoking, levels = smoke_levels, labels = smoke_labels)
MIcase_final$Smoking <- factor(MIcase_final$Smoking, levels = smoke_levels, labels = smoke_labels)
Strokecase$Smoking <- factor(Strokecase$Smoking, levels = smoke_levels, labels = smoke_labels)
AFibcase$Smoking <- factor(AFibcase$Smoking, levels = smoke_levels, labels = smoke_labels)

# labels 
year_labels <- c("2000-2004", "2005-2009", "2010-2014", "2015-2020")
age_labels <- c("40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89")
age10_labels <- c("40-49", "50-59", "60-69", "70-79", "80-89")
ser_labels <- c("Osteoporosis", "Osteopenia", "Monitoring/Unknown")
ckd_labels <- c("No CKD Diagnosis", "CKD diagnosis")
autoim_labels <- c("No Autoimmune Diagnosis", "Autoimmune diagnosis")
strd_labels <- c("No Steroid", "Steroid")
ca_labels <- c("No Calcium", "Calcium")
hrttib_labels <- c("No HRT or Tibolone", "HRT and/or Tibolone")
smoking_labels <- c("Non-smoker", "Current Smoker", "Former Smoker", "Unknown")

# stratified IR bar plots
strataIR_Barplot(AFibcase, exp, "ExpCensor7", "Year", year_labels, "Calendar Year")
strataIR_Barplot(AFibcase, exp, "ExpCensor7", "AgeGrp", age_labels, "Age")
strataIR_Barplot(AFibcase, exp, "ExpCensor7", "AgeGrp10", age10_labels, "Age")
strataIR_Barplot(AFibcase, exp, "ExpCensor7", "Severity", ser_labels, "Disease Type")
strataIR_Barplot(AFibcase, exp, "ExpCensor7", "Smoking", smoking_labels, "Smoking Status")
strataIR_Barplot(AFibcase, exp, "ExpCensor7", "CKD", ckd_labels, "Chronic Kidney Disease")
strataIR_Barplot(AFibcase, exp, "ExpCensor7", "AutoImmune", autoim_labels, "Autoimmune Disorders")
strataIR_Barplot(AFibcase, exp, "ExpCensor7", "StrdExp", strd_labels, "Steroid Use")
strataIR_Barplot(AFibcase, exp, "ExpCensor7", "Calcium", ca_labels, "Prescription Calcium Use")
strataIR_Barplot(AFibcase, exp, "ExpCensor7", "HRTTib", hrttib_labels, "HRT/Tibolone Use")


