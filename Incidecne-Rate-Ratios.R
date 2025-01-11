library(tidyverse)
library(broom)
library(gtsummary)
## load data for exposure 
exp<-read.csv("R:/Students/Jessy/Osteoporosis Treatments/Datasets/case/20240918_Osteoporosis_exp.csv")
case<-read.csv("R:/Students/Jessy/Osteoporosis Treatments/Datasets/case/20240918_Osteoporosis_MIcase.csv")
case<-read.csv("R:/Students/Jessy/Osteoporosis Treatments/Datasets/case/20240918_Osteoporosis_Strokecase.csv")
case<-read.csv("R:/Students/Jessy/Osteoporosis Treatments/Datasets/case/20240918_Osteoporosis_StrokecaseSens.csv")
case<-read.csv("R:/Students/Jessy/Osteoporosis Treatments/Datasets/case/20240930_Osteoporosis_FatalStrokecase.csv")
case<-read.csv("R:/Students/Jessy/Osteoporosis Treatments/Datasets/case/20241028_Osteoporosis_AFibcase.csv")
case<-read.csv("R:/Students/Jessy/Osteoporosis Treatments/Datasets/case/20241028_Osteoporosis_AFibcaseSens.csv")

# function for creating data set
pois_df <- function(expData, caseData, expcensor, predictors= NULL) {
  # grouping for exposure 
  exp_grp <- expData %>%
    group_by(!!sym(expcensor), !!!syms(predictors)) %>%
    summarise(Total_Count = sum(Count), .groups = 'drop')
  
  exp_grp$PY <- exp_grp$Total_Count / 365.25
  exp_grp <- dplyr::select(exp_grp, -Total_Count)
  
  # grouping for case
  case_grp <- caseData %>%
    group_by(!!sym(expcensor), !!!syms(predictors)) %>%
    summarise(Event = n(), .groups = 'drop')
  
  # merge data
  df_final <- merge(exp_grp, case_grp, by = c(expcensor, predictors), all.x = TRUE)
  df_final <- df_final %>% replace(is.na(df_final), 0)
  df_final <- df_final %>% filter(df_final$PY != 0)
  df_final[[expcensor]] <- ifelse(df_final[[expcensor]] == "U", "_ref", df_final[[expcensor]])
  
  drug_list <- c('A'='Alendronate', 'B'='Ibandronate', 'C'='Risedronate', 'D'='ZoledronicAcid', 'E'='Etidronate', 
                 'F'='Denosumab', 'G'='Raloxifene', 'H'='Teriparatide', 'I'='Calcitonin', 'M'='2+ study drugs', '_ref'='_VitD Only', 'Z'='Past Use')
  df_final$ExpCensor <- drug_list[df_final[[expcensor]]]
  
  df_final[predictors] <- lapply(df_final[predictors], as.factor)
  
  return(df_final)
}

# function for strata poission regression model
stratified_pois <- function(data, exposure, predictors= NULL, subgroup, subset_values) {
  
  # Define the drug list inside the function
  drug_list <- c('A'='Alendronate', 'B'='Ibandronate',
                 'C'='Risedronate', 'D'='ZoledronicAcid',
                 'E'='Etidronate', 'F'='Denosumab',
                 'G'='Raloxifene', 'H'='Teriparatide',
                 'I'='Calcitonin', 'M'='2+ study drugs',
                 '_ref'='VitD Only', 'Z'='Past Use')
  
  # Run Poisson regression for each subset
  results_list <- lapply(subset_values, function(value) {
    subset_data <- data %>% filter(get(subgroup) == value)
    
    model <- glm(as.formula(paste("Event ~", paste(c(exposure, predictors), collapse = " + "))),
                 data = subset_data, family = "poisson",
                 offset = log(PY))
    
    results <- as.data.frame(tbl_regression(model, exponentiate = TRUE, tidy_fun = broom.helpers::tidy_parameters)) %>%
      mutate(row_id = row_number())
    return(results)
  })
  
  # Combine the results
  combined_results <- Reduce(function(x, y) full_join(x, y, by = "row_id"), results_list)
  #combined_results <- combined_results %>% select(-row_id)
  
  # Recode drug names
  combined_results <- combined_results[-1,] %>%
    mutate(across(everything(), ~ recode(., !!!drug_list)))
  
  return(combined_results)
}

# All population 
df_Crude <- pois_df(exp, case, "ExpCensor7")
df_YrAgeSevSmo <- pois_df(exp, case, "ExpCensor7", predictors = c("Year", "AgeGrp", "Severity", "Smoking"))

pois_Crude <- glm(Event ~ ExpCensor7, data = df_Crude, family = "poisson", offset = log(PY))
pois_YrAgeSevSmo <- glm(Event ~ ExpCensor7 + Year + AgeGrp + Severity + Smoking, data = df_YrAgeSevSmo, family = "poisson", offset = log(PY))

results_Crude <- as.data.frame(tbl_regression(pois_Crude, exponentiate = T, tidy_fun = broom.helpers::tidy_parameters)) %>% mutate(row_id = row_number())
results_YrAgeSevSmo <- as.data.frame(tbl_regression(pois_YrAgeSevSmo, exponentiate = T, tidy_fun = broom.helpers::tidy_parameters)) %>% mutate(row_id = row_number())

# Subgroup population
df_Age70 <- pois_df(exp, case, "ExpCensor7", predictors = "Age70")
df_Age70_YrAgeSevSmo <- pois_df(exp, case, "ExpCensor7", predictors = c("Age70","Year","AgeGrp","Severity","Smoking"))

results_Age70<- stratified_pois(data = df_Age70,
                                exposure = "ExpCensor7",
                                predictors = NULL,
                                subgroup = "Age70",
                                subset_values = c("<70", ">=70"))

results_Age70_YrAgeSevSmo <- stratified_pois(data = df_Age70_YrAgeSevSmo,
                                             exposure = "ExpCensor7",
                                             predictors = c("Year", "AgeGrp", "Severity", "Smoking"),
                                             subgroup = "Age70",
                                             subset_values = c("<70", ">=70"))
