library(tidyverse)
## load data for exposure 
exp<-read.csv("R:/Students/Jessy/Osteoporosis Treatments/Datasets/case/20240918_Osteoporosis_exp.csv")
exp <- exp[, c(1:8, 15, 11, 12)]

# List of subgroup variables
subgroups <- c("Year", "AgeGrp", "Severity", "CKD", "AutoImmune", "StrdExp", "Calcium", "HRTTib", "Smoking") 
# Initialize the data frame to store results
exp_proportion_list <- list()
# Loop over each subgroup
for (subgroup in subgroups) {
  # Calculate the proportion for the current subgroup
  exp_proportion <- exp %>%
    group_by(ExpCensor7, !!sym(subgroup)) %>%
    summarise(Total_Count = sum(Count), .groups = 'drop') %>%
    group_by(ExpCensor7) %>% 
    mutate(Proportion = round(Total_Count / sum(Total_Count)*100, 2)) %>%
    ungroup()
  
  # Store the result in the list
  exp_proportion_list[[subgroup]] <- exp_proportion
}

drug_list <- c('A'='Alendronate', 'B'='Ibandronate', 'C'='Risedronate', 'D'='ZoledronicAcid', 'E'='Etidronate', 
               'F'='Denosumab', 'G'='Raloxifene', 'H'='Teriparatide', 'I'='Calcitonin', 'M'='2+ study drugs', 'U'='VitD Only', 'Z'='Past Use')

library(ggsci)
library(ggpubr)
# Loop through the list to create bar plots for each subgroup
for (subgroup in names(exp_proportion_list)) {
  # Extract the data frame for the current subgroup
  df_plot <- exp_proportion_list[[subgroup]]
  df_plot$Exposure <- drug_list[df_plot$ExpCensor7]
  # Create the bar plot
  p <- ggplot(df_plot, aes(x = Exposure, y = Proportion, fill= factor(!!sym(subgroup)))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("PY Proportion by Osteoporosis Treatment and", subgroup),
         x = "Osteoporosis Treatment",
         y = "PY Proportion") +
    theme_pubr() +
    scale_fill_npg() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  
  # Print the plot
  print(p)
}

# Combine the results into one data frame
exp_strata <- bind_rows(exp_proportion_list, .id = "Subgroup")

