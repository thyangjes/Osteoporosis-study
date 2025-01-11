# Function to create stratified IR Table
strataIR_Table <- function(data_case, data_exp, expcensor, subgrp = NULL) {
  # Required packages
  library(tidyverse)
  
  # Determine grouping variables
  group_vars <- if (is.null(subgrp)) {
    expcensor
  } else {
    c(expcensor, subgrp)
  }
  
  # Grouping and summarizing case data
  CasebyDrugbygrp <- data_case %>% 
    group_by(across(all_of(group_vars))) %>% 
    summarise(Case = n(), .groups = 'drop')
  
  # Grouping and summarizing exposure data
  ExpbyDrugbygrp <- data_exp %>% 
    group_by(across(all_of(group_vars))) %>% 
    summarise(PY = sum(Count), .groups = 'drop')
  
  # Convert person-time from days to years
  ExpbyDrugbygrp <- ExpbyDrugbygrp %>%
    mutate(PY = PY / 365.25)
  
  # Merging case and exposure data
  Ratebygrp <- full_join(ExpbyDrugbygrp, CasebyDrugbygrp, by = group_vars) %>%
    replace(is.na(.), 0)
  
  # Calculate crude or stratified rate and 95% CI
  Ratebygrp <- Ratebygrp %>%
    mutate(
      # Incidence rate per 10,000 person-years
      IR = case_when(
        PY == 0 ~ 0,
        TRUE ~ round(Case / PY * 10000, 1)
      ),
      
      # Lower CI using Poisson method
      lower_CI = case_when(
        Case == 0 ~ 0,
        TRUE ~ round(qchisq(0.025, 2 * Case) / (2 * PY) * 10000, 1)
      ),
      
      # Upper CI using Poisson method
      upper_CI = case_when(
        PY == 0 ~ 0,
        TRUE ~ round(qchisq(0.975, 2 * (Case + 1)) / (2 * PY) * 10000, 1)
      ),
      
      # Format CI string
      CI = case_when(
        IR == 0 ~ "(0.0, 0.0)",
        TRUE ~ sprintf("(%.1f, %.1f)", lower_CI, upper_CI)
      ),
      
      # Combine IR and CI
      IR_CI = case_when(
        IR == 0 ~ "0.0 (0.0, 0.0)",
        TRUE ~ paste(sprintf("%.1f", IR), CI)
      )
    )
  
  # Dynamically set long table name
  longtable_name <- if (is.null(subgrp)) {
    "long_IR_Crude"
  } else {
    paste0("long_IR_by", subgrp)
  }
  assign(longtable_name, Ratebygrp, envir = .GlobalEnv)
  
  # Reshape to wide format if stratified
  if (!is.null(subgrp)) {
    Ratebygrp_Table <- Ratebygrp %>%
      select(all_of(c(expcensor, subgrp, "Case", "PY", "IR_CI"))) %>%
      pivot_wider(names_from = subgrp, values_from = c("Case", "PY", "IR_CI"))
    
    # Create a lookup table for treatment names
    drug_names <- data.frame(
      ExpCensor7 = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'M', 'U', 'Z'),
      Exposure = c('Alendronate', 'Ibandronate', 'Risedronate', 'ZoledronicAcid', 'Etidronate', 'Denosumab',
                   'Raloxifene', 'Teriparatide', 'Calcitonin', '2+ study drugs', 'VitD Only', 'Past Use')
    )
    
    # Merge with drug names
    Ratebygrp_Table <- drug_names %>%
      rename(!!expcensor := ExpCensor7) %>%
      left_join(Ratebygrp_Table, by = expcensor)
    
    # Dynamically set wide table name
    table_name <- paste0("IR_by", subgrp, "_Table")
    assign(table_name, Ratebygrp_Table, envir = .GlobalEnv)
  }
}



# Function to create stratified IR bar plots
strataIR_Barplot <- function(data_case, data_exp, expcensor, subgrp, subgrp_label, subtitle) {
  #required packages
  library(tidyverse)
  library(ggsci)
  library(ggpubr)
  
  # Grouping and summarizing case data
  CasebyDrugbygrp <- data_case %>% 
    group_by(!!sym(expcensor), !!sym(subgrp)) %>% 
    summarise(Case=n(), .groups = 'drop')
  
  # Grouping and summarizing exposure data
  ExpbyDrugbygrp <- data_exp %>% 
    group_by(!!sym(expcensor), !!sym(subgrp)) %>% 
    summarise(PY=sum(Count), .groups = 'drop')
  # Convert person-time from days to years
  ExpbyDrugbygrp$PY <- ExpbyDrugbygrp$PY/365.25
  
  # Merging case and exposure data
  Ratebygrp <- merge(ExpbyDrugbygrp, CasebyDrugbygrp, by=c(expcensor, subgrp), all.x = T)
  Ratebygrp <- Ratebygrp %>% replace(is.na(Ratebygrp), 0)
  
  # Calculate rate and 95% CI
  Ratebygrp <- Ratebygrp %>%
    mutate(
      # Incidence rate per 10,000 person-years
      IR = case_when(
        PY == 0 ~ 0,
        TRUE ~ round(Case / PY * 10000, 1)
      ),
      
      # Lower CI using Poisson method
      lower_CI = case_when(
        Case == 0 ~ 0,
        TRUE ~ round(qchisq(0.025, 2 * Case) / (2 * PY) * 10000, 1)
      ),
      
      # Upper CI using Poisson method
      upper_CI = case_when(
        Case == 0 ~ NA_real_,
        TRUE ~ round(qchisq(0.975, 2 * (Case + 1)) / (2 * PY) * 10000, 1)
      )
    )
  # treatment labels
  trt_levels = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'M', 'U', 'Z')
  trt_labels = c('Alendronate', 'Ibandronate', 'Risedronate', 'ZoledronicAcid', 'Etidronate', 'Denosumab',
                 'Raloxifene', 'Teriparatide', 'Calcitonin', '2+ study drugs', 'VitD Only', 'Past Use')
  Ratebygrp$ExpCensor7 <- factor(Ratebygrp$ExpCensor7, levels = trt_levels, labels = trt_labels)
  
  # Convert subgrp to a factor type and apply labels
  Ratebygrp[[subgrp]] <- factor(Ratebygrp[[subgrp]], labels = subgrp_label)
  
  # Create a dynamic name for the barplot
  barplot_name <- paste0("IR_by", subgrp, "_Barplot")
  
  # Plotting the results
  IR_barplot <- Ratebygrp %>%
    group_by(!!sym(expcensor), !!sym(subgrp)) %>%
    ggplot(aes(!!sym(expcensor), IR, fill= factor(!!sym(subgrp)))) +
    geom_bar(stat="identity", position="dodge") +
    #geom_text(mapping=aes(label=IR), position=position_dodge(width=0.9), cex=2.5, vjust=-4) +
    labs(title = "IR of Atrial Fibrillation by Osteoporosis Treatment", #Myocardial Infarction, Stroke, Atrial Fibrillation 
         subtitle = paste("and",subtitle),
         x = paste("Treatment by",subtitle),
         y = "IR per 10,000 PY", 
         fill = NULL) +
    geom_errorbar(mapping=aes(ymin = lower_CI, ymax = upper_CI), 
                  width = 0.2, position = position_dodge(width = 0.9)) +
    theme_minimal() +
    scale_fill_npg() +
    theme(axis.line = element_line(color = "black"),  # Add back x and y axis lines
          legend.position = "bottom",
          panel.border = element_blank(),  # Remove panel border
          panel.grid.major.x = element_blank(),
          
          # Title text size
          plot.title = element_text(size = 18, hjust = 0.5),
          plot.subtitle = element_text(size = 16, hjust = 0.5),
          
          # Axis text sizes
          axis.title.x = element_text(size = 14),      # X-axis title
          axis.title.y = element_text(size = 14),      # Y-axis title
          axis.text.x = element_text(size = 14, angle = 90, hjust = 1, vjust = 0.5),   # X-axis tick labels
          axis.text.y = element_text(size = 14),       # Y-axis tick labels
          
          # Legend text size
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14),
          ) +
    coord_cartesian(ylim = c(0, 150))
  
  # Assign the plot to a dynamically named variable
  assign(barplot_name, IR_barplot, envir = .GlobalEnv)
  
  # Print the plot
  print(IR_barplot)
}

