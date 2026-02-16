#########################################
###### One-way Sensitivity Analysis #####  
#########################################

# =======================================
#   Break Even Point Analysis: NHS
# =======================================

bep_arm2_sensitivity_nhs <- uniroot(f = break_even_point, 
                                    interval = c(0.7, 0.999), 
                                    params = input_nhs,
                                    n_cohort = 1000, 
                                    param_name = "arm2_sensitivity",
                                    driver = "sensitivity")

y_min <- -10
y_max <- 20

curve(expr = Vectorize(break_even_point, vectorize.args = "target_value")(
  target_value = x, 
  params = input_nhs, 
  n_cohort = 1000, 
  param_name = "arm2_sensitivity", 
  driver = "sensitivity"            
), 
from = 0.7, to = 0.999,            
ylim = c(y_min, y_max), 
col = "#8C8781", 
lwd = 2,
axes = FALSE,   
ann = FALSE
)   

abline(h = 0, col = "#404040", lty = 3, lwd = 1)

axis(side = 1, at = seq(0.7, 1.0, by = 0.1), cex.axis = 0.7) 
axis(side = 2, at = seq(y_min, y_max, by = 10), cex.axis = 0.7, las = 1)

title(main = "Break Even Point Analysis - NHS Perspective", 
      cex.main = 1.2, font.main = 2) 

title(xlab = "Sensitivity (Arm 2)", 
      ylab = "Incremental Cost (£)", 
      cex.lab = 0.9, font.lab = 2)   

box()   

root_value <- bep_arm2_sensitivity_nhs$root
base_value <- input_nhs$arm2_sensitivity

points(x = root_value, y = 0, pch = 19, col = "#D98C75", cex = 0.8)
abline(v = root_value, col = "#D98C75", lty = "dotted")

text(x = root_value, 
     y = 10,  
     labels = paste0("BEP: ", round(root_value, 3), "\nBase: ", round(base_value, 3)), 
     col = "#D98C75", 
     pos = 1,      
     offset = 0.8,   
     cex = 0.6, 
     font = 2)


new_sensitivity_nhs <- data.frame(
  # Probabilities Arm 1 
  arm1_sensitivity                    = arm1_sensitivity,
  arm1_specificity                    = arm1_specificity,
  
  arm1_p_arb                          = arm1_p_arb,      
  
  # Probabilities Arm 2               
  arm2_sensitivity                    = bep_arm2_sensitivity_nhs$root,  
  arm2_specificity                    = arm2_specificity,
  
  prevalence                          = pooled_prevalence,
  
  # Costs              
  cost_mammogram                      = cost_mammogram_nhs,
  cost_ultrasound                     = cost_ultrasound_nhs,
  reading_time_senior_radiologist     = reading_time_senior_radiologist,
  reading_time_consultant_radiologist = reading_time_consultant_radiologist,
  annual_salary_senior_radiologist     = annual_salary_senior_radiologist_nhs,
  annual_salary_consultant_radiologist = annual_salary_consultant_radiologist_nhs,
  cost_lunit                          = cost_lunit_nhs, 
  cost_biopsy                         = cost_biopsy_nhs
  
)

new_sensitivity_nhs <- dec_tree(params = new_sensitivity_nhs, n_cohort = 1000, driver = "sensitivity")




# =======================================
#   Break Even Point Analysis: Provider
# =======================================

bep_lunit_cost_provider <- uniroot(f = break_even_point, 
                                    interval = c(1, 20), 
                                    params = input_provider,
                                    n_cohort = 1000, 
                                    param_name = "cost_lunit",
                                    driver = "lunit cost")

y_min <- -10
y_max <- 20

curve(expr = Vectorize(break_even_point, vectorize.args = "target_value")(
  target_value = x, 
  params = input_provider, 
  n_cohort = 1000, 
  param_name = "cost_lunit", 
  driver = "lunit cost"            
), 
from = 1, to = 20,            
ylim = c(y_min, y_max), 
col = "#8C8781", 
lwd = 2,
axes = FALSE,   
ann = FALSE
)   

abline(h = 0, col = "#404040", lty = 3, lwd = 1)

axis(side = 1, at = seq(0, 20, by = 5), cex.axis = 0.7) 
axis(side = 2, at = seq(y_min, y_max, by = 10), cex.axis = 0.7, las = 1)

title(main = "Break Even Point Analysis - Provider Perspective", 
      cex.main = 1.2, font.main = 2) 

title(xlab = "Lunit Cost per Scan (£)", 
      ylab = "Incremental Cost (£)", 
      cex.lab = 0.9, font.lab = 2)   

box()   

root_value <- bep_lunit_cost_provider$root
base_value <- input_provider$cost_lunit

points(x = root_value, y = 0, pch = 19, col = "#D98C75", cex = 0.8)
abline(v = root_value, col = "#D98C75", lty = "dotted")

text(x = root_value, 
     y = 10,  
     labels = paste0("BEP: ", round(root_value, 3), "\nBase: ", round(base_value, 3)), 
     col = "#D98C75", 
     pos = 1,      
     offset = 0.8,   
     cex = 0.6, 
     font = 2)



# =======================================
#       Tornado Diagram: NHS
# =======================================

params_to_vary_nhs <- c(
  
  "arm1_sensitivity", 
  "arm1_specificity", 
  "arm1_p_arb",       

  "arm2_sensitivity", 
  "arm2_specificity", 
  
  "prevalence",

  "cost_mammogram",   
  "cost_ultrasound",  
  "reading_time_senior_radiologist_nhs",    
  "reading_time_consultant_radiologist_nhs",
  "annual_salary_senior_radiologist_nhs",    
  "annual_salary_consultant_radiologist_nhs",       
  "cost_lunit",       
  "cost_biopsy"      
)

sensitivity_ranges_nhs <- list()

for(p in params_to_vary_nhs) {
  base_val <- input_nhs[[p]]
  low_val  <- base_val * 0.8
  high_val <- base_val * 1.2
  
  # If it's a probability (contains "_p_" or starts with "arm"), bound between 0 and 1
  if(grepl("_p_", p) | grepl("arm", p)) {
    low_val  <- max(0, low_val)
    high_val <- min(1, high_val)
  }
  
  sensitivity_ranges_nhs[[p]] <- c(BaseCase = base_val, Low = low_val, High = high_val)
}

m_tor_nhs <- matrix(NA, nrow = length(params_to_vary_nhs), ncol = 3)
colnames(m_tor_nhs) <- c("basecase", "low", "high")
rownames(m_tor_nhs) <- params_to_vary_nhs

for(i in seq_along(params_to_vary_nhs)) {
  p_name <- params_to_vary_nhs[i]
  
  current_driver <- "sensitivity" 
  
  if (p_name == "arm2_specificity") {
    current_driver <- "specificity"
  }
  
  for(val_type in c("BaseCase", "Low", "High")) {
    
    temp_params <- input_nhs
    
    val_to_test <- sensitivity_ranges_nhs[[p_name]][val_type]
    temp_params[[p_name]] <- val_to_test
    
    res <- dec_tree(temp_params, n_cohort = 1000, driver = current_driver)
    
    inc_cost <- res$Total_Cost[2] - res$Total_Cost[1]
    
    m_tor_nhs[i, tolower(val_type)] <- inc_cost
  }
}

devtools::source_url("https://github.com/mbounthavong/Decision_Analysis/blob/master/tornado_diagram_code.R?raw=TRUE")

m_tor_nhs_filtered <- m_tor_nhs[abs(m_tor_nhs[, "high"] - m_tor_nhs[, "low"]) > 1e-6, , drop = FALSE]

print(rownames(m_tor_nhs_filtered))

# Plot
tornado_nhs <- TornadoPlot(main_title = "One-Way Sensitivity Analysis - NHS Perspective",
                           Parms = rownames(m_tor_nhs_filtered), 
                           Outcomes = m_tor_nhs_filtered, 
                           outcomeName = "", 
                           xlab = "Incremental Cost (£)", 
                           ylab = "Model Parameters", 
                           col1 = "#8DA399", col2 = "#2F4F4F")

tornado_nhs + theme(
  plot.title = element_text(hjust = 0, size = 14, face = "bold"),
  plot.title.position = "plot",
  
  axis.text.x = element_text(size = 8), 
  axis.text.y = element_text(size = 8), 
  
  axis.title.x = element_text(size = 10),
  axis.title.y = element_text(size = 10),
  
  legend.title = element_text(size = 7), 
  legend.text = element_text(size = 7)   
)


# =======================================
#       Tornado Diagram: Provider
# =======================================

params_to_vary_provider <- c(
  "arm1_sensitivity", 
  "arm1_specificity", 
  "arm1_p_arb",       
  
  "arm2_sensitivity", 
  "arm2_specificity", 
  
  "prevalence",
  
  "cost_mammogram",   
  "cost_ultrasound",  
  "reading_time_senior_radiologist",    
  "reading_time_consultant_radiologist",
  "annual_salary_senior_radiologist",    
  "annual_salary_consultant_radiologist",       
  "cost_lunit",       
  "cost_biopsy"      
)

sensitivity_ranges_provider <- list()

for(p in params_to_vary_provider) {
  base_val <- input_provider[[p]]
  low_val  <- base_val * 0.8
  high_val <- base_val * 1.2
  
  # If it's a probability (contains "_p_" or starts with "arm"), bound between 0 and 1
  if(grepl("_p_", p) | grepl("arm", p)) {
    low_val  <- max(0, low_val)
    high_val <- min(1, high_val)
  }
  
  sensitivity_ranges_provider[[p]] <- c(BaseCase = base_val, Low = low_val, High = high_val)
}

m_tor_provider <- matrix(NA, nrow = length(params_to_vary_provider), ncol = 3)
colnames(m_tor_provider) <- c("basecase", "low", "high")
rownames(m_tor_provider) <- params_to_vary_provider

for(i in seq_along(params_to_vary_provider)) {
  p_name <- params_to_vary_provider[i]
  
  current_driver <- "sensitivity" 
  
  if (p_name == "arm2_specificity") {
    current_driver <- "specificity"
  }
  
  for(val_type in c("BaseCase", "Low", "High")) {
    
    temp_params <- input_provider
    
    val_to_test <- sensitivity_ranges_provider[[p_name]][val_type]
    temp_params[[p_name]] <- val_to_test
    
    res <- dec_tree(temp_params, n_cohort = 1000, driver = current_driver)
    
    inc_cost <- res$Total_Cost[2] - res$Total_Cost[1]
    
    m_tor_provider[i, tolower(val_type)] <- inc_cost
  }
}

devtools::source_url("https://github.com/mbounthavong/Decision_Analysis/blob/master/tornado_diagram_code.R?raw=TRUE")

m_tor_provider_filtered <- m_tor_provider[abs(m_tor_provider[, "high"] - m_tor_provider[, "low"]) > 1e-6, , drop = FALSE]

print(rownames(m_tor_provider_filtered))

# Plot
tornado_provider <- TornadoPlot(main_title = "One-Way Sensitivity Analysis - Provider Perspective",
                                Parms = rownames(m_tor_provider_filtered), 
                                Outcomes = m_tor_provider_filtered, 
                                outcomeName = "", 
                                xlab = "Incremental Cost (£)", 
                                ylab = "Model Parameters", 
                                col1 = "#8DA399", col2 = "#2F4F4F")

tornado_provider + theme(
  plot.title = element_text(hjust = 0, size = 14, face = "bold"),
  plot.title.position = "plot",
  
  axis.text.x = element_text(size = 8), 
  axis.text.y = element_text(size = 8), 
  
  axis.title.x = element_text(size = 10),
  axis.title.y = element_text(size = 10),
  
  legend.title = element_text(size = 7), 
  legend.text = element_text(size = 7)   
)
