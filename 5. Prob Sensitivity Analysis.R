#########################################
######  Prob. Sensitivity Analysis  #####  
#########################################

# =======================================
#    National Health Service
# =======================================
n_psa <- 1000

# Reproducibility
set.seed(123) 

psa_inputs_nhs <- data.frame(
  # --- ARM 1 (N = 1092) ---
  
  # Sensitivity (N = 108 Disease(+))
  arm1_sensitivity = sim_p_psa(input_nhs$arm1_sensitivity, n_cohort = 108, n_sims = n_psa),
  
  # Specificity (N = 984 Disease(-))
  arm1_specificity = sim_p_psa(input_nhs$arm1_specificity, n_cohort = 984, n_sims = n_psa),
  
  # Arbitration rate (N = 1092 total)
  arm1_p_arb       = sim_p_psa(input_nhs$arm1_p_arb, n_cohort = 1092, n_sims = n_psa),
  
  # --- ARM 2 ---
  
  # Sensitivity (N = 179 Disease (+))
  arm2_sensitivity = sim_p_psa(input_nhs$arm2_sensitivity, n_cohort = 179, n_sims = n_psa),
  
  # Specificity (N = 1689 Disease (-))
  arm2_specificity = sim_p_psa(input_nhs$arm2_specificity, n_cohort = 1689, n_sims = n_psa),
  
  prevalence       = sim_p_psa(input_nhs$prevalence, n_cohort = 2960, n_sims = n_psa),
  
  # --- COSTS ---
  
  cost_mammogram   = sim_c_psa(input_nhs$cost_mammogram, n_psa),
  cost_ultrasound  = sim_c_psa(input_nhs$cost_ultrasound, n_psa),
  cost_lunit       = sim_c_psa(input_nhs$cost_lunit, n_psa),
  cost_biopsy      = sim_c_psa(input_nhs$cost_biopsy, n_psa),
  
  reading_time_senior_radiologist     = sim_c_psa(input_nhs$reading_time_senior_radiologist, n_psa),
  reading_time_consultant_radiologist = sim_c_psa(input_nhs$reading_time_consultant_radiologist, n_psa),
  
  annual_salary_senior_radiologist     = sim_c_psa(input_nhs$annual_salary_senior_radiologist, n_psa),
  annual_salary_consultant_radiologist = sim_c_psa(input_nhs$annual_salary_consultant_radiologist, n_psa)
)



psa_list_nhs <- lapply(1:nrow(psa_inputs_nhs), function(i) {
  
  current_params <- psa_inputs_nhs[i, ]
  
  res <- dec_tree(params = current_params, n_cohort = 1000)
  
  data.frame(
    Simulation = i,
    Total_Cost_Arm1 = res$Total_Cost[res$Strategy == "Arm 1: Usual Care"],
    Total_Cost_Arm2 = res$Total_Cost[res$Strategy == "Arm 2: Lunit AI"],
    Incremental_Cost = res$Total_Cost[res$Strategy == "Arm 2: Lunit AI"] - res$Total_Cost[res$Strategy == "Arm 1: Usual Care"]
  )
})

psa_results_nhs <- do.call(rbind, psa_list_nhs)

mean_val_psa_nhs <- mean(psa_results_nhs$Incremental_Cost)
prob_saving_nhs  <- mean(psa_results_nhs$Incremental_Cost < 0) * 100

mean_label_text_nhs <- paste0("Mean Incremental Cost = £", round(mean_val_psa_nhs, 2))

mean_point_data_nhs <- data.frame(
  x = mean_val_psa_nhs,
  y = 0,
  Category = "Mean" 
)

ggplot(psa_results_nhs, aes(x = Incremental_Cost)) +
  stat_density(geom = "area", fill = "#f8f9fa", color = NA) +
  
  stat_density(aes(fill = after_stat(x < 0)), 
               geom = "area", alpha = 0.8, color = NA, show.legend = FALSE) +
  
  stat_density(geom = "line", color = "#2c3e50", size = 0.8) +
  
  geom_vline(xintercept = 0, linetype = "solid", color = "white", size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#e74c3c", size = 0.5) +
  
  geom_point(data = mean_point_data_nhs, 
             aes(x = x, y = y, color = Category), 
             size = 3) +
  
  scale_fill_manual(values = c("TRUE" = "#00468B", "FALSE" = "#ced4da")) +
  
  scale_color_manual(
    name = NULL, 
    values = c("Mean" = "#e74c3c"),       
    labels = c("Mean" = mean_label_text_nhs)  
  ) + 
  
  scale_x_continuous(labels = dollar_format(prefix = "£")) +
  
  labs(
    title = "Probabilistic Sensitivity Analysis - NHS Perspective",
    subtitle = paste0("The blue area represents a ", round(prob_saving_nhs, 1), 
                      "% probability of cost savings compared to usual care"),
    x = "Incremental Cost per Patient (£)",
    y = "Probability Density",
    caption = NULL 
  ) +
  
  theme_classic(base_size = 12) +
  theme(
    text = element_text(color = "#2c3e50"),
    plot.title = element_text(face = "bold", size = 16, margin = margin(b=5)),
    plot.subtitle = element_text(size = 11, color = "#5a6268", margin = margin(b=20)),
    
    legend.position = "bottom", 
    legend.justification = "right",
    legend.key = element_blank(),
    legend.text = element_text(size = 10, face = "bold"),
    
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_text(color = "#95a5a6"),
    axis.title.x = element_text(face = "bold", margin = margin(t=10)),
    plot.margin = margin(20, 20, 20, 20)
  )

# =======================================
#        Healthcare Providers
# =======================================

# Reproducibility
set.seed(1234) 

psa_inputs_provider <- data.frame(
  # --- ARM 1 (N = 1092) ---
  
  # Sensitivity (N = 108 Disease(+))
  arm1_sensitivity = sim_p_psa(input_provider$arm1_sensitivity, n_cohort = 108, n_sims = n_psa),
  
  # Specificity (N = 984 Disease(-))
  arm1_specificity = sim_p_psa(input_provider$arm1_specificity, n_cohort = 984, n_sims = n_psa),
  
  # Arbitration rate (N = 1092 total)
  arm1_p_arb       = sim_p_psa(input_provider$arm1_p_arb, n_cohort = 1092, n_sims = n_psa),
  
  # --- ARM 2 ---
  
  # Sensitivity (N = 179 Disease (+))
  arm2_sensitivity = sim_p_psa(input_provider$arm2_sensitivity, n_cohort = 179, n_sims = n_psa),
  
  # Specificity (N = 1689 Disease (-))
  arm2_specificity = sim_p_psa(input_provider$arm2_specificity, n_cohort = 1689, n_sims = n_psa),
  
  prevalence       = sim_p_psa(input_provider$prevalence, n_cohort = 2960, n_sims = n_psa),
  
  # --- COSTS ---
  
  cost_mammogram   = sim_c_psa(input_provider$cost_mammogram, n_psa),
  cost_ultrasound  = sim_c_psa(input_provider$cost_ultrasound, n_psa),
  cost_lunit       = sim_c_psa(input_provider$cost_lunit, n_psa),
  cost_biopsy      = sim_c_psa(input_provider$cost_biopsy, n_psa),
  
  reading_time_senior_radiologist     = sim_c_psa(input_provider$reading_time_senior_radiologist, n_psa),
  reading_time_consultant_radiologist = sim_c_psa(input_provider$reading_time_consultant_radiologist, n_psa),
  
  annual_salary_senior_radiologist     = sim_c_psa(input_provider$annual_salary_senior_radiologist, n_psa),
  annual_salary_consultant_radiologist = sim_c_psa(input_provider$annual_salary_consultant_radiologist, n_psa)
)


psa_list_provider <- lapply(1:nrow(psa_inputs_provider), function(i) {
  
  current_params <- psa_inputs_provider[i, ]
  
  res <- dec_tree(params = current_params, n_cohort = 1000)
  
  data.frame(
    Simulation = i,
    Total_Cost_Arm1 = res$Total_Cost[res$Strategy == "Arm 1: Usual Care"],
    Total_Cost_Arm2 = res$Total_Cost[res$Strategy == "Arm 2: Lunit AI"],
    Incremental_Cost = res$Total_Cost[res$Strategy == "Arm 2: Lunit AI"] - res$Total_Cost[res$Strategy == "Arm 1: Usual Care"]
  )
})

psa_results_provider <- do.call(rbind, psa_list_provider)


mean_val_psa_provider <- mean(psa_results_provider$Incremental_Cost)
prob_saving_provider <- mean(psa_results_provider$Incremental_Cost < 0) * 100

mean_label_text_provider <- paste0("Mean Incremental Cost = £", round(mean_val_psa_provider, 2))

mean_point_data_provider <- data.frame(
  x = mean_val_psa_provider,
  y = 0,
  Category = "Mean" 
)

ggplot(psa_results_provider, aes(x = Incremental_Cost)) +
  stat_density(geom = "area", fill = "#f8f9fa", color = NA) +
  
  stat_density(aes(fill = after_stat(x < 0)), 
               geom = "area", alpha = 0.8, color = NA, show.legend = FALSE) +
  
  stat_density(geom = "line", color = "#2c3e50", size = 0.8) +
  
  geom_vline(xintercept = 0, linetype = "solid", color = "white", size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#e74c3c", size = 0.5) +
  
  geom_point(data = mean_point_data_provider, 
             aes(x = x, y = y, color = Category), 
             size = 3) +
  
  scale_fill_manual(values = c("TRUE" = "#00468B", "FALSE" = "#ced4da")) +
  
  scale_color_manual(
    name = NULL, 
    values = c("Mean" = "#e74c3c"),       
    labels = c("Mean" = mean_label_text_provider)  
  ) + 
  
  scale_x_continuous(labels = dollar_format(prefix = "£")) +
  
  labs(
    title = "Probabilistic Sensitivity Analysis - Provider Perspective",
    subtitle = paste0("The blue area represents a ", round(prob_saving_provider, 1), 
                      "% probability of cost savings compared to usual care"),
    x = "Incremental Cost per Patient (£)",
    y = "Probability Density",
    caption = NULL 
  ) +
  
  theme_classic(base_size = 12) +
  theme(
    text = element_text(color = "#2c3e50"),
    plot.title = element_text(face = "bold", size = 16, margin = margin(b=5)),
    plot.subtitle = element_text(size = 11, color = "#5a6268", margin = margin(b=20)),
    
    legend.position = "bottom", 
    legend.justification = "right",
    legend.key = element_blank(),
    legend.text = element_text(size = 10, face = "bold"),
    
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_text(color = "#95a5a6"),
    axis.title.x = element_text(face = "bold", margin = margin(t=10)),
    plot.margin = margin(20, 20, 20, 20)
  )
