#########################################
######           Functions         ######    
#########################################

dec_tree <- function(params, n_cohort, driver = "sensitivity") { 
  with(
    as.list(params), 
    {
      # ==========================================
      #   CONSTANTS
      # ==========================================
      min_per_year <- 52 * 37.5 * 60  
      
      # ==========================================
      #   1. ARM 1: USUAL CARE 
      # ==========================================
      p_disease_a1 <- prevalence 
      p_healthy_a1 <- 1 - p_disease_a1
      
      p_tp_a1   <- p_disease_a1 * arm1_sensitivity
      p_fn_a1   <- p_disease_a1 * (1 - arm1_sensitivity) 
      p_tn_a1   <- p_healthy_a1 * arm1_specificity
      p_fp_a1   <- p_healthy_a1 * (1 - arm1_specificity)
      
      # ==========================================
      #   2. ARM 2: LUNIT AI (Dynamic Trade-off)
      # ==========================================
      
      # A. Baseline Parameters 
      base_sens <- 0.939 
      base_spec <- 0.922
      
      # B. Diagnostic Odds Ratio (DOR) Calculation
      dor_lunit <- (base_sens * base_spec) / ((1 - base_sens) * (1 - base_spec))
      
      # C. Dynamic Calculation based on Driver 

      if (driver == "sensitivity") {
        final_sens_a2 <- arm2_sensitivity
        final_spec_a2 <- (dor_lunit * (1 - final_sens_a2)) / (final_sens_a2 + (dor_lunit * (1 - final_sens_a2)))
        
      } else if (driver == "specificity") {
        final_spec_a2 <- arm2_specificity
        final_sens_a2 <- (dor_lunit * (1 - final_spec_a2)) / (final_spec_a2 + (dor_lunit * (1 - final_spec_a2)))
        
      } else if (driver == "psa") {
        final_sens_a2 <- arm2_sensitivity
        final_spec_a2 <- arm2_specificity
        
      } else if(driver == "lunit cost") {
        final_sens_a2 <- arm2_sensitivity
        final_spec_a2 <- arm2_specificity
        
      } else {
        stop("Error: argument 'driver' must be 'sensitivity', 'specificity', 'psa', or 'lunit cost'")
      }
      
      p_disease_a2 <- prevalence 
      p_healthy_a2 <- 1 - p_disease_a2
      
      p_tp_a2   <- p_disease_a2 * final_sens_a2
      p_fn_a2   <- p_disease_a2 * (1 - final_sens_a2)
      p_tn_a2   <- p_healthy_a2 * final_spec_a2
      p_fp_a2   <- p_healthy_a2 * (1 - final_spec_a2)
      
      # ==========================================
      #   3. COST CALCULATIONS
      # ==========================================
      
      # --- Costs Arm 1 ---
      cost_hr_a1  <- (2 * ((annual_salary_senior_radiologist/min_per_year) * reading_time_senior_radiologist))  
      cost_arb_a1 <- (arm1_p_arb * ((annual_salary_consultant_radiologist/min_per_year) * reading_time_consultant_radiologist))
      
      tc_tp_a1   <- cost_mammogram + cost_ultrasound + cost_hr_a1 + cost_arb_a1 + cost_biopsy 
      tc_fn_a1   <- cost_mammogram + cost_ultrasound + cost_hr_a1 + cost_arb_a1 
      tc_tn_a1   <- cost_mammogram + cost_ultrasound + cost_hr_a1 + cost_arb_a1 
      tc_fp_a1   <- cost_mammogram + cost_ultrasound + cost_hr_a1 + cost_arb_a1 + cost_biopsy 
      
      etc_a1     <- (tc_tp_a1 * p_tp_a1) + (tc_fn_a1 * p_fn_a1) + (tc_tn_a1 * p_tn_a1) + (tc_fp_a1 * p_fp_a1)
      
      # --- Costs Arm 2 ---
      cost_hr_a2  <- (1 * ((annual_salary_senior_radiologist/min_per_year) * reading_time_senior_radiologist)) 
      cost_arb_a2 <- (arm1_p_arb * ((annual_salary_consultant_radiologist/min_per_year) * reading_time_consultant_radiologist))  
      
      tc_tp_a2    <- cost_mammogram + cost_ultrasound + cost_hr_a2 + cost_arb_a2 + cost_lunit + cost_biopsy 
      tc_fn_a2    <- cost_mammogram + cost_ultrasound + cost_hr_a2 + cost_arb_a2 + cost_lunit  
      tc_tn_a2    <- cost_mammogram + cost_ultrasound + cost_hr_a2 + cost_arb_a2 + cost_lunit  
      tc_fp_a2    <- cost_mammogram + cost_ultrasound + cost_hr_a2 + cost_arb_a2 + cost_lunit + cost_biopsy
      
      etc_a2     <- (tc_tp_a2 * p_tp_a2) + (tc_fn_a2 * p_fn_a2) + (tc_tn_a2 * p_tn_a2) + (tc_fp_a2 * p_fp_a2)
      
      # --- Unit Costs for Reporting ---
      uc_scre_a1 <- cost_mammogram + cost_ultrasound + cost_hr_a1 + cost_arb_a1
      uc_bio_a1  <- (p_tp_a1 + p_fp_a1) * cost_biopsy
      
      uc_ai_a2   <- cost_lunit 
      uc_scre_a2 <- cost_mammogram + cost_ultrasound + cost_hr_a2 + cost_arb_a2 + uc_ai_a2
      uc_bio_a2  <- (p_tp_a2 + p_fp_a2) * cost_biopsy
      
      # ==========================================
      #   4. RESULTS OUTPUT
      # ==========================================
      
      df_results <- data.frame(
        Strategy             = c("Arm 1: Usual Care", "Arm 2: Lunit AI"),
        Cohort_Size          = c(n_cohort, n_cohort),
           
        Used_Sensitivity     = c(arm1_sensitivity, final_sens_a2),
        Used_Specificity     = c(arm1_specificity, final_spec_a2),
           
        TP                   = round(c(p_tp_a1 * n_cohort, p_tp_a2 * n_cohort)),
        FP                   = round(c(p_fp_a1 * n_cohort, p_fp_a2 * n_cohort)),
        FN                   = round(c(p_fn_a1 * n_cohort, p_fn_a2 * n_cohort)),
        TN                   = round(c(p_tn_a1 * n_cohort, p_tn_a2 * n_cohort)),
        Cancer_Detected      = round(c(p_tp_a1 * n_cohort, p_tp_a2 * n_cohort)),
        Biopsies             = round(c((p_tp_a1 + p_fp_a1) * n_cohort, (p_tp_a2 + p_fp_a2) * n_cohort)),
        Interval_Cancer      = round(c(p_fn_a1 * n_cohort, p_fn_a2 * n_cohort)),
        
        Cost_Mammogram       = c(cost_mammogram, cost_mammogram),
        Cost_Ultrasound      = c(cost_ultrasound, cost_ultrasound),
        Cost_Human_Reading   = c(cost_hr_a1, cost_hr_a2),
        Cost_Arbitration     = c(cost_arb_a1, cost_arb_a2),
        Cost_Screening       = c(uc_scre_a1, uc_scre_a2),
        Cost_Biopsy          = c(uc_bio_a1, uc_bio_a2),
        Total_Cost           = c(etc_a1, etc_a2)
      )
      
      return(df_results)
    }
  )
}


break_even_point <- function(target_value, params, n_cohort, param_name, driver = "sensitivity") {
  
  # 1. Dynamically update the specific parameter to evaluate
  params[[param_name]] <- target_value
  
  # 2. Run the tree passing the 'driver' argument
  res <- dec_tree(params, n_cohort, driver = driver)
  
  # 3. Return the Incremental Cost (Cost AI - Cost Usual Care)
  diff <- res$Total_Cost[2] - res$Total_Cost[1]
  
  return(diff)
}

# Standard error for probabilities => 95% = 0.01, remaining 0.1
sim_p_psa <- function(mu, n_cohort = NULL, n_sims = 1000) {
  
  if (mu >= 1 | mu <= 0) return(rep(mu, n_sims))
  

  if (!is.null(n_cohort)) {
    se <- sqrt((mu * (1 - mu)) / n_cohort)
  } else {
    se_pct <- if (mu > 0.95) 0.01 else 0.1
    se <- mu * se_pct
  }
  
  max_se <- sqrt(mu * (1 - mu)) * 0.999
  
  final_se <- min(se, max_se)
  
  var <- final_se^2
  alpha <- mu * ((mu * (1 - mu) / var) - 1)
  beta  <- (1 - mu) * ((mu * (1 - mu) / var) - 1)
  
  return(round(rbeta(n_sims, alpha, beta), 5))
}

sim_c_psa <- function(mu, n = 1000) {
  if (mu <= 0) return(rep(0, n))
  
  se <- mu * 0.1 
  shape <- mu^2 / se^2
  scale <- se^2 / mu
  
  return(round(rgamma(n, shape = shape, scale = scale), 5))
}
