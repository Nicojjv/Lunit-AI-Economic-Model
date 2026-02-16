#########################################
######      Model's Parameters     ######    
#########################################

required_packages <- c("ggplot2", "dplyr", "tidyr", "scales", "stringr")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if(length(new_packages)) {
  install.packages(new_packages)
}

lapply(required_packages, library, character.only = TRUE)
message("### Libraries installed and loaded! ###")


# =======================================
#    Transition probabilities: ARM 1
# =======================================

arm1_sensitivity <- 0.943
arm1_specificity <- 0.929
arm1_prevalence  <- 0.091
arm1_p_arb       <- 0.0109
arm1_cohort_size <- 1092

# =======================================
#    Transition probabilities: ARM 2
# =======================================

arm2_sensitivity <- 0.939
arm2_specificity <- 0.922
arm2_prevalence  <- 0.0958
arm2_cohort_size <- 1868
arm2_p_arb       <- 0.0109


total_cancer_cases <- (arm1_prevalence * arm1_cohort_size) + (arm2_prevalence * arm2_cohort_size)

total_population   <- arm1_cohort_size + arm2_cohort_size

pooled_prevalence  <- total_cancer_cases / total_population

# =======================================
#   Costs: National Health Service (NHS)
# =======================================

cost_us_biopsy_nhs        <- 329
cost_stereo_biopsy_nhs    <- 376

# Extracted from National Schedule of NHS Costs
freq_us_biopsy            <- 31
freq_stereo_biopsy        <- 12
total_biopsy              <- freq_us_biopsy + freq_stereo_biopsy
weight_us_biopsy          <- freq_us_biopsy / total_biopsy
weight_stereo_biopsy      <- freq_stereo_biopsy / total_biopsy

# Weighted average cost based on biopsies frequencies
cost_biopsy_nhs           <- (cost_us_biopsy_nhs * weight_us_biopsy) + (cost_stereo_biopsy_nhs * weight_stereo_biopsy)

cost_mammogram_nhs      <- 30                
cost_ultrasound_nhs     <- 48                
cost_lunit_nhs          <- 0            

# cost of Human Readers
cost_hr_nhs             <- 0                  
cost_arb_nhs            <- 0

# =======================================
#   Costs: Healthcare providers
# =======================================
cost_us_biopsy_provider        <- 244.93
cost_stereo_biopsy_provider    <- 380.44
cost_biopsy_provider           <- (cost_us_biopsy_provider * weight_us_biopsy) + 
  (cost_stereo_biopsy_provider * weight_stereo_biopsy)

cost_lunit_provider            <- 3.5            


# Cost Human Reading and Arbitration
min_per_year                        <- (37.5 * 60 * 52)        # minutes worked per year
annual_salary_senior_radiologist_provider     <- 73992
annual_salary_consultant_radiologist_provider <- 120000

annual_salary_senior_radiologist_nhs          <- 0
annual_salary_consultant_radiologist_nhs      <- 0


# Procedure time in minutes
reading_time_senior_radiologist              <- 20        
reading_time_consultant_radiologist          <- 20     


# NHS Uplift
uplift_2020_21 <- 1.025 # ~2.5%
uplift_2021_22 <- 1.011 # ~1.1%
uplift_2022_23 <- 1.052 # ~5.2% 
uplift_2023_24 <- 1.050 # ~5.0% 
uplift_2024_25 <- 1.017 # 1.7% 

cumulative_factor <- uplift_2020_21 * uplift_2021_22 * uplift_2022_23 * uplift_2023_24 * uplift_2024_25

# Costs (£46) based on https://doi.org/10.1038/s41467-023-41754-0 
cost_mammogram_provider  <- 46 * cumulative_factor 
cost_ultrasound_provider <- 79.07


# =======================================
#   Inputs: National Health Service (NHS)
# =======================================

input_nhs <- data.frame(
  # Probabilities Arm 1 
  arm1_sensitivity                    = arm1_sensitivity,
  arm1_specificity                    = arm1_specificity,
              
  arm1_p_arb                          = arm1_p_arb,      
              
  # Probabilities Arm 2               
  arm2_sensitivity                    = arm2_sensitivity,
  arm2_specificity                    = arm2_specificity,
  
  arm2_p_arb                          = arm2_p_arb,      
  
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


# =======================================
#   Inputs: Healthcare Providers
# =======================================

input_provider <- data.frame(
  # Probabilities Arm 1 
  arm1_sensitivity                    = arm1_sensitivity,
  arm1_specificity                    = arm1_specificity,
                
  arm1_p_arb                          = arm1_p_arb,      
              
  # Probabilities Arm 2               
  arm2_sensitivity                    = arm2_sensitivity,
  arm2_specificity                    = arm2_specificity,
  
  arm2_p_arb                          = arm2_p_arb,      
  
  prevalence                          = pooled_prevalence,
  
  # Costs              
  cost_mammogram                      = cost_mammogram_provider,
  cost_ultrasound                     = cost_ultrasound_provider,
  reading_time_senior_radiologist     = reading_time_senior_radiologist,
  reading_time_consultant_radiologist = reading_time_consultant_radiologist,
  annual_salary_senior_radiologist     = annual_salary_senior_radiologist_provider,
  annual_salary_consultant_radiologist = annual_salary_consultant_radiologist_provider,
  cost_lunit                          = cost_lunit_provider, 
  cost_biopsy                         = cost_biopsy_provider
)
