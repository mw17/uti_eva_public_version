### Define input parameters

# Number of PSA samples
n_samples <- 4   # using a small number for now so easier to look at objects while I write code

patient_subgroup_names <- c("mixed", "women")
n_patient_subgroups <- length(patient_subgroup_names)

test_type_names <- c("rapid_poct_with_ast_or_path", "poct_with_culture", "lab_culture")
n_test_types <- length(test_type_names)

test_names <- c("lodestar_dx", "flexicult_human", "id_flexicult")
n_tests <- length(test_names)

treatment_type_names <- c("targeted", "empiric", "empiric_then_targeted", "no_treatment")
n_treatment_types <- length(treatment_type_names)

# Matrix mapping tests to test_types
mapped_tests <- cbind(test_names, c(test_type_names[1], test_type_names[2], test_type_names[2]))
colnames(mapped_tests) <- c("test_names", "test_type_names")

# Names and costs of different courses of antibiotics
# costs in £
treatment_costs <- c(
  nitrofurantoin_100mg_bd_3d = 4.07, # 100 mg modified-release twice a day for 3 days
  nitrofurantoin_100mg_bd_7d = 9.50, # 100 mg modified-release twice a day for 7 days
  nitrofurantoin_100mg_bd_5d = 6.79, # 100 mg modified-release twice a day for 5 days - average cost for women and men
  cefalexin_500mg_btd_7to10d = 2.74, # 500 mg twice or three times a day for 7 to 10 days, using average cost: (1.61 + 3.86)/2 = 2.74
  trimethoprim_200mg_bd_3d = 0.75, # 200 mg twice a day for 3 days
  trimethoprim_200mg_bd_7d = 1.76, # 200 mg twice a day for 7 days
  trimethoprim_200mg_bd_5d = 1.26) # 200 mg twice a day for 5 days - average cost for women and men

treatment_names <- names(treatment_costs)
n_treatments <- length(treatment_names)

# Probability of needing more than one course of antibiotics (needed for costing)
# Vector with one element per PSA sample
#p_multiple_courses <- rep(NA, n_samples)
# No longer using this. Use number_of_courses instead.

# Mean number of antibiotic courses required for empiric
number_of_courses <- NA

# Matrix mapping patient groups to antibiotic costs
mapped_treatment_costs <- matrix(NA, nrow = n_patient_subgroups, ncol = n_treatments)
rownames(mapped_treatment_costs) <- patient_subgroup_names
colnames(mapped_treatment_costs) <- treatment_names

mapped_treatment_costs[c("women"), "nitrofurantoin_100mg_bd_3d"] <- treatment_costs["nitrofurantoin_100mg_bd_3d"]
mapped_treatment_costs[c("mixed"), "nitrofurantoin_100mg_bd_5d"] <- treatment_costs["nitrofurantoin_100mg_bd_5d"]
mapped_treatment_costs[c("women"), "trimethoprim_200mg_bd_3d"] <- treatment_costs["trimethoprim_200mg_bd_3d"]
mapped_treatment_costs[c("mixed"), "trimethoprim_200mg_bd_5d"] <- treatment_costs["trimethoprim_200m_bd_5d"]

# Matrix mapping test result/patient group combinations to treatment costs
# Multiplying empiric by number_of_courses, but for now not modelling a different cost for later courses of antibiotics
mapped_tests_treatment_costs <- as.data.frame(matrix(NA, nrow = ((1 + n_tests) * n_patient_subgroups), ncol = 3,
                                                     dimnames = list(NULL, c("test_result", "patient_subgroup", "treatment_cost"))))
mapped_tests_treatment_costs[, 1] <- rep(c("empiric", paste0(test_names, "_targ")), each = 2)
mapped_tests_treatment_costs[, 2] <- rep(patient_subgroup_names, times = 2)

mapped_tests_treatment_costs[which(mapped_tests_treatment_costs$test_result == "empiric" &
                                     mapped_tests_treatment_costs$patient_subgroup == "women"), "treatment_cost"] <-
  mapped_treatment_costs["women", "nitrofurantoin_100mg_bd_3d"] * number_of_courses

mapped_tests_treatment_costs[which(mapped_tests_treatment_costs$test_result == "empiric" &
                                     mapped_tests_treatment_costs$patient_subgroup == "mixed"), "treatment_cost"] <-
  mapped_treatment_costs["mixed", "nitrofurantoin_100mg_bd_5d"] * number_of_courses

mapped_tests_treatment_costs[which(mapped_tests_treatment_costs$test_result == "lodestar_dx_targ" &
                                     mapped_tests_treatment_costs$patient_subgroup == "women"), "treatment_cost"] <-
  NA  # No data yet

mapped_tests_treatment_costs[which(mapped_tests_treatment_costs$test_result == "lodestar_dx_targ" &
                                     mapped_tests_treatment_costs$patient_subgroup == "mixed"), "treatment_cost"] <-
  NA # No data yet

mapped_tests_treatment_costs[which(mapped_tests_treatment_costs$test_result == "flexicult_human_targ" &
                                     mapped_tests_treatment_costs$patient_subgroup == "women"), "treatment_cost"] <-
  mean(c(mapped_treatment_costs["women", "nitrofurantoin_100mg_bd_3d"],
         # ampicillin - antibiotic course cost not yet calculated
         mapped_treatment_costs["women", "trimethoprim_200mg_bd_3d"]))

mapped_tests_treatment_costs[which(mapped_tests_treatment_costs$test_result == "flexicult_human_targ" &
                                     mapped_tests_treatment_costs$patient_subgroup == "mixed"), "treatment_cost"] <-
  mean(c(mapped_treatment_costs["mixed", "nitrofurantoin_100mg_bd_5d"],
         # ampicillin - antibiotic course cost not yet calculated
         mapped_treatment_costs["mixed", "trimethoprim_200mg_bd_5d"]))

mapped_tests_treatment_costs[which(mapped_tests_treatment_costs$test_result == "id_flexicult_targ" &
                                     mapped_tests_treatment_costs$patient_subgroup == "women"), "treatment_cost"] <-
  NA # No data yet

mapped_tests_treatment_costs[which(mapped_tests_treatment_costs$test_result == "id_flexicult_targ" &
                                     mapped_tests_treatment_costs$patient_subgroup == "mixed"), "treatment_cost"] <-
  NA # No data yet


# True probability of UTI (differs by patient subgroup)
# Matrix with one row per PSA sample and one column per patient subgroup
# Don't currently have data to differentiate between patient subgroups
p_uti <- matrix(NA, nrow = n_samples, ncol = n_patient_subgroups,
                dimnames = list(NULL, patient_subgroup_names))
p_uti[, "mixed"] <- rbeta(n = n_samples, shape1 = 2.212762, shape2 = 1.475174)
p_uti[, "women"] <- p_uti[, "mixed"]

# Probability of correctly detecting a UTI (sensitivity or true positive rate) (differs by test)
# Matrix with one row per PSA sample and one column per test
p_uti_tp <- matrix(NA, nrow = n_samples, ncol = n_tests,
                   dimnames = list(NULL, test_names))
# Lodestar data is confidential so not included here
p_uti_tp[, "lodestar_dx"] <- NA # confidential
# Normal distribution approximation is Normal(0.79, (0.85 - 0.72) / 3.92) = Normal(0.79, 0.033)
p_uti_tp[, "flexicult_human"] <- rnorm(n_samples, mean = 0.79, sd = 0.033)
# Normal distribution approximation is Normal(0.89, (0.93 - 0.84) / 3.92) = Normal(0.89, 0.023)
p_uti_tp[, "id_flexicult"] <- rnorm(n_samples, mean = 0.89, sd = 0.023)

# Probability of incorrectly diagnosing a non-UTI patient as having UTI and then giving them antibiotics (false positive rate) (differs by test)
# Matrix with one row per PSA sample and one column per test
p_uti_fp <- matrix(0, nrow = n_samples, ncol = n_tests,
                   dimnames = list(NULL, test_names))
# Lodestar data is confidential so not included here
p_uti_fp[, "lodestar_dx"] <- NA # confidential
# Normal distribution approximation is Normal(0.67, (0.90 - 0.30) / 3.92) = Normal(0.67, 0.153)
p_uti_fp[, "flexicult_human"] <- rnorm(n_samples, mean = 0.67, sd = 0.153)
# Normal distribution approximation is Normal(0.70, (0.84 - 0.52) / 3.92) = Normal(0.89, 0.082)
p_uti_fp[, "id_flexicult"] <- rnorm(n_samples, mean = 0.70, sd = 0.082)

# Probability of identifying specific antibiotic for targeted treatment, given that a UTI was detected using rapid POCT with AST or pathogenic cause (differs by test)
# Matrix with one row per PSA sample and one column per test
p_targ <- matrix(NA, nrow = n_samples, ncol = n_tests,
                 dimnames = list(NULL, test_names))


### Ensure that p_healthy_targ + p_sepsis_targ + p_kidney_failure_targ + p_pyelonephritis_targ = 1
### Ensure that p_healthy_emp + p_sepsis_emp + p_kidney_failure_emp + p_pyelonephritis_emp = 1
### Ensure that p_healthy_no_treatment + p_sepsis_no_treatment + p_kidney_failure_no_treatment + p_pyelonephritis_no_treatment = 1
### Ensure that p_healthy_emp_then_targ + p_sepsis_emp_then_targ + p_kidney_failure_emp_then_targ + p_pyelonephritis_emp_then_targ = 1

# Probability of "healthy" on targeted treatment
# Matrix with one row per PSA sample and one column per patient subgroup
p_healthy_targ <- matrix(NA, nrow = n_samples, ncol = n_patient_subgroups,
                         dimnames = list(NULL, patient_subgroup_names))

# Probability of sepsis on targeted treatment
# Matrix with one row per PSA sample and one column per patient subgroup
p_sepsis_targ <- matrix(NA, nrow = n_samples, ncol = n_patient_subgroups,
                        dimnames = list(NULL, patient_subgroup_names))

# Probability of kidney failure on targeted treatment
# Matrix with one row per PSA sample and one column per patient subgroup
p_kidney_failure_targ <- matrix(NA, nrow = n_samples, ncol = n_patient_subgroups,
                                dimnames = list(NULL, patient_subgroup_names))

# Probability of pyelonephritis on targeted treatment
# Matrix with one row per PSA sample and one column per patient subgroup
p_pyelonephritis_targ <- matrix(NA, nrow = n_samples, ncol = n_patient_subgroups,
                                dimnames = list(NULL, patient_subgroup_names))

# Probability of complications on targeted treatment
# Matrix with one row per PSA sample and one column per patient subgroup
p_complications_targ <- p_sepsis_targ + p_kidney_failure_targ + p_pyelonephritis_targ


# Probability of "healthy" on empiric treatment (varies by test as depends on bacteria test can detect)
# Table 20 gives 0.618 in women. Using this for all patient groups and tests.
# 3d array with dimensions for PSA samples, patient subgroups and tests
p_healthy_emp <- array(0.618, dim = c(n_samples, n_patient_subgroups, n_tests),
                       dimnames = list(NULL, patient_subgroup_names, test_names))

# Probability of sepsis on empiric treatment (varies by test as depends on bacteria test can detect)
# 3d array with dimensions for PSA samples, patient subgroups and tests
p_sepsis_emp <- array(NA, dim = c(n_samples, n_patient_subgroups, n_tests),
                      dimnames = list(NULL, patient_subgroup_names, test_names))

# Probability of kidney failure on empiric treatment (varies by test as depends on bacteria test can detect)
# 3d array with dimensions for PSA samples, patient subgroups and tests
p_kidney_failure_emp <- array(NA, dim = c(n_samples, n_patient_subgroups, n_tests),
                              dimnames = list(NULL, patient_subgroup_names, test_names))

# Probability of pyelonephritis on empiric treatment (varies by test as depends on bacteria test can detect)
# Table 20 gives 0.056 in women. Using this for all patient groups and tests.
# 3d array with dimensions for PSA samples, patient subgroups and tests
p_pyelonephritis_emp <- array(0.056, dim = c(n_samples, n_patient_subgroups, n_tests),
                              dimnames = list(NULL, patient_subgroup_names, test_names))

# Probability of complications on empiric treatment (varies by test as depends on bacteria test can detect)
# 3d array with dimensions for PSA samples, patient subgroups and tests
p_complications_emp <- p_sepsis_emp + p_kidney_failure_emp + p_pyelonephritis_emp


# Probability of "healthy" on "no treatment" (i.e. either recovered naturally from UTI or was given treatment for a true UTI despite no positive test result)
# Varies by test type as for culture testing patients may be given antibiotics while awaiting test results
# 3d array with dimensions for PSA samples, patient subgroups and test types
p_healthy_no_treatment <- array(NA, dim = c(n_samples, n_patient_subgroups, n_test_types),
                                dimnames = list(NULL, patient_subgroup_names, test_type_names))
p_healthy_no_treatment[, "women", ] <- 0.257
p_healthy_no_treatment[, "mixed", ] <- 0.2135

# Probability of sepsis on "no treatment"
# Varies by test type as for culture testing patients may be given antibiotics while awaiting test results
# 3d array with dimensions for PSA samples, patient subgroups and test types
p_sepsis_no_treatment <- array(NA, dim = c(n_samples, n_patient_subgroups, n_test_types),
                               dimnames = list(NULL, patient_subgroup_names, test_type_names))

# Probability of kidney failure on "no treatment"
# Varies by test type as for culture testing patients may be given antibiotics while awaiting test results
# 3d array with dimensions for PSA samples, patient subgroups and test types
p_kidney_failure_no_treatment <- array(NA, dim = c(n_samples, n_patient_subgroups, n_test_types),
                                       dimnames = list(NULL, patient_subgroup_names, test_type_names))

# Probability of pyelonephritis on "no treatment"
# Varies by test type as for culture testing patients may be given antibiotics while awaiting test results
# 3d array with dimensions for PSA samples, patient subgroups and test types
p_pyelonephritis_no_treatment <- array(NA, dim = c(n_samples, n_patient_subgroups, n_test_types),
                                       dimnames = list(NULL, patient_subgroup_names, test_type_names))
p_pyelonephritis_no_treatment[, "women", ] <- 0.663

# Probability of complications on no treatment
# Varies by test type as for culture testing patients may be given antibiotics while awaiting test results
# 3d array with dimensions for PSA samples, patient subgroups and test types
p_complications_no_treatment <- p_sepsis_no_treatment + p_kidney_failure_no_treatment + p_pyelonephritis_no_treatment


# Probability of "healthy" on empiric then targeted treatment
# Matrix with one row per PSA sample and one column per patient subgroup
p_healthy_emp_then_targ <- matrix(NA, nrow = n_samples, ncol = n_patient_subgroups,
                                  dimnames = list(NULL, patient_subgroup_names))

# Probability of sepsis on empiric then targeted treatment
# Matrix with one row per PSA sample and one column per patient subgroup
p_sepsis_emp_then_targ <- matrix(NA, nrow = n_samples, ncol = n_patient_subgroups,
                                 dimnames = list(NULL, patient_subgroup_names))

# Probability of kidney failure on empiric then targeted treatment
# Matrix with one row per PSA sample and one column per patient subgroup
p_kidney_failure_emp_then_targ <- matrix(NA, nrow = n_samples, ncol = n_patient_subgroups,
                                         dimnames = list(NULL, patient_subgroup_names))

# Probability of pyelonephritis on empiric then targeted treatment
# Matrix with one row per PSA sample and one column per patient subgroup
p_pyelonephritis_emp_then_targ <- matrix(NA, nrow = n_samples, ncol = n_patient_subgroups,
                                         dimnames = list(NULL, patient_subgroup_names))

# Probability of complications on empiric then targeted treatment
# Matrix with one row per PSA sample and one column per patient subgroup
p_complications_emp_then_targ <- p_sepsis_targ + p_kidney_failure_targ + p_pyelonephritis_targ


# Cost of follow-up GP appointment if antibiotic needs to be changed
cost_followup_appt <- 42

# Proportion of patients who are given antibiotics despite test not detecting a UTI
# This proportion is different for culture testing as patients may be given antibiotics while awaiting test results
# Matrix with one row per PSA sample and one column per test type
prop_emp_when_no_detected_uti <- matrix(NA, nrow = n_samples, ncol = n_tests,
                                        dimnames = list(NULL, test_type_names))

# Overall cost of test (varies by test)
# Lodestar data is confidential so not included here
# Matrix with one row per PSA sample and one column per test
cost_test <- matrix(NA, nrow = n_samples, ncol = n_tests,
                    dimnames = list(NULL, test_names))
cost_test[, "lodestar_dx"] <- NA # confidential
cost_test[, "flexicult_human"] <- 48
cost_test[, "id_flexicult"] <- NA  # confidential as estimated using Lodestar

# Cost of treating sepsis
# Vector with one element per PSA sample
cost_sepsis <- rep(NA, n_samples)

# Cost of treating kidney failure
# Vector with one element per PSA sample
cost_kidney_failure <- rep(NA, n_samples)

# Cost of treating pyelonephritis
# Vector with one element per PSA sample
cost_pyelonephritis <- rep(1221.26, n_samples)

# QALY loss from uncomplicated UTI
# QALD 0.68 (0.56 - 0.72). Need duration to calculate QALY.
# Vector with one element per PSA sample
qaly_loss_uti <- rep(NA, n_samples)

# Additional QALY loss from sepsis in the short-term model
# Vector with one element per PSA sample
qaly_loss_sepsis <- rep(NA, n_samples)

# Additional QALY loss from kidney failure in the short-term model
# Vector with one element per PSA sample
qaly_loss_kidney_failure <- rep(NA, n_samples)

# Additional QALY loss from pyelonephritis in the short-term model
# QALD 0.59 (0.48, 0.64). Duration data in Table 19.
# Vector with one element per PSA sample
qaly_loss_pyelonephritis <- rep(NA, n_samples)


# Probability of side effects from antibiotics
# mean = log(0.1) = -2.303     # sd = (log(0.3) - log(0.05)) / 3.92 = 0.457
# Vector with one element per PSA sample
p_side_effects_antibiotics <- rlnorm(n_samples, meanlog = -2.303, sdlog = 0.457)

# Duration of side effects from antibiotics
# Vector with one element per PSA sample
duration_side_effects_antibiotics <- rnorm(n_samples, mean = 3, sd = 0.5)

# QALY loss
qaly_loss_antibiotic_ae <- rep(NA, n_samples)
# If have data for QALD loss not QALY loss then convert to QALYs by multiplying by (duration_side_effects_antibiotics/365)

# QALY loss from antibiotic AE (adverse events) on targeted treatment
# Vector with one element per PSA sample
qaly_loss_antibiotic_ae_targ <- p_side_effects_antibiotics * qaly_loss_antibiotic_ae

# QALY loss from antibiotic AE (adverse events) on empiric treatment
# Assume QALY loss from antibiotics increases linearly with number of courses of antibiotics?
# Vector with one element per PSA sample
qaly_loss_antibiotic_ae_emp <- qaly_loss_antibiotic_ae_targ * number_of_courses

# QALY loss from antibiotic AE (adverse events) on empiric then targeted treatment
# Assume QALY loss from antibiotics increases linearly with number of courses of antibiotics?
# Vector with one element per PSA sample
qaly_loss_antibiotic_ae_emp_then_targ <- qaly_loss_antibiotic_ae_targ * 2


### Calculate probabilities

# p_healthy includes cured of UTI (naturally or with treatment), but not no UTI in first place since these patients are assumed to have another underlying condition
p_healthy <- array(NA, dim = c(n_samples, n_patient_subgroups, n_tests),   # defining empty array first
                   dimnames = list(NULL, patient_subgroup_names, test_names))

for (i_sample in 1:n_samples) {
  for (i_patient_subgroup in 1:n_patient_subgroups) {
    for (i_test in 1:n_tests) {
      if (mapped_tests[i_test, "test_type_names"] == "rapid_poct_with_ast_or_path") {
        
        p_healthy[i_sample, i_patient_subgroup, i_test] <-
          p_uti[i_sample, i_patient_subgroup] * p_uti_tp[i_sample, i_test] * p_targ[i_sample, i_test] * p_healthy_targ[i_sample, i_patient_subgroup] +
          p_uti[i_sample, i_patient_subgroup] * p_uti_tp[i_sample, i_test] * (1 - p_targ[i_sample, i_test]) * p_healthy_emp[i_sample, i_patient_subgroup, i_test] +
          p_uti[i_sample, i_patient_subgroup] * (1 - p_uti_tp[i_sample, i_test]) * p_healthy_no_treatment[i_sample, i_patient_subgroup, i_test]
        
      } else if (mapped_tests[i_test, "test_type_names"] %in% c("poct_with_culture", "lab_culture")) {
        
        p_healthy[i_sample, i_patient_subgroup, i_test] <-
          p_uti[i_sample, i_patient_subgroup] * p_uti_tp[i_sample, i_test] * p_healthy_emp_then_targ[i_sample, i_patient_subgroup] +
          p_uti[i_sample, i_patient_subgroup] * (1 - p_uti_tp[i_sample, i_test]) * p_healthy_no_treatment[i_sample, i_patient_subgroup, i_test]
        
      }
    }
  }
}

p_sepsis <- array(NA, dim = c(n_samples, n_patient_subgroups, n_tests),
                  dimnames = list(NULL, patient_subgroup_names, test_names))

for (i_sample in 1:n_samples) {
  for (i_patient_subgroup in 1:n_patient_subgroups) {
    for (i_test in 1:n_tests) {
      if (mapped_tests[i_test, "test_type_names"] == "rapid_poct_with_ast_or_path") {
        
        p_sepsis[i_sample, i_patient_subgroup, i_test] <-
          p_uti[i_sample, i_patient_subgroup] * p_uti_tp[i_sample, i_test] * p_targ[i_sample, i_test] * p_sepsis_targ[i_sample, i_patient_subgroup] +
          p_uti[i_sample, i_patient_subgroup] * p_uti_tp[i_sample, i_test] * (1 - p_targ[i_sample, i_test]) * p_sepsis_emp[i_sample, i_patient_subgroup, i_test] +
          p_uti[i_sample, i_patient_subgroup] * (1 - p_uti_tp[i_sample, i_test]) * p_sepsis_no_treatment[i_sample, i_patient_subgroup, i_test]
        
      } else if (mapped_tests[i_test, "test_type_names"] %in% c("poct_with_culture", "lab_culture")) {
        
        p_sepsis[i_sample, i_patient_subgroup, i_test] <-
          p_uti[i_sample, i_patient_subgroup] * p_uti_tp[i_sample, i_test] * p_sepsis_emp_then_targ[i_sample, i_patient_subgroup] +
          p_uti[i_sample, i_patient_subgroup] * (1 - p_uti_tp[i_sample, i_test]) * p_sepsis_no_treatment[i_sample, i_patient_subgroup, i_test]
        
      }
    }
  }
}

p_kidney_failure <- array(NA, dim = c(n_samples, n_patient_subgroups, n_tests),
                          dimnames = list(NULL, patient_subgroup_names, test_names))

for (i_sample in 1:n_samples) {
  for (i_patient_subgroup in 1:n_patient_subgroups) {
    for (i_test in 1:n_tests) {
      if (mapped_tests[i_test, "test_type_names"] == "rapid_poct_with_ast_or_path") {
        
        p_kidney_failure[i_sample, i_patient_subgroup, i_test] <-
          p_uti[i_sample, i_patient_subgroup] * p_uti_tp[i_sample, i_test] * p_targ[i_sample, i_test] * p_kidney_failure_targ[i_sample, i_patient_subgroup] +
          p_uti[i_sample, i_patient_subgroup] * p_uti_tp[i_sample, i_test] * (1 - p_targ[i_sample, i_test]) * p_kidney_failure_emp[i_sample, i_patient_subgroup, i_test] +
          p_uti[i_sample, i_patient_subgroup] * (1 - p_uti_tp[i_sample, i_test]) * p_kidney_failure_no_treatment[i_sample, i_patient_subgroup, i_test]
        
      } else if (mapped_tests[i_test, "test_type_names"] %in% c("poct_with_culture", "lab_culture")) {
        
        p_kidney_failure[i_sample, i_patient_subgroup, i_test] <-
          p_uti[i_sample, i_patient_subgroup] * p_uti_tp[i_sample, i_test] * p_kidney_failure_emp_then_targ[i_sample, i_patient_subgroup] +
          p_uti[i_sample, i_patient_subgroup] * (1 - p_uti_tp[i_sample, i_test]) * p_kidney_failure_no_treatment[i_sample, i_patient_subgroup, i_test]
        
      }
    }
  }
}

p_pyelonephritis <- array(NA, dim = c(n_samples, n_patient_subgroups, n_tests),
                          dimnames = list(NULL, patient_subgroup_names, test_names))

for (i_sample in 1:n_samples) {
  for (i_patient_subgroup in 1:n_patient_subgroups) {
    for (i_test in 1:n_tests) {
      if (mapped_tests[i_test, "test_type_names"] == "rapid_poct_with_ast_or_path") {
        
        p_pyelonephritis[i_sample, i_patient_subgroup, i_test] <-
          p_uti[i_sample, i_patient_subgroup] * p_uti_tp[i_sample, i_test] * p_targ[i_sample, i_test] * p_pyelonephritis_targ[i_sample, i_patient_subgroup] +
          p_uti[i_sample, i_patient_subgroup] * p_uti_tp[i_sample, i_test] * (1 - p_targ[i_sample, i_test]) * p_pyelonephritis_emp[i_sample, i_patient_subgroup, i_test] +
          p_uti[i_sample, i_patient_subgroup] * (1 - p_uti_tp[i_sample, i_test]) * p_pyelonephritis_no_treatment[i_sample, i_patient_subgroup, i_test]
        
      } else if (mapped_tests[i_test, "test_type_names"] %in% c("poct_with_culture", "lab_culture")) {
        
        p_pyelonephritis[i_sample, i_patient_subgroup, i_test] <-
          p_uti[i_sample, i_patient_subgroup] * p_uti_tp[i_sample, i_test] * p_pyelonephritis_emp_then_targ[i_sample, i_patient_subgroup] +
          p_uti[i_sample, i_patient_subgroup] * (1 - p_uti_tp[i_sample, i_test]) * p_pyelonephritis_no_treatment[i_sample, i_patient_subgroup, i_test]
        
      }
    }
  }
}

p_other_underlying_condition <- 1 - p_uti


### Calculate treatment costs

# cost_targ is the overall cost of the targeted treatment suitable for that patient group and test
# Assuming that if targeted treatment is given it works first time, therefore no need for additional courses of antibiotics
# Different test results will identify different targeted antibiotics, so cost_targ is averaged over these different targeted antibiotic costs
# Assuming equal split across antibiotic sensitivities
cost_targ <- array(NA, dim = c(n_samples, n_patient_subgroups, n_tests),   # defining empty array first
                   dimnames = list(NULL, patient_subgroup_names, test_names))
for (i_sample in 1:n_samples) {
  for (i_patient_subgroup in 1:n_patient_subgroups) {
    for (i_test in 1:n_tests) {
      cost_targ[i_sample, i_patient_subgroup, i_test] <-
        mapped_tests_treatment_costs[which(mapped_tests_treatment_costs[, "patient_subgroup"] == patient_subgroup_names[i_patient_subgroup] & 
                                             mapped_tests_treatment_costs[, "test_result"] == paste0(test_names[i_test], "_targ")),
                                     "treatment_cost"]
    }
  }
}

# cost_emp is the overall cost of the empiric treatment suitable for that patient group
# Assuming that it doesn't depend on which test was used
cost_emp <- matrix(NA, nrow = n_samples, ncol = n_patient_subgroups)   # defining empty matrix first
colnames(cost_emp) <- patient_subgroup_names

for (i_sample in 1:n_samples) {
  for (i_patient_subgroup in 1:n_patient_subgroups) {
    cost_emp[i_sample, i_patient_subgroup] <-
      mapped_tests_treatment_costs[which(mapped_tests_treatment_costs[, "patient_subgroup"] == patient_subgroup_names[i_patient_subgroup] & 
                                           mapped_tests_treatment_costs[, "test_result"] == "empiric"),
                                   "treatment_cost"]
  }
}

# cost_emp_then_targ is the overall cost of the empiric then targeted treatment suitable for that patient group
# Assuming that it doesn't depend on which particular test was used (though is only used for "poct_with_culture" and "lab_culture" test types)
cost_emp_then_targ <- matrix(NA, nrow = n_samples, ncol = n_patient_subgroups)   # defining empty matrix first
colnames(cost_emp_then_targ) <- patient_subgroup_names
# Could (conservatively) assume that this includes the sum of the cost of a full course of empiric and a full course of targeted
# Also includes the cost of follow-up GP appointment if antibiotic needs to be changed (cost_followup_appt)


### Calculate overall costs

# 3d array with dimensions for PSA samples, patient subgroups and tests
costs <- array(NA, dim = c(n_samples, n_patient_subgroups, n_tests),   # defining empty array first
               dimnames = list(NULL, patient_subgroup_names, test_names))

for (i_sample in 1:n_samples) {
  for (i_patient_subgroup in 1:n_patient_subgroups) {
    for (i_test in 1:n_tests) {
      if (mapped_tests[i_test, "test_type_names"] == "rapid_poct_with_ast_or_path") {
        
        costs[i_sample, i_patient_subgroup, i_test] <-
          
          cost_test[i_sample, i_test] +
          
          ((p_uti[i_sample, i_patient_subgroup] * p_uti_tp[i_sample, i_test] * p_targ[i_sample, i_test]) +
             ((1 - p_uti[i_sample, i_patient_subgroup]) * p_uti_fp[i_sample, i_test] * p_targ[i_sample, i_test])) *
          cost_targ[i_sample, i_patient_subgroup, i_test] +
          
          ((p_uti[i_sample, i_patient_subgroup] * p_uti_tp[i_sample, i_test] * (1 - p_targ[i_sample, i_test])) +
             (p_uti[i_sample, i_patient_subgroup] * (1 - p_uti_tp[i_sample, i_test]) * prop_emp_when_no_detected_uti[i_sample, mapped_tests[i_test, "test_type_names"]]) +
             ((1 - p_uti[i_sample, i_patient_subgroup]) * p_uti_fp[i_sample, i_test] * (1 - p_targ[i_sample, i_test])) +
             ((1 - p_uti[i_sample, i_patient_subgroup]) * (1 - p_uti_fp[i_sample, i_test]) * prop_emp_when_no_detected_uti[i_sample, mapped_tests[i_test, "test_type_names"]])) *
          cost_emp[i_sample, i_patient_subgroup] +
          
          p_sepsis[i_sample, i_patient_subgroup, i_test] * cost_sepsis[i_sample] +
          p_kidney_failure[i_sample, i_patient_subgroup, i_test] * cost_kidney_failure[i_sample] +
          p_pyelonephritis[i_sample, i_patient_subgroup, i_test] * cost_pyelonephritis[i_sample]
        
      } else if (mapped_tests[i_test, "test_type_names"] %in% c("poct_with_culture", "lab_culture")) {
        
        costs[i_sample, i_patient_subgroup, i_test] <-
          
          cost_test[i_sample, i_test] +
          
          ((p_uti[i_sample, i_patient_subgroup] * p_uti_tp[i_sample, i_test]) +
             ((1 - p_uti[i_sample, i_patient_subgroup]) * p_uti_fp[i_sample, i_test])) *
          cost_emp_then_targ[i_sample, i_patient_subgroup] +
          
          ((p_uti[i_sample, i_patient_subgroup] * (1 - p_uti_tp[i_sample, i_test]) * prop_emp_when_no_detected_uti[i_sample, mapped_tests[i_test, "test_type_names"]]) +
             ((1 - p_uti[i_sample, i_patient_subgroup]) * (1 - p_uti_fp[i_sample, i_test]) * prop_emp_when_no_detected_uti[i_sample, mapped_tests[i_test, "test_type_names"]])) *
          cost_emp[i_sample, i_patient_subgroup] +
          
          p_sepsis[i_sample, i_patient_subgroup, i_test] * cost_sepsis[i_sample] +
          p_kidney_failure[i_sample, i_patient_subgroup, i_test] * cost_kidney_failure[i_sample] +
          p_pyelonephritis[i_sample, i_patient_subgroup, i_test] * cost_pyelonephritis[i_sample]
      }
    }
  }
}

### Calculate overall QALYs

# 3d array with dimensions for PSA samples, patient subgroups and tests
qalys <- array(NA, dim = c(n_samples, n_patient_subgroups, n_tests),   # defining empty array first
               dimnames = list(NULL, patient_subgroup_names, test_names))

for (i_sample in 1:n_samples) {
  for (i_patient_subgroup in 1:n_patient_subgroups) {
    for (i_test in 1:n_tests) {
      if (mapped_tests[i_test, "test_type_names"] == "rapid_poct_with_ast_or_path") {
        
        qalys[i_sample, i_patient_subgroup, i_test] <-
          
          p_healthy[i_sample, i_patient_subgroup, i_test] * qaly_loss_uti[i_sample] +
          p_sepsis[i_sample, i_patient_subgroup, i_test] * qaly_loss_sepsis[i_sample] +
          p_kidney_failure[i_sample, i_patient_subgroup, i_test] * qaly_loss_kidney_failure[i_sample] +
          p_pyelonephritis[i_sample, i_patient_subgroup, i_test] * qaly_loss_pyelonephritis[i_sample] +
          
          ((p_uti[i_sample, i_patient_subgroup] * p_uti_tp[i_sample, i_test] * p_targ[i_sample, i_test]) +
             ((1 - p_uti[i_sample, i_patient_subgroup]) * p_uti_fp[i_sample, i_test] * p_targ[i_sample, i_test])) *
          qaly_loss_antibiotic_ae_targ[i_sample] +
          
          ((p_uti[i_sample, i_patient_subgroup] * p_uti_tp[i_sample, i_test] * (1 - p_targ[i_sample, i_test])) +
             (p_uti[i_sample, i_patient_subgroup] * (1 - p_uti_tp[i_sample, i_test]) * prop_emp_when_no_detected_uti[i_sample, mapped_tests[i_test, "test_type_names"]]) +
             ((1 - p_uti[i_sample, i_patient_subgroup]) * p_uti_fp[i_sample, i_test] * (1 - p_targ[i_sample, i_test])) +
             ((1 - p_uti[i_sample, i_patient_subgroup]) * (1 - p_uti_fp[i_sample, i_test]) * prop_emp_when_no_detected_uti[i_sample, mapped_tests[i_test, "test_type_names"]])) *
          qaly_loss_antibiotic_ae_emp[i_sample]
        
      } else if (mapped_tests[i_test, "test_type_names"] %in% c("poct_with_culture", "lab_culture")) {
        
        qalys[i_sample, i_patient_subgroup, i_test] <-
          
          p_healthy[i_sample, i_patient_subgroup, i_test] * qaly_loss_uti[i_sample] +
          p_sepsis[i_sample, i_patient_subgroup, i_test] * qaly_loss_sepsis[i_sample] +
          p_kidney_failure[i_sample, i_patient_subgroup, i_test] * qaly_loss_kidney_failure[i_sample] +
          p_pyelonephritis[i_sample, i_patient_subgroup, i_test] * qaly_loss_pyelonephritis[i_sample] +
          
          ((p_uti[i_sample, i_patient_subgroup] * p_uti_tp[i_sample, i_test]) +
             ((1 - p_uti[i_sample, i_patient_subgroup]) * p_uti_fp[i_sample, i_test])) *
          qaly_loss_antibiotic_ae_emp_then_targ[i_sample] +
          
          ((p_uti[i_sample, i_patient_subgroup] * (1 - p_uti_tp[i_sample, i_test]) * prop_emp_when_no_detected_uti[i_sample, mapped_tests[i_test, "test_type_names"]]) +
             ((1 - p_uti[i_sample, i_patient_subgroup]) * (1 - p_uti_fp[i_sample, i_test]) * prop_emp_when_no_detected_uti[i_sample, mapped_tests[i_test, "test_type_names"]])) *
          qaly_loss_antibiotic_ae_emp[i_sample]
      }
    }
  }
}

### Use BCEA R package to generate total and incremental costs, QALYs, and net benefit at both £20k/QALY and £30k/QALY, and plot CEACs and CE-plane

# Load BCEA library
# If not installed use the following line first
# install.packages("BCEA)
library(BCEA)

# Average costs (averaging over PSA samples)
average_costs <- apply(costs, c(2, 3), mean)

# Average QALYs (averaging over PSA samples)
average_qalys <- apply(qalys, c(2, 3), mean)

# Incremental costs and effects (relative to lodestar_dx, but could choose any test)
incremental_costs <- array(NA, dim = c(n_samples, n_patient_subgroups, n_tests),
                           dimnames = list(NULL, patient_subgroup_names, test_names))
incremental_qalys <- array(NA, dim = c(n_samples, n_patient_subgroups, n_tests),
                           dimnames = list(NULL, patient_subgroup_names, test_names))
for (i_test in 1:n_tests) {
  incremental_costs[, , i_test] <- costs[, , i_test] - costs[, , "lodestar_dx"]
  incremental_qalys[, , i_test] <- qalys[, , i_test] - qalys[, , "lodestar_dx"]
}

# Incremental net benefit at willingness-to-pay threshold of £20k/QALY
incremental_net_benefit_wtp20k <- 20000 * incremental_qalys - incremental_costs
# Average incremental net benefit at willingness-to-pay threshold of £20k/QALY
average_inb_wtp20k <- apply(incremental_net_benefit_wtp20k, c(2, 3), mean)

# Incremental net benefit at willingness-to-pay threshold of £30k/QALY
incremental_net_benefit_wtp30k <- 30000 * incremental_qalys - incremental_costs
# Average incremental net benefit at willingness-to-pay threshold of £20k/QALY
average_inb_wtp30k <- apply(incremental_net_benefit_wtp30k, c(2, 3), mean)

# Use the BCEA package to plot CEACs and CE plane

# Results for mixed population (relative to lodestar_dx, but could choose any test)
# The "ref" parameter is the index of the test selected from our test_names vector - so ref = 1 is lodestar_dx
bcea_object_mixed <- bcea(eff = qalys[, "mixed", ], cost = costs[, "mixed", ], ref = 1, interventions = test_names) 
# Get summary statistics
summary(bcea_object_mixed, wtp = 20000)
# Plot the cost-effectiveness plane
ceplane.plot(bcea_object_mixed, wtp = 20000)
# Plot a CEAC
bcea_mixed_multi_ce <- multi.ce(bcea_object_mixed)
ceac.plot(bcea_mixed_multi_ce, pos = c(1,0))

# Results for women (relative to lodestar_dx, but could choose any test)
# The "ref" parameter is the index of the test selected from our test_names vector - so ref = 1 is lodestar_dx
bcea_object_women <- bcea(eff = qalys[, "women", ], cost = costs[, "women", ], ref = 1, interventions = test_names) 
# Get summary statistics
summary(bcea_object_women, wtp = 20000)
# Plot the cost-effectiveness plane
ceplane.plot(bcea_object_women, wtp = 20000)
# Plot a CEAC
bcea_women_multi_ce <- multi.ce(bcea_object_women)
ceac.plot(bcea_women_multi_ce, pos = c(1,0))
