#*******************************************************************************
#### 0. LIBRARIES ####
#*******************************************************************************

library(readxl)
library(dplyr)
library(tidyr)
library(MASS)      
library(broom)    
library(yardstick) 
library(pscl)     
library(ggplot2)
library(purrr)
library(tibble)
library(ggseg)
library(forcats)
library(nnet)
library(stringr)
library(flextable)
library(officer)
library(car)
library(DescTools)
library(pROC)
library(rsample)
library(logistf)



#*******************************************************************************
#### 1. INPUT ####
#*******************************************************************************

data_social  <- readxl::read_excel("~/Desktop/Brain-Health/data/MODELO_1_LIMPIO.xlsx")
data_volume  <- readxl::read_excel("~/Desktop/Brain-Health/data/Volumenes estructurales por grupos de SC.xlsx")
data_network <- readxl::read_excel("~/Desktop/Brain-Health/data/Conectividad_redes_sujetos.xlsx")
dictionary   <- readxl::read_excel("~/Desktop/Brain-Health/data/diccionario socio economico.xlsx")



#*******************************************************************************
#### 2. DEPURATION ####
#*******************************************************************************

# Ensure record_id is in uppercase and merge all social/volume/network data
colnames(data_network)[1] <- "record_id"  # rename first column in data_network

data <- data_social %>%
  # Standardize key
  dplyr::mutate(record_id = toupper(record_id)) %>%
  # Merge selected columns from data_volume
  dplyr::left_join(
    data_volume %>% dplyr::select(record_id, 6:124),
    by = "record_id"
  ) %>%
  # Merge selected columns from data_network
  dplyr::left_join(
    data_network %>% dplyr::select(record_id, 6:15),
    by = "record_id"
  )

# Keep only 'data' and 'dictionary' in the environment
rm(list = setdiff(ls(), c("data", "dictionary")))

# Harmonize the 'Type' column in dictionary
dictionary <- dictionary %>%
  dplyr::mutate(
    # Lowercase for easier pattern matching
    Type = tolower(Type),
    # Map to consistent English labels
    Type = dplyr::case_when(
      grepl("num",      Type) ~ "Numerical",
      grepl("númerica", Type) ~ "Numerical",
      grepl("ordinal",  Type) ~ "Categorical ordinal",
      grepl("nominal",  Type) ~ "Categorical nominal",
      TRUE                    ~ Type
    ),
    # Keep original variable name for auditing
    Variable_original = Variable,
    # Manual one-by-one fixes to match colnames(data)
    Variable = dplyr::recode(
      Variable,
      "[msoc_20rev] work_nigth"            = "work_nigth",
      "[msoc_bas_0]"                       = "msoc_bas",
      "Education_livingston"               = "education_livingston",
      "TEC_livingston"                     = "tec_livingston",
      "Actividad_fisic__livingston"        = "actividad_fisic_livingston",
      "Aislamiento_social_livingston"      = "aislamiento_social_livingston",
      "Alcohol_livingston"                 = "alcohol_livingston",
      "Livingston"                         = "livingston",
      "Fragiles"                           = "fragiles",
      "Prefragiles"                        = "prefragiles",
      "Robustos"                           = "robustos",
      "pais_nacim [demo_place]"            = "pais_nacim",
      "pais_residen [demo_resid]"          = "pais_residen",
      "education_year [demo_sp_education]" = "education_year"
    )
  )

# Optional sanity checks (run interactively if needed)
setdiff(unique(dictionary$Variable), colnames(data))      # dictionary vars not in data
setdiff(colnames(data), unique(dictionary$Variable))      # data vars not in dictionary

# Mark specific rows as Binomial and one as Categorical ordinal
dictionary <- dictionary %>%
  dplyr::mutate(
    Type = ifelse(
      dplyr::row_number() %in% c(5, 6, 9:12, 17:18, 23:24, 26:39),
      "Binomial",
      Type
    )
  )

dictionary[16, "Type"] <- "Categorical ordinal"

# Add the names of the variables in Dictionary
dictionary <- dictionary %>%
  dplyr::mutate(
    Label_en = dplyr::recode(
      Variable,
      "uls_total"                          = "UCLA loneliness score",
      "msoc_contact"                       = "Social contact frequency",
      "social_index"                       = "Social support index",
      "aislamiento_score"                  = "Social isolation index",
      "work_nigth"                         = "History of night-shift work",
      "msoc_bas"                           = "Difficulty meeting basic needs",
      "msoc_fin"                           = "Current household finances",
      "msoc_fin_rec"                       = "Household finances (12 months)",
      "msoc_care"                          = "Difficulty paying medical care",
      "msoc_91rev"                         = "Unmet medical care need",
      "msoc_eat"                           = "Food insufficiency (eating less)",
      "msoc_bal"                           = "Food insecurity (unbalanced diet)",
      "msoc_ladder_35"                     = "Subjective social status ladder",
      "indice_econosocial"                 = "Socioeconomic index",
      "indice_econosocial_0_10"            = "Socioeconomic index (0–10)",
      "categoria_econosocial"              = "Socioeconomic category",
      "msoc_att"                           = "Physical assault with weapon",
      "msoc_humi"                          = "Humiliation / emotional abuse",
      "indice_violencia"                   = "Violence exposure index",
      "z_violencia_score"                  = "Violence exposure z-score",
      "violencia_score_0_10_r"             = "Violence exposure (0–10)",
      "totalsocial_score"                  = "Total violence exposure score",
      "education_livingston"               = "Education (Livingston)",
      "audit_livingston"                   = "Hearing adequacy",
      "visual_livingston"                  = "Vision adequacy",
      "tec_livingston"                     = "History of head trauma",
      "actividad_fisic_livingston"         = "Physical activity",
      "hipertension_livingston"            = "Hypertension",
      "diabetes_livingston"                = "Diabetes",
      "obesidad_livingston"                = "Obesity",
      "demo_smoke_livingston"              = "Smoking (yes/no)",
      "aislamiento_social_livingston"      = "Social isolation (yes/no)",
      "depression_livingston"              = "Depression",
      "alcohol_livingston"                 = "Alcohol use",
      "dislipidemia_colesterol_livingston" = "Hypercholesterolemia",
      "livingston"                         = "Total Livingston score",
      "fragiles"                           = "Frail",
      "prefragiles"                        = "Pre-frail",
      "robustos"                           = "Robust",
      "gad_total"                          = "Anxiety score (GAD)",
      "gds_total"                          = "Depression score (GDS)",
      "pais_nacim"                         = "Country of birth",
      "pais_residen"                       = "Country of residence",
      "demo_sex"                           = "Sex assigned at birth",
      "education_year"                     = "Years of education",
      "demo_age"                           = "Age"
    )
  )

# Recode and clean variables in 'data'
data <- data %>%
  # Simple numeric replacement before categorical recoding
  dplyr::mutate(
    # Set value 3 as missing in msoc_humi
    msoc_humi = ifelse(msoc_humi == 3, NA, msoc_humi),
    # In audit_livingston, recode 2 -> 1 (still numeric at this stage)
    audit_livingston = ifelse(audit_livingston == 2, 1, audit_livingston)
  ) %>%
  # Categorical recoding using across()
  dplyr::mutate(
    # 1/2 -> Yes/No in selected columns (by index)
    dplyr::across(c(13, 18, 19, 20, 25, 26),
                  ~ dplyr::recode(., `1` = "Yes", `2` = "No")),
    
    # audit_livingston: 0/1 -> Yes/No
    dplyr::across(audit_livingston,
                  ~ dplyr::recode(., `0` = "Yes", `1` = "No")),
    
    # Selected columns: 1/0 -> Yes/No
    dplyr::across(c(17, 34:43),
                  ~ dplyr::recode(., `1` = "Yes", `0` = "No")),
    
    # Frailty-related variables
    dplyr::across(fragiles,
                  ~ dplyr::recode(., `1` = ">3", `0` = "<3")),
    dplyr::across(prefragiles,
                  ~ dplyr::recode(., `1` = "1-2", `0` = ">3")),
    dplyr::across(robustos,
                  ~ dplyr::recode(., `1` = "0",  `0` = "1-3")),
    
    # Sex: 1/2 -> Female/Male
    dplyr::across(demo_sex,
                  ~ dplyr::recode(., `1` = "Female", `2` = "Male")),
    
    # Social contact frequency
    dplyr::across(msoc_contact,
                  ~ dplyr::recode(., `1` = "<1t/w",
                                  `2` = "1-2t/w",
                                  `3` = "3-5t/w",
                                  `4` = ">5t/w")),
    
    # Financial situation variables
    dplyr::across(c(msoc_fin, msoc_fin_rec),
                  ~ dplyr::recode(.,
                                  `1` = "Had extra money to save",
                                  `2` = "Had enough to cover necessities",
                                  `3` = "Did not have enough to cover necessities")),
    
    # Visual status
    dplyr::across(visual_livingston,
                  ~ dplyr::recode(.,
                                  `0` = "Yes",
                                  `1` = "No, but adequate with prescription glasses",
                                  `2` = "Cannot see")),
    
    # Education: years threshold
    dplyr::across(education_livingston,
                  ~ dplyr::recode(.,
                                  `1` = ">8 years",
                                  `0` = "<8 years")),
    
    # Country of birth / residence
    dplyr::across(c(pais_nacim, pais_residen),
                  ~ dplyr::recode(.,
                                  `1` = "Argentina",
                                  `2` = "Brasil",
                                  `3` = "Chile",
                                  `4` = "Colombia",
                                  `5` = "Mexico",
                                  `6` = "Peru",
                                  `7` = "Estados Unidos",
                                  `8` = "Otro")),
    
    dplyr::across(msoc_bas,
                  ~ dplyr::recode(.,
                                  `0` = "No",
                                  `1` = "Yes"))
  )



#*******************************************************************************
#### 3. BINARY LOGISTIC REG. MODELS FOR BRAIN HEALTH (OBH vs GBH) ####
#*******************************************************************************
# Goal:
#   Outcome = General Brain Health (GBH) vs Optimal Brain Health (OBH)
#   Estimate ORs (and 95% CIs) for predictors of being *GBH* (vs OBH).
#
# Interpretation (given the outcome coding below):
#   OR > 1  => higher odds of being GBH (worse brain health)
#   OR < 1  => lower odds of being GBH (i.e., relatively more OBH)
#
# Notes:
# - We achieve this by creating a 2-level outcome restricted to OBH/GBH.
#   Any other group (e.g., BHD) is set to NA and dropped by na.omit.
#*******************************************************************************

# Use treatment contrasts also for ordered factors (prevents polynomial contrasts)
# so factor terms behave as classic dummy indicators unless we explicitly create trends.
options(contrasts = c("contr.treatment", "contr.treatment"))

#*******************
##### 3.1. Create dataframes by type (from dictionary) #####
#*******************
Numerical            <- dictionary[dictionary$Type == "Numerical",            "Column", drop = FALSE]
Categorical.nominal  <- dictionary[dictionary$Type == "Categorical nominal",  "Column", drop = FALSE]
Categorical.ordinal  <- dictionary[dictionary$Type == "Categorical ordinal",  "Column", drop = FALSE]
Binomial             <- dictionary[dictionary$Type == "Binomial",             "Column", drop = FALSE]

# Helper to robustly extract column names from dictionary
# (works whether dictionary$Column stores indices or names)
get_cols_from_dictionary <- function(dic_sub, df) {
  cols <- dic_sub$Column
  if (is.numeric(cols)) {
    return(names(df)[cols])
  } else {
    return(as.character(cols))
  }
}

# Copy 'data' into 'df' so the original object remains intact
df <- data

#*******************
###### 3.1.2. Define BINARY outcome: OBH vs GBH (EVENT = GBH) ######
#*******************
# IMPORTANT: glm with a 2-level factor models the log-odds of the SECOND level.
# Here we set levels = c("OBH","GBH") so the modeled event is "GBH".
# Any non-OBH/GBH groups (e.g., BHD) are set to NA and dropped by na.omit.
df$obh_gbh_bin <- factor(
  dplyr::case_when(
    df$tipo_grupo == "OBH" ~ "OBH",
    df$tipo_grupo == "GBH" ~ "GBH",
    TRUE                   ~ NA_character_
  ),
  levels = c("OBH", "GBH")   # <<<<<< CAMBIO CLAVE: segundo nivel = GBH (evento)
)

# Convenience function for ordered factors
ord_factor <- function(x, lev) factor(x, levels = lev, ordered = TRUE)

#*******************
###### 3.1.3. Numerical and nominal variables ######
#*******************
# Numerical columns based on dictionary
num_cols <- get_cols_from_dictionary(Numerical, df)

# Remove 'categoria_econosocial' from numerical set (it will be treated as ordinal)
num_cols <- setdiff(num_cols, "categoria_econosocial")

# Convert numerical columns to numeric (handles character/factor safely)
df[num_cols] <- lapply(df[num_cols], function(x) as.numeric(as.character(x)))

# Categorical nominal variables (from dictionary)
nom_cols <- get_cols_from_dictionary(Categorical.nominal, df)
df[nom_cols] <- lapply(df[nom_cols], as.factor)

# Ensure 'livingston' is numeric (if present)
if ("livingston" %in% names(df)) {
  df$livingston <- as.numeric(as.character(df$livingston))
}

#*******************
###### 3.1.4. Ordinal categorical variables (explicit level order) ######
#*******************
# Social contact frequency
if ("msoc_contact" %in% names(df)) {
  df$msoc_contact <- ord_factor(df$msoc_contact, c("<1t/w", "1-2t/w", "3-5t/w", ">5t/w"))
}

# Financial situation
fin_levels <- c(
  "Did not have enough to cover necessities",
  "Had enough to cover necessities",
  "Had extra money to save"
)

if ("msoc_fin" %in% names(df))      df$msoc_fin      <- ord_factor(df$msoc_fin,      fin_levels)
if ("msoc_fin_rec" %in% names(df))  df$msoc_fin_rec  <- ord_factor(df$msoc_fin_rec,  fin_levels)

# Visual status
if ("visual_livingston" %in% names(df)) {
  df$visual_livingston <- ord_factor(
    df$visual_livingston,
    c("Cannot see", "No, but adequate with prescription glasses", "Yes")
  )
}

# Socioeconomic category
if ("categoria_econosocial" %in% names(df)) {
  df$categoria_econosocial <- ord_factor(df$categoria_econosocial, c("Bajo", "Medio", "Alto"))
}

#*******************
###### 3.1.5. Binary variables (Yes/No and ordered binaries) ######
#*******************
bin_yes_no <- c(
  "work_nigth",
  "msoc_care",
  "msoc_91rev",
  "msoc_eat",
  "msoc_bal",
  "msoc_att",
  "msoc_humi",
  "audit_livingston",
  "actividad_fisic_livingston",
  "hipertension_livingston",
  "diabetes_livingston",
  "obesidad_livingston",
  "demo_smoke_livingston",
  "aislamiento_social_livingston",
  "depression_livingston",
  "alcohol_livingston",
  "dislipidemia_colesterol_livingston",
  "msoc_bas"
)

# Only apply to variables that actually exist in df
bin_yes_no <- intersect(bin_yes_no, names(df))

# Reference category is "No" for all binary predictors
df[bin_yes_no] <- lapply(
  df[bin_yes_no],
  function(x) factor(x, levels = c("No", "Yes"))
)

# Education (years threshold)
if ("education_livingston" %in% names(df)) {
  df$education_livingston <- ord_factor(df$education_livingston, c("<8 years", ">8 years"))
}

# Frailty-related variables
if ("fragiles" %in% names(df))     df$fragiles     <- ord_factor(df$fragiles,     c("<3", ">3"))
if ("prefragiles" %in% names(df))  df$prefragiles  <- ord_factor(df$prefragiles,  c("1-2", ">3"))
if ("robustos" %in% names(df))     df$robustos     <- ord_factor(df$robustos,     c("0", "1-3"))

# Variable with only "No": remove if present (no variability)
if ("tec_livingston" %in% names(df)) {
  df$tec_livingston <- NULL
}

#*******************
##### 3.2. Predictor selection and FULL multivariable model (glm) #####
#*******************
# Candidate predictors: columns 4 to 48 (as in your original script)
# (safeguard in case df has fewer columns)
idx_end  <- min(48, ncol(df))
idx_from <- min(4, idx_end)
all_preds <- names(df)[idx_from:idx_end]

# Drop outcome-related columns if they are in the candidate range
all_preds <- setdiff(all_preds, c("tipo_grupo", "obh_gbh_bin", "tipo_grupo_ord"))

# Keep only predictors with >1 unique non-missing value
preds_final <- all_preds[
  sapply(df[all_preds], function(x) length(unique(na.omit(x))) > 1)
]

# Build formula: GBH (vs OBH) ~ all predictors with variability
form_str  <- paste("obh_gbh_bin ~", paste(preds_final, collapse = " + "))
form_full <- as.formula(form_str)

fit_bin_full <- glm(
  formula   = form_full,
  data      = df,
  family    = binomial(link = "logit"),
  na.action = na.omit
)

#*******************
##### 3.3. Coefficients, ORs and confidence intervals (FULL model) ####
#*******************
# NOTE:
# - ORs from fit_bin_full are now for the odds of being "GBH" (vs "OBH"),
#   because GBH is the SECOND level in df$obh_gbh_bin.

#*******************
##### 3.4. Settings for individual models (unadjusted and adjusted) #####
#*******************
adjust_vars <- c("demo_age", "education_year")  # <<< MODIFY THIS VECTOR ONLY TO CHANGE ADJUSTERS
adjust_vars <- intersect(adjust_vars, names(df)) # safeguard

# Ordinal predictors with >2 levels to model as a *linear trend* (1,2,3,...)
ordinal_trend_vars <- c(
  "msoc_contact",
  "msoc_fin",
  "msoc_fin_rec",
  "visual_livingston",
  "categoria_econosocial"
)
ordinal_trend_vars <- intersect(ordinal_trend_vars, names(df)) # safeguard

main_predictors <- setdiff(preds_final, adjust_vars)

#*******************
###### 3.4.1. Helper function to fit a single BINARY logistic model ######
#*******************
fit_single_glm <- function(
    exposure,
    adjust = NULL,
    data,
    outcome = "obh_gbh_bin",
    ordinal_trend_vars = NULL
) {
  exposure <- as.character(exposure)[1]
  
  data_model <- data
  rhs_terms  <- c(exposure, adjust)
  
  # Ordinal trend -> numeric score 1,2,3... by factor order
  if (!is.null(ordinal_trend_vars) && exposure %in% ordinal_trend_vars) {
    trend_name <- paste0(exposure, "_trend")
    data_model[[trend_name]] <- as.numeric(data_model[[exposure]])
    rhs_terms[rhs_terms == exposure] <- trend_name
  } else {
    trend_name <- NULL
  }
  
  form_str  <- paste(outcome, "~", paste(rhs_terms, collapse = " + "))
  form_full <- as.formula(form_str)
  
  model <- glm(
    formula   = form_full,
    data      = data_model,
    family    = binomial(link = "logit"),
    na.action = na.omit
  )
  
  aic_val <- AIC(model)
  bic_val <- BIC(model)
  
  # McFadden pseudo-R2: 1 - (LL_model / LL_null)
  r2_mcfadden <- tryCatch({
    null_model <- update(model, . ~ 1)
    1 - (as.numeric(logLik(model)) / as.numeric(logLik(null_model)))
  }, error = function(e) NA_real_)
  
  coefs <- summary(model)$coefficients
  
  res <- data.frame(
    term      = rownames(coefs),
    estimate  = coefs[, "Estimate"],
    std.error = coefs[, "Std. Error"],
    z         = coefs[, "z value"],
    row.names = NULL
  ) %>%
    dplyr::mutate(
      p_value    = 2 * pnorm(abs(z), lower.tail = FALSE),
      OR         = exp(estimate),
      OR_low95   = exp(estimate - 1.96 * std.error),
      OR_high95  = exp(estimate + 1.96 * std.error),
      exposure   = exposure,
      adjustment = ifelse(
        is.null(adjust) || length(adjust) == 0,
        "none",
        paste(adjust, collapse = " + ")
      ),
      R2         = r2_mcfadden,
      AIC        = aic_val,
      BIC        = bic_val
    ) %>%
    dplyr::filter(term != "(Intercept)")
  
  # Keep only exposure term(s)
  if (!is.null(trend_name)) {
    res <- res %>%
      dplyr::filter(term == trend_name) %>%
      dplyr::mutate(term = exposure)
  } else {
    res <- res %>%
      dplyr::filter(grepl(paste0("^", exposure), term))
  }
  
  res %>%
    dplyr::select(
      exposure,
      adjustment,
      term,
      OR,
      OR_low95,
      OR_high95,
      estimate,
      std.error,
      z,
      p_value,
      R2,
      AIC,
      BIC
    )
}

#*******************
###### 3.4.2. Run ALL unadjusted models (one per predictor) ######
#*******************
# Interpretation:
# - ORs are for odds of GBH vs OBH.
# - OR > 1 indicates higher odds of GBH (relative to OBH).
results <- purrr::map_dfr(
  main_predictors,
  ~ fit_single_glm(
    exposure           = .x,
    adjust             = NULL,
    data               = df,
    ordinal_trend_vars = ordinal_trend_vars
  )
)

#*******************
###### 3.4.3. Run ALL adjusted models (one per predictor, adjusted by chosen covariates) ######
#*******************
results_adjusted <- purrr::map_dfr(
  main_predictors,
  ~ fit_single_glm(
    exposure           = .x,
    adjust             = adjust_vars,
    data               = df,
    ordinal_trend_vars = ordinal_trend_vars
  )
)



#*******************************************************************************
##### 3.5. Sex-stratified BINARY logistic models (Female / Male) #####
#*******************************************************************************
# Outcome = OBH vs GBH (EVENT = GBH because levels = c("OBH","GBH"))
# OR > 1 => higher odds of GBH (vs OBH)
# OR < 1 => lower odds of GBH (i.e., relatively more OBH)

#*******************
###### 3.5.1. Define sex-specific datasets ######
#*******************
df_female <- df %>% dplyr::filter(demo_sex == "Female")
df_male   <- df %>% dplyr::filter(demo_sex == "Male")

adjust_vars_sex <- setdiff(adjust_vars, "demo_sex")

#*******************
###### 3.5.2. Helper: select predictors with variation in each sex ######
#*******************
get_predictors_with_variation <- function(data, predictors) {
  predictors[
    vapply(
      predictors,
      function(v) length(unique(stats::na.omit(data[[v]]))) > 1,
      FUN.VALUE = logical(1)
    )
  ]
}

main_predictors_female <- get_predictors_with_variation(df_female, main_predictors)
main_predictors_male   <- get_predictors_with_variation(df_male,   main_predictors)

length(main_predictors_female)
length(main_predictors_male)

#*******************
###### 3.5.3. Run models for FEMALE only ######
#*******************
# Interpretation:
# - ORs are for odds of GBH vs OBH.
# - OR > 1 indicates higher odds of GBH (relative to OBH).
results_female <- purrr::map_dfr(
  main_predictors_female,
  ~ fit_single_glm(
    exposure           = .x,
    adjust             = NULL,
    data               = df_female,
    outcome            = "obh_gbh_bin",
    ordinal_trend_vars = ordinal_trend_vars
  )
)

results_adjusted_female <- purrr::map_dfr(
  main_predictors_female,
  ~ fit_single_glm(
    exposure           = .x,
    adjust             = adjust_vars_sex,
    data               = df_female,
    outcome            = "obh_gbh_bin",
    ordinal_trend_vars = ordinal_trend_vars
  )
)

#*******************
###### 3.5.4. Run models for MALE only ######
#*******************
# Interpretation:
# - ORs are for odds of GBH vs OBH.
# - OR > 1 indicates higher odds of GBH (relative to OBH).
results_male <- purrr::map_dfr(
  main_predictors_male,
  ~ fit_single_glm(
    exposure           = .x,
    adjust             = NULL,
    data               = df_male,
    outcome            = "obh_gbh_bin",
    ordinal_trend_vars = ordinal_trend_vars
  )
)

results_adjusted_male <- purrr::map_dfr(
  main_predictors_male,
  ~ fit_single_glm(
    exposure           = .x,
    adjust             = adjust_vars_sex,
    data               = df_male,
    outcome            = "obh_gbh_bin",
    ordinal_trend_vars = ordinal_trend_vars
  )
)






#*******************
##### 3.6. Forest Plots #####
#*******************
# Goal of this version:
# - Use ONLY the variable names from Label_en on the y-axis (no “: term” suffixes).
# - Avoid duplicated y-axis labels by collapsing to ONE row per exposure when needed.
#   (For exposures that generate multiple coefficients, we keep the term with the
#    smallest p-value; you can change this rule easily.)
# - ORDER within each category (facet) from LOWEST to HIGHEST *signed effect*:
#     negative estimate (OR < 1)  ---> positive estimate (OR > 1)
#   This is done by sorting on estimate (log-odds). If estimate is missing,
#   we fall back to log(OR).

#*******************
###### 3.6.1. Add classification (from dictionary) and significance flag ######
#*******************

prepare_results_with_groups <- function(
    res_df,
    dictionary,
    sig_col = c("p_value", "p_BH")  # uses p_BH if present, otherwise p_value
) {
  sig_col <- sig_col[sig_col %in% names(res_df)][1]
  if (is.na(sig_col)) sig_col <- "p_value"
  
  # Dictionary mapping: assumes dictionary$Variable contains the df column names
  # (i.e., the same strings used in res_df$exposure).
  dict_groups <- dictionary %>%
    dplyr::arrange(Column) %>%
    dplyr::select(Column, Clasification, Variable, Label_en) %>%
    tidyr::fill(Clasification, .direction = "down") %>%
    dplyr::rename(
      group_es = Clasification,
      exposure = Variable
    )
  
  res_df %>%
    dplyr::mutate(exposure = as.character(exposure)) %>%
    dplyr::left_join(dict_groups, by = "exposure") %>%
    dplyr::mutate(
      group_en = dplyr::recode(
        group_es,
        "SOCIALES"                  = "Social variables",
        "VARIABLES SOCIOECONOMICAS" = "Socioeconomic variables",
        "ADVERSIDADES"              = "Adversities",
        "VARIABLES CLÍNICAS"        = "Clinical variables",
        "VARIABLES PSIQUIATRICAS"   = "Psychiatric variables",
        "DEMOGRAFICAS"              = "Demographic variables",
        "CONTROL"                   = "Control variables",
        .default = group_es
      ),
      group_en = factor(group_en, levels = unique(group_en[order(Column)])),
      significance = ifelse(.data[[sig_col]] < 0.05, "p < 0.05", "Not significant"),
      significance = factor(significance, levels = c("Not significant", "p < 0.05"))
    ) %>%
    dplyr::filter(!is.na(group_en))
}

#*******************
###### 3.6.2. Helper: keep ONE row per exposure (to avoid duplicated Label_en) ######
#*******************

pick_one_row_per_exposure <- function(df, prefer_sig_col = c("p_value", "p_BH")) {
  sig_col <- prefer_sig_col[prefer_sig_col %in% names(df)][1]
  if (is.na(sig_col)) sig_col <- "p_value"
  
  df %>%
    dplyr::group_by(exposure) %>%
    dplyr::arrange(.data[[sig_col]], .by_group = TRUE) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
}

#*******************
###### 3.6.3. Grouped forest plot (by variable category) ######
#*******************
# NOTE:
# - We keep your ORIGINAL ordering algorithm for:
#   (1) Overall plots, and (2) Female plots.
# - We add a NEW option ONLY for the MALE plots: force the male y-order to match
#   the FEMALE y-order (within each facet/category), without touching the general/female code.

#*******************
###### 3.6.3.A) ORIGINAL function (UNCHANGED) — used for Overall + Female ######
#*******************
make_grouped_forest <- function(
    df,
    label_col = "Label_en",   # y-axis label column
    group_col = "group_en",   # facet group column
    title = NULL,
    x_label = "Odds ratio (log scale)"
) {
  group_order <- c(
    "Control variables",
    "Social variables",
    "Socioeconomic variables",
    "Adversities",
    "Clinical variables",
    "Psychiatric variables"
  )
  
  df_plot <- df %>%
    dplyr::filter(
      !is.na(OR),
      !is.na(OR_low95),
      !is.na(OR_high95),
      OR > 0,
      OR_low95 > 0,
      OR_high95 > 0
    ) %>%
    pick_one_row_per_exposure(prefer_sig_col = c("p_value", "p_BH")) %>%
    dplyr::mutate(
      group = factor(.data[[group_col]], levels = group_order),
      label = as.character(.data[[label_col]]),
      
      # Signed ordering variable (negative -> positive):
      ord_value = dplyr::coalesce(estimate, log(OR)),
      
      # Unique y-key to preserve within-facet ordering without changing displayed labels
      label_key = paste0(as.character(group), "___", label),
      
      or_ci_label = sprintf("%.2f [%.2f–%.2f]", OR, OR_low95, OR_high95)
    ) %>%
    dplyr::filter(!is.na(group)) %>%
    dplyr::arrange(group, ord_value, label) %>%
    dplyr::mutate(
      # Put earliest rows at TOP of each facet
      label_key = factor(label_key, levels = rev(unique(label_key)))
    )
  
  max_ci    <- max(df_plot$OR_high95, na.rm = TRUE)
  label_pos <- max_ci * 1.6
  df_plot   <- df_plot %>% dplyr::mutate(x_label_pos = label_pos)
  
  ggplot2::ggplot(df_plot, ggplot2::aes(x = OR, y = label_key)) +
    ggplot2::geom_vline(
      xintercept = 1,
      linetype   = "dashed",
      linewidth  = 0.4,
      colour     = "grey50"
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = OR_low95, xmax = OR_high95, colour = significance),
      width     = 0,
      linewidth = 0.6
    ) +
    ggplot2::geom_point(
      ggplot2::aes(fill = significance, colour = significance),
      shape  = 21,
      size   = 2.6,
      stroke = 0.6
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = x_label_pos, label = or_ci_label, colour = significance),
      hjust = 0,
      size  = 5,
      show.legend = FALSE
    ) +
    ggplot2::scale_x_log10(
      name   = x_label,
      breaks = c(0.25, 0.5, 1, 2, 4),
      expand = ggplot2::expansion(mult = c(0.02, 0.35))
    ) +
    ggplot2::scale_fill_manual(
      name   = NULL,
      values = c("Not significant" = "white", "p < 0.05" = "black")
    ) +
    ggplot2::scale_colour_manual(
      values = c("Not significant" = "black", "p < 0.05" = "black"),
      guide = "none"
    ) +
    ggplot2::facet_grid(
      rows   = ggplot2::vars(group),
      scales = "free_y",
      space  = "free_y",
      switch = "y"
    ) +
    ggplot2::scale_y_discrete(labels = function(x) sub("^.*___", "", x)) +
    ggplot2::labs(title = title, y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(linewidth = 0.25, colour = "grey90"),
      axis.title.y  = ggplot2::element_blank(),
      axis.ticks.y  = ggplot2::element_blank(),
      axis.text.y   = ggplot2::element_text(size = 15.5),
      axis.text.x   = ggplot2::element_text(size = 15, angle = 45, hjust = 1),
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.text      = ggplot2::element_text(size = 15),
      plot.title        = ggplot2::element_text(face = "bold", hjust = 0, size = 14),
      strip.placement   = "outside",
      strip.background  = ggplot2::element_rect(fill = "grey95", colour = NA),
      strip.text.y.left = ggplot2::element_text(angle = 0, face = "bold", size = 16),
      plot.margin = ggplot2::margin(t = 5, r = 55, b = 5, l = 5)
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        override.aes = list(size = 3, colour = "black", shape = 21)
      )
    )
}

p_adjusted_grouped <- make_grouped_forest(
  df        = results_adjusted_grouped,
  label_col = "Label_en",
  title     = "Adjusted models"
)
print(p_adjusted_grouped)

#*******************
###### 3.6.3.B) NEW helpers — used ONLY to force MALE order = FEMALE order ######
#*******************

# Build the *exact* y-order (label_key levels) produced by the current algorithm,
# using a reference dataset (Females).
get_label_key_levels_from_reference <- function(
    ref_df,
    label_col = "Label_en",
    group_col = "group_en"
) {
  group_order <- c(
    "Control variables",
    "Social variables",
    "Socioeconomic variables",
    "Adversities",
    "Clinical variables",
    "Psychiatric variables"
  )
  
  ref_keys <- ref_df %>%
    dplyr::filter(
      !is.na(OR),
      !is.na(OR_low95),
      !is.na(OR_high95),
      OR > 0,
      OR_low95 > 0,
      OR_high95 > 0
    ) %>%
    pick_one_row_per_exposure(prefer_sig_col = c("p_value", "p_BH")) %>%
    dplyr::mutate(
      group = factor(.data[[group_col]], levels = group_order),
      label = as.character(.data[[label_col]]),
      ord_value = dplyr::coalesce(estimate, log(OR)),
      label_key = paste0(as.character(group), "___", label)
    ) %>%
    dplyr::filter(!is.na(group)) %>%
    dplyr::arrange(group, ord_value, label) %>%
    dplyr::pull(label_key) %>%
    unique()
  
  # IMPORTANT: match the factor levels used in your plot (rev(unique(...)))
  rev(ref_keys)
}

# Same plot as make_grouped_forest, but MALE y-order is forced to match FEMALE.
# General/Female plots are NOT affected because we only call this for males.
make_grouped_forest_match_reference <- function(
    df,
    reference_df,
    label_col = "Label_en",
    group_col = "group_en",
    title = NULL,
    x_label = "Odds ratio (log scale)"
) {
  group_order <- c(
    "Control variables",
    "Social variables",
    "Socioeconomic variables",
    "Adversities",
    "Clinical variables",
    "Psychiatric variables"
  )
  
  # Female-derived y-order (label_key levels)
  ref_levels <- get_label_key_levels_from_reference(
    ref_df    = reference_df,
    label_col = label_col,
    group_col = group_col
  )
  
  df_plot <- df %>%
    dplyr::filter(
      !is.na(OR),
      !is.na(OR_low95),
      !is.na(OR_high95),
      OR > 0,
      OR_low95 > 0,
      OR_high95 > 0
    ) %>%
    pick_one_row_per_exposure(prefer_sig_col = c("p_value", "p_BH")) %>%
    dplyr::mutate(
      group = factor(.data[[group_col]], levels = group_order),
      label = as.character(.data[[label_col]]),
      ord_value = dplyr::coalesce(estimate, log(OR)),
      label_key = paste0(as.character(group), "___", label),
      or_ci_label = sprintf("%.2f [%.2f–%.2f]", OR, OR_low95, OR_high95)
    ) %>%
    dplyr::filter(!is.na(group)) %>%
    dplyr::arrange(group, ord_value, label)
  
  # If the male set has any labels not present in females, append them at the end
  male_default_levels <- rev(unique(df_plot$label_key))
  final_levels <- c(ref_levels, setdiff(male_default_levels, ref_levels))
  
  df_plot <- df_plot %>%
    dplyr::mutate(label_key = factor(label_key, levels = final_levels))
  
  max_ci    <- max(df_plot$OR_high95, na.rm = TRUE)
  label_pos <- max_ci * 1.6
  df_plot   <- df_plot %>% dplyr::mutate(x_label_pos = label_pos)
  
  ggplot2::ggplot(df_plot, ggplot2::aes(x = OR, y = label_key)) +
    ggplot2::geom_vline(
      xintercept = 1,
      linetype   = "dashed",
      linewidth  = 0.4,
      colour     = "grey50"
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = OR_low95, xmax = OR_high95, colour = significance),
      width     = 0,
      linewidth = 0.6
    ) +
    ggplot2::geom_point(
      ggplot2::aes(fill = significance, colour = significance),
      shape  = 21,
      size   = 2.6,
      stroke = 0.6
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = x_label_pos, label = or_ci_label, colour = significance),
      hjust = 0,
      size  = 5,
      show.legend = FALSE
    ) +
    ggplot2::scale_x_log10(
      name   = x_label,
      breaks = c(0.25, 0.5, 1, 2, 4),
      expand = ggplot2::expansion(mult = c(0.02, 0.35))
    ) +
    ggplot2::scale_fill_manual(
      name   = NULL,
      values = c("Not significant" = "white", "p < 0.05" = "black")
    ) +
    ggplot2::scale_colour_manual(
      values = c("Not significant" = "black", "p < 0.05" = "black"),
      guide = "none"
    ) +
    ggplot2::facet_grid(
      rows   = ggplot2::vars(group),
      scales = "free_y",
      space  = "free_y",
      switch = "y"
    ) +
    ggplot2::scale_y_discrete(labels = function(x) sub("^.*___", "", x)) +
    ggplot2::labs(title = title, y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(linewidth = 0.25, colour = "grey90"),
      axis.title.y  = ggplot2::element_blank(),
      axis.ticks.y  = ggplot2::element_blank(),
      axis.text.y   = ggplot2::element_text(size = 15.5),
      axis.text.x   = ggplot2::element_text(size = 15, angle = 45, hjust = 1),
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.text      = ggplot2::element_text(size = 15),
      plot.title        = ggplot2::element_text(face = "bold", hjust = 0, size = 14),
      strip.placement   = "outside",
      strip.background  = ggplot2::element_rect(fill = "grey95", colour = NA),
      strip.text.y.left = ggplot2::element_text(angle = 0, face = "bold", size = 16),
      plot.margin = ggplot2::margin(t = 5, r = 55, b = 5, l = 5)
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        override.aes = list(size = 3, colour = "black", shape = 21)
      )
    )
}



#*******************
###### 3.6.4. Build grouped results (overall) ######
#*******************

results_grouped          <- prepare_results_with_groups(results,          dictionary)
results_adjusted_grouped <- prepare_results_with_groups(results_adjusted, dictionary)

exclude_exposures <- c(
  "totalsocial_score", "categoria_econosocial",
  "indice_econosocial", "indice_econosocial_0_10",
  "fragiles", "z_violencia_score", "violencia_score_0_10_r", "demo_sex",
  "obesidad_livingston", "education_livingston"
)

results_grouped <- results_grouped %>%
  dplyr::filter(!exposure %in% exclude_exposures)

results_adjusted_grouped <- results_adjusted_grouped %>%
  dplyr::filter(!exposure %in% exclude_exposures)

# Overall plots (UNCHANGED ordering algorithm)
p_unadjusted_grouped <- make_grouped_forest(
  df        = results_grouped,
  label_col = "Label_en",
  title     = "Unadjusted models"
)
print(p_unadjusted_grouped)

p_adjusted_grouped <- make_grouped_forest(
  df        = results_adjusted_grouped,
  label_col = "Label_en",
  title     = "Adjusted models"
)
print(p_adjusted_grouped)

#*******************
###### 3.6.5. Forest plots stratified by sex ######
#*******************

results_female_grouped <- results_female %>%
  prepare_results_with_groups(dictionary = dictionary) %>%
  dplyr::filter(!exposure %in% exclude_exposures)

results_adjusted_female_grouped <- results_adjusted_female %>%
  prepare_results_with_groups(dictionary = dictionary) %>%
  dplyr::filter(!exposure %in% exclude_exposures)

results_male_grouped <- results_male %>%
  prepare_results_with_groups(dictionary = dictionary) %>%
  dplyr::filter(!exposure %in% exclude_exposures)

results_adjusted_male_grouped <- results_adjusted_male %>%
  prepare_results_with_groups(dictionary = dictionary) %>%
  dplyr::filter(!exposure %in% exclude_exposures)

# Female plots (UNCHANGED ordering algorithm)
p_unadjusted_female <- make_grouped_forest(
  df        = results_female_grouped,
  label_col = "Label_en",
  title     = "Unadjusted models (Females)"
)
print(p_unadjusted_female)

p_adjusted_female <- make_grouped_forest(
  df        = results_adjusted_female_grouped,
  label_col = "Label_en",
  title     = "Adjusted models (Females)"
)
print(p_adjusted_female)

# Male plots (FORCE SAME ORDER AS FEMALES)
# - Unadjusted male follows unadjusted female order
p_unadjusted_male <- make_grouped_forest_match_reference(
  df           = results_male_grouped,
  reference_df = results_female_grouped,
  label_col    = "Label_en",
  title        = "Unadjusted models (Males)"
)
print(p_unadjusted_male)

# - Adjusted male follows adjusted female order
p_adjusted_male <- make_grouped_forest_match_reference(
  df           = results_adjusted_male_grouped,
  reference_df = results_adjusted_female_grouped,
  label_col    = "Label_en",
  title        = "Adjusted models (Males)"
)
print(p_adjusted_male)







#*******************************************************************************
#### 4. BINARY LOGISTIC MODELS FOR BRAIN STRUCTURE VOLUMES (OBH vs GBH) ####
#*******************************************************************************
# Goal:
#   Outcome = General Brain Health (GBH) vs Optimal Brain Health (OBH)
#   Predictors = brain structure volumes (columns 50 to 168 in df),
#                plus global lesion/volume measures.
#   Fit ONE model per exposure (one-at-a-time), with:
#     - Unadjusted models
#     - Adjusted models (by adjust_vars defined previously)
#
# Interpretation (given the outcome coding below):
#   OR > 1  => higher odds (risk) of being GBH (worse brain health)
#   OR < 1  => lower odds (risk) of being GBH (i.e., more likely OBH)
#
# Notes:
# - Regional volumes are standardized (mean 0, SD 1) so ORs are per 1 SD increase.
# - Global measures listed in no_scale_vars keep their original units (OR per 1 unit).
# - Uses glm(..., family = binomial) (binary logistic), NOT polr.
#*******************************************************************************

# Use treatment contrasts (prevents polynomial contrasts from ordered factors)
options(contrasts = c("contr.treatment", "contr.treatment"))

# 1) Copy data and define BINARY outcome: OBH vs GBH
df <- data

# Binary outcome:
# IMPORTANT: glm with a 2-level factor models the log-odds of the SECOND level.
# Here we set levels = c("OBH", "GBH") so the modeled event is "GBH".
# Any other groups (e.g., BHD) are set to NA and dropped by na.omit.
df$obh_gbh_bin <- factor(
  dplyr::case_when(
    df$tipo_grupo == "OBH" ~ "OBH",
    df$tipo_grupo == "GBH" ~ "GBH",
    TRUE                   ~ NA_character_
  ),
  levels = c("OBH", "GBH")
)

# Rename Lesion_Volume (ml) to a safe name, if present
if ("Lesion_Volume (ml)" %in% names(df)) {
  names(df)[names(df) == "Lesion_Volume (ml)"] <- "Lesion_Volume_ml"
}

#*******************
##### 4.1. Select and scale volume predictors #####
#*******************

if (ncol(df) < 50) {
  stop("df has fewer than 50 columns; cannot select volume predictors from columns 50:168.")
}

idx_from <- 50
idx_to   <- min(168, ncol(df))
volume_preds_all <- names(df)[idx_from:idx_to]

no_scale_vars <- c("vic", "Lesion_Volume_ml", "Number_of_Lesions")
no_scale_vars <- intersect(no_scale_vars, volume_preds_all)

volume_scaled_vars <- setdiff(volume_preds_all, no_scale_vars)

df_vol <- df

if (length(volume_scaled_vars) > 0) {
  df_vol[volume_scaled_vars] <- lapply(
    df_vol[volume_scaled_vars],
    function(x) as.numeric(scale(as.numeric(as.character(x))))
  )
}

if (length(no_scale_vars) > 0) {
  df_vol[no_scale_vars] <- lapply(
    df_vol[no_scale_vars],
    function(x) as.numeric(as.character(x))
  )
}

volume_preds_final <- volume_preds_all[
  sapply(df_vol[volume_preds_all], function(x) length(unique(na.omit(x))) > 1)
]

#*******************
##### 4.2. Helper: fit ONE binary logistic model per exposure (one-at-a-time) #####
#*******************

regex_escape <- function(x) {
  gsub("([\\.|\\+|\\*|\\?|\\^|\\$|\\(|\\)|\\[|\\]|\\{|\\}|\\||\\\\])", "\\\\\\1", x)
}

fit_single_glm_volume <- function(
    exposure,
    adjust = NULL,
    data,
    outcome = "obh_gbh_bin"
) {
  exposure <- as.character(exposure)[1]
  
  rhs_terms <- c(exposure, adjust)
  form_str  <- paste(outcome, "~", paste(rhs_terms, collapse = " + "))
  form_full <- stats::as.formula(form_str)
  
  model <- stats::glm(
    formula   = form_full,
    data      = data,
    family    = stats::binomial(link = "logit"),
    na.action = stats::na.omit
  )
  
  aic_val <- stats::AIC(model)
  bic_val <- stats::BIC(model)
  
  r2_mcfadden <- tryCatch({
    null_model <- stats::update(model, . ~ 1)
    1 - (as.numeric(stats::logLik(model)) / as.numeric(stats::logLik(null_model)))
  }, error = function(e) NA_real_)
  
  coefs <- summary(model)$coefficients
  
  res <- tibble::tibble(
    term      = rownames(coefs),
    estimate  = coefs[, "Estimate"],
    std.error = coefs[, "Std. Error"],
    z         = coefs[, "z value"]
  ) %>%
    dplyr::mutate(
      p_value   = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
      OR        = exp(estimate),
      OR_low95  = exp(estimate - 1.96 * std.error),
      OR_high95 = exp(estimate + 1.96 * std.error),
      exposure  = exposure,
      adjustment = ifelse(
        is.null(adjust) || length(adjust) == 0,
        "none",
        paste(adjust, collapse = " + ")
      ),
      R2         = r2_mcfadden,
      AIC        = aic_val,
      BIC        = bic_val
    ) %>%
    dplyr::filter(term != "(Intercept)")
  
  safe_exposure <- regex_escape(exposure)
  res <- res %>%
    dplyr::filter(grepl(paste0("^", safe_exposure), term))
  
  res %>%
    dplyr::select(
      exposure,
      adjustment,
      term,
      OR,
      OR_low95,
      OR_high95,
      estimate,
      std.error,
      z,
      p_value,
      R2,
      AIC,
      BIC
    )
}

#*******************
##### 4.3. Run models #####
#*******************

results_volume <- purrr::map_dfr(
  volume_preds_final,
  ~ fit_single_glm_volume(
    exposure = .x,
    adjust   = NULL,
    data     = df_vol,
    outcome  = "obh_gbh_bin"
  )
)

if (!exists("adjust_vars")) {
  stop("adjust_vars not found in your environment. Define adjust_vars before running adjusted models.")
}
adjust_vars <- intersect(adjust_vars, names(df_vol))

results_adjusted_volume <- purrr::map_dfr(
  volume_preds_final,
  ~ fit_single_glm_volume(
    exposure = .x,
    adjust   = adjust_vars,
    data     = df_vol,
    outcome  = "obh_gbh_bin"
  )
)

#*******************************************************************************
##### 4.4. DK cortical atlas map — OBH vs No-OBH (binary logistic) #####
#     Diverging colors around OR = 1 (white at OR=1), with a LEGEND THAT MATCHES
#     THE ACTUAL MAPPED CORTICAL EFFECTS
#*******************************************************************************
# What this script does (correctly):
# - Uses results_adjusted_volume from your BINARY logistic models (OBH vs No-OBH).
# - Maps ONLY cortical DK regions using region_lut (so: no pallidum, no globals).
# - For each (region, hemisphere), keeps the SINGLE most significant cortical entry
#   (smallest p-value), instead of averaging.
# - Colors using log(OR): blue (OR<1), white (OR=1), red (OR>1).
# - If ONLY_SIG = TRUE, non-significant cortical regions are grey (NA fill).
#
# IMPORTANT:
# - If you have significant protective effects (OR<1) but they are NOT DK cortical
#   (e.g., pallidum, lesion volume), they will NOT appear here. They belong in your
#   "unmapped/global/subcortical" heatmap/table section.
#*******************************************************************************

ONLY_SIG <- TRUE   # TRUE = color only significant cortical DK regions; FALSE = color all mapped cortical DK regions

#*******************
###### 4.4.1. Lookup table: column prefix -> DK atlas region (cortical ONLY) ######
#*******************
region_lut <- tibble::tribble(
  ~col_prefix,               ~region,
  "G_Precentral",            "precentral",
  "G_Frontal_Sup",           "superior frontal",
  "G_Frontal_Mid",           "rostral middle frontal",
  "G_Supp_Motor_Area",       "superior frontal",
  "G_Frontal_Sup_Orb",       "lateral orbitofrontal",
  "G_Frontal_Mid_Orb",       "lateral orbitofrontal",
  "G_Frontal_Inf_Orb",       "pars orbitalis",
  "G_Frontal_Med_Orb",       "medial orbitofrontal",
  "G_Frontal_Inf_Oper",      "pars opercularis",
  "G_Frontal_Inf_Tri",       "pars triangularis",
  "G_Insula",                "insula",
  "G_Cingulum_Ant",          "rostral anterior cingulate",
  "G_Cingulum_Mid",          "caudal anterior cingulate",
  "G_Cingulum_Post",         "posterior cingulate",
  "G_ParaHippocampal",       "parahippocampal",
  "G_Calcarine",             "pericalcarine",
  "G_Cuneus",                "cuneus",
  "G_Lingual",               "lingual",
  "G_Occipital_Sup",         "lateral occipital",
  "G_Occipital_Mid",         "lateral occipital",
  "G_Occipital_Inf",         "lateral occipital",
  "G_Fusiform",              "fusiform",
  "G_Postcentral",           "postcentral",
  "G_Parietal_Sup",          "superior parietal",
  "G_Parietal_Inf",          "inferior parietal",
  "G_SupraMarginal",         "supramarginal",
  "G_Angular",               "inferior parietal",
  "G_Precuneus",             "precuneus",
  "G_Paracentral_Lob",       "paracentral",
  "G_Heschl",                "transverse temporal",
  "G_Temporal_Sup",          "superior temporal",
  "G_Temporal_Mid",          "middle temporal",
  "G_Temporal_Inf",          "inferior temporal",
  "G_Temporal_Pole_Sup",     "temporal pole",
  "G_Temporal_Pol_Mid",      "temporal pole"
)

#*******************************************************************************
# FIX (minimal): Recreate mapping_check / mapped_in_lut / not_mapped_in_lut
# so "unmapped_exposures" exists for your heatmaps.
#
# Why it disappeared:
# - In the old script you built not_mapped_in_lut from df columns (50:168).
# - In your current script you removed that block, so not_mapped_in_lut
#   (and thus unmapped_exposures) no longer exists.
#
# What we do now:
# - Build the mapping table directly from results_adjusted_volume$exposure,
#   so it always matches the exposures you actually modeled.
# - IMPORTANT: This will include globals (Lesion_Volume_ml, Number_of_Lesions, vic)
#   and subcorticals (e.g., G_Pallidum_L) as "not mapped" (correct for DK cortex).
#*******************************************************************************

# Exposures actually present in your model results
modeled_exposures <- results_adjusted_volume %>%
  dplyr::distinct(exposure) %>%
  dplyr::pull(exposure)

mapping_check <- tibble::tibble(exposure = modeled_exposures) %>%
  dplyr::mutate(
    hemi = dplyr::case_when(
      grepl("_L$", exposure) ~ "left",
      grepl("_R$", exposure) ~ "right",
      TRUE ~ NA_character_
    ),
    col_prefix = gsub("_(L|R)$", "", exposure)
  ) %>%
  dplyr::left_join(region_lut, by = "col_prefix")

mapped_in_lut <- mapping_check %>% dplyr::filter(!is.na(region))
not_mapped_in_lut <- mapping_check %>% dplyr::filter(is.na(region))

# Vector of exposures that were NOT mapped in the cortical brain plot
unmapped_exposures <- not_mapped_in_lut$exposure

#*******************
###### 4.4.2. Build region-hemisphere effects table for ggseg ######
#    Keep ONLY the most significant cortical entry per (region, hemi)
#*******************
effects_ggseg <- results_adjusted_volume %>%
  dplyr::mutate(
    logOR = log(OR),
    hemi  = dplyr::case_when(
      grepl("_L$", exposure) ~ "left",
      grepl("_R$", exposure) ~ "right",
      TRUE ~ NA_character_
    ),
    col_prefix = gsub("_(L|R)$", "", exposure)
  ) %>%
  dplyr::left_join(region_lut, by = "col_prefix") %>%
  dplyr::filter(!is.na(region), !is.na(hemi)) %>%
  dplyr::group_by(region, hemi) %>%
  dplyr::arrange(p_value, .by_group = TRUE) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    sig = p_value < 0.05,
    logOR_plot = dplyr::if_else(ONLY_SIG & !sig, NA_real_, logOR)
  )

#*******************
###### 4.4.3. LEGEND: set breaks based on the ACTUAL mapped cortical values ######
#*******************

max_log <- max(effects_ggseg$logOR_plot, na.rm = TRUE)
min_log <- min(effects_ggseg$logOR_plot, na.rm = TRUE)

max_or <- exp(max_log)
min_or <- exp(min_log)

has_blue <- any(effects_ggseg$logOR_plot < 0, na.rm = TRUE)

or_breaks_pos <- c(1, 1.25, 1.5, 2, 3, 4)
or_breaks_neg <- c(0.5, 0.75, 1)

if (has_blue) {
  or_breaks <- unique(c(or_breaks_neg, or_breaks_pos))
} else {
  or_breaks <- or_breaks_pos
}

or_breaks <- or_breaks[or_breaks >= min_or * 0.95 & or_breaks <= max_or * 1.05]
or_breaks <- unique(c(1, or_breaks))
or_breaks <- sort(or_breaks)

fill_breaks <- log(or_breaks)

# NOTE: keep the SCALE symmetric around 0 so white is exactly OR = 1
max_abs <- max(abs(effects_ggseg$logOR_plot), na.rm = TRUE)
fill_limits <- c(-max_abs, max_abs)

#*******************
###### 4.4.4. Plot ######
#*******************
ggseg(
  atlas   = dk,
  .data   = effects_ggseg,
  mapping = aes(fill = logOR_plot),
  colour  = "black",
  size    = 0.2,
  show.legend = TRUE
) +
  scale_fill_gradient2(
    name     = "OR (per 1 SD)",
    low      = "#2166ac",
    mid      = "white",
    high     = "#b2182b",
    midpoint = 0,
    limits   = fill_limits,
    breaks   = fill_breaks,
    labels   = format(or_breaks, trim = TRUE),
    na.value = "grey95"
  ) +
  labs(
    title    = "Brain map of cortical volume effects on odds of OBH",
    subtitle = if (ONLY_SIG) {
      if (has_blue) {
        "Only significant DK cortical regions colored (p < 0.05); white = OR 1.0"
      } else {
        "Only significant DK cortical regions colored (p < 0.05); all mapped effects are OR ≥ 1"
      }
    } else {
      if (has_blue) {
        "All mapped DK cortical regions colored; white = OR 1.0"
      } else {
        "All mapped DK cortical regions colored; all mapped effects are OR ≥ 1"
      }
    }
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title      = element_text(hjust = 0, face = "bold")
  )

# Optional sanity check
# effects_ggseg %>%
#   dplyr::summarise(
#     n_regions = n(),
#     n_sig     = sum(sig, na.rm = TRUE),
#     any_OR_lt1 = any(exp(logOR_plot) < 1, na.rm = TRUE),
#     min_OR    = min(exp(logOR_plot), na.rm = TRUE),
#     max_OR    = max(exp(logOR_plot), na.rm = TRUE)
#   ) %>% print()

#*******************
##### 4.5. Heatmap for subcortical, cerebellar and global measures #####
#*******************

effects_grid_unmapped <- results_adjusted_volume %>%
  dplyr::filter(exposure %in% unmapped_exposures) %>%
  dplyr::mutate(
    OR   = OR,
    logOR = log(OR),
    hemi = dplyr::case_when(
      grepl("_L$", exposure) ~ "Left",
      grepl("_R$", exposure) ~ "Right",
      TRUE                   ~ "Midline/Global"
    ),
    region_base = gsub("_(L|R)$", "", exposure),
    lobe = dplyr::case_when(
      grepl("Frontal",  region_base, ignore.case = TRUE) ~ "Frontal (basal/medial)",
      grepl("Parietal", region_base, ignore.case = TRUE) ~ "Parietal",
      grepl("Temporal", region_base, ignore.case = TRUE) ~ "Temporal medial",
      grepl("Occipital",region_base, ignore.case = TRUE) ~ "Occipital",
      grepl("Cerebelum|Vermis", region_base, ignore.case = TRUE) ~ "Cerebellum / Vermis",
      grepl("Hippocampus|Amygdala|Caudate|Putamen|Pallidum|Thalamus",
            region_base, ignore.case = TRUE) ~ "Subcortical nuclei",
      grepl("vic|Lesion_Volume|Number_of_Lesions",
            region_base, ignore.case = TRUE) ~ "Global lesion burden",
      TRUE ~ "Other"
    ),
    sig_stars = dplyr::case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE            ~ ""
    )
  )

effects_grid_unmapped$lobe <- factor(
  effects_grid_unmapped$lobe,
  levels = c(
    "Subcortical nuclei",
    "Cerebellum / Vermis",
    "Temporal medial",
    "Frontal (basal/medial)",
    "Global lesion burden",
    "Parietal",
    "Occipital",
    "Other"
  )
)

plot_volume_heatmap <- function(df, main_title, subtitle_extra = "") {
  ggplot(
    df,
    aes(x = hemi, y = forcats::fct_reorder(region_base, OR))
  ) +
    geom_tile(aes(fill = logOR), color = "white") +
    geom_text(
      data = subset(df, sig_stars != ""),
      aes(label = sig_stars),
      color    = "black",
      size     = 2.5,
      fontface = "bold"
    ) +
    scale_fill_gradient2(
      name     = "OR",
      low      = "#2166ac",
      mid      = "white",
      high     = "#b2182b",
      midpoint = 0,  # log(1) = 0
      breaks   = log(c(0.8, 1, 1.2, 1.5, 2, 3.5)),
      labels   = c("0.8", "1.0", "1.2", "1.5", "2.0", "3.5")
    ) +
    labs(
      x = "Hemisphere",
      y = "Region / measure",
      title = main_title
      # subtitle can be re-enabled if you want
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid  = element_blank(),
      strip.text  = element_text(face = "bold"),
      axis.text.y = element_text(size = 7)
    )
}

df_subcortical <- effects_grid_unmapped %>% dplyr::filter(lobe == "Subcortical nuclei")
plot_volume_heatmap(df_subcortical, main_title = "Subcortical nuclei volumes and brain health")

df_cerebellum <- effects_grid_unmapped %>% dplyr::filter(lobe == "Cerebellum / Vermis")
plot_volume_heatmap(df_cerebellum, main_title = "Cerebellar and vermis volumes and brain health")

df_global <- effects_grid_unmapped %>% dplyr::filter(lobe == "Global lesion burden")
plot_volume_heatmap(
  df_global,
  main_title = "Global lesion burden and brain health",
  subtitle_extra = "vic, total lesion volume (ml), number of lesions"
)

df_temporal_medial <- effects_grid_unmapped %>% dplyr::filter(lobe == "Temporal medial")
plot_volume_heatmap(df_temporal_medial, main_title = "Medial temporal structures and brain health")

df_frontal_basal <- effects_grid_unmapped %>% dplyr::filter(lobe == "Frontal (basal/medial)")
plot_volume_heatmap(df_frontal_basal, main_title = "Basal/medial frontal structures and brain health")

df_other <- effects_grid_unmapped %>% dplyr::filter(lobe == "Other")
plot_volume_heatmap(df_other, main_title = "Other non-cortical regions and brain health")

# Optional: inspect mapping tables
# print(mapped_in_lut, n = 200)
# print(not_mapped_in_lut, n = 200)




#*******************
##### 4.6. Forest for global indicators of volume dataframe #####
#*******************

globals <- c("vic", "Lesion_Volume_ml", "Number_of_Lesions")

forest_global <- bind_rows(
  # Unadjusted models
  results_volume %>%
    dplyr::filter(exposure %in% globals, adjustment == "none") %>%
    dplyr::mutate(model = "Unadjusted"),
  # Adjusted models
  results_adjusted_volume %>%
    dplyr::filter(exposure %in% globals, adjustment != "none") %>%
    dplyr::mutate(model = "Adjusted")
) %>%
  dplyr::mutate(
    exposure_f = factor(
      exposure,
      levels = globals,
      labels = c("vic (index)", "Lesion volume (ml)", "Number of lesions")
    ),
    model = factor(model, levels = c("Unadjusted", "Adjusted"))
  )


label_df <- forest_global %>%
  dplyr::mutate(or_ci = sprintf("%.2f [%.2f–%.2f]", OR, OR_low95, OR_high95)) %>%
  dplyr::select(exposure_f, model, or_ci) %>%
  tidyr::pivot_wider(names_from = model, values_from = or_ci) %>%
  dplyr::mutate(
    label = paste0(
      "Unadj: ", Unadjusted, "\n",
      "Adj:   ", Adjusted
    )
  )

# Posición en el eje X donde irá la columna de texto
max_ci    <- max(forest_global$OR_high95, na.rm = TRUE)
label_pos <- max_ci * 1.6


# misma posición para barras y puntos
pos_dodge <- position_dodge(width = 0.4)

# Función para hacer un forest plot de UNA sola variable
make_single_forest <- function(df, var_name, var_label = var_name) {
  
  df_plot <- df %>%
    dplyr::filter(exposure == var_name) %>%
    dplyr::mutate(
      model = factor(model, levels = c("Unadjusted", "Adjusted"))
    )
  
  # Texto OR [IC95]
  label_df <- df_plot %>%
    dplyr::mutate(or_ci = sprintf("%.2f [%.2f–%.2f]", OR, OR_low95, OR_high95))
  
  # Rango del eje X (escala LINEAL)
  max_ci <- max(df_plot$OR_high95, na.rm = TRUE)
  min_ci <- min(df_plot$OR_low95, na.rm = TRUE)
  
  # Que el eje incluya el 1 siempre
  x_min <- min(1, min_ci * 0.9)
  x_max <- max(1, max_ci * 1.1)
  
  # Posición para la columna de texto
  x_text <- max_ci * 1.05
  x_lim_max <- max_ci * 1.35
  
  ggplot(df_plot, aes(x = OR, y = model)) +
    # Línea de referencia en OR = 1
    geom_vline(xintercept = 1, linetype = "dashed",
               linewidth = 0.4, colour = "grey40") +
    
    # Barras de IC
    geom_errorbar(
      aes(xmin = OR_low95, xmax = OR_high95),
      width = 0,
      linewidth = 0.5
    ) +
    
    # Puntos de OR
    geom_point(size = 2) +
    
    # Columna de texto OR [IC95]
    geom_text(
      data = label_df,
      aes(y = model, label = or_ci),  # mapeos
      x = x_text,                     # posición fija en X
      inherit.aes = FALSE,
      hjust = 0,
      size  = 3
    ) +
    
    # Encabezado de la columna de texto
    annotate(
      "text",
      x = x_text,
      y = 2.3,  # un poco por encima de la fila superior (hay 2 modelos)
      label = "OR [95% CI]",
      hjust = 0,
      size  = 3.1,
      fontface = "bold"
    ) +
    
    scale_x_continuous(
      limits = c(x_min, x_lim_max),
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    
    labs(
      x = "Odds ratio",
      y = NULL,
      title = var_label
    ) +
    
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
      axis.text.y        = element_text(face = "bold"),
      axis.text.x        = element_text(size = 8),
      plot.title         = element_text(face = "bold", hjust = 0),
      legend.position    = "none",
      plot.margin        = margin(t = 5, r = 60, b = 5, l = 5)  # margen derecho p/ texto
    )
}


# Llamadas para cada variable

p_vic <- make_single_forest(
  forest_global,
  var_name  = "vic",
  var_label = "vic (index)"
)

p_volume <- make_single_forest(
  forest_global,
  var_name  = "Lesion_Volume_ml",
  var_label = "Lesion volume (ml)"
)

p_nlesions <- make_single_forest(
  forest_global,
  var_name  = "Number_of_Lesions",
  var_label = "Number of lesions"
)

# Mostrar cada uno
p_vic
p_volume
p_nlesions




#*******************
##### 4.7. Word table of significant results #####
#*******************

# filter significant rows using results_adjusted_volume$p_value
significant_rows <- results_adjusted_volume[results_adjusted_volume$p_value < 0.05, ]

significant_rows$term = NULL

#*******************
# 1) Lookup: exposure -> label + section + ordering
#*******************
aal_lookup <- tibble::tribble(
  ~exposure,                 ~section,                          ~brain_region_aal,                                     ~base_id,                      ~hemi,
  # Frontal regions
  "G_Precentral_L",          "Frontal regions",                 "Precentral gyrus, left",                              "Precentral gyrus",            "L",
  "G_Precentral_R",          "Frontal regions",                 "Precentral gyrus, right",                             "Precentral gyrus",            "R",
  "G_Supp_Motor_Area_R",     "Frontal regions",                 "Supplementary motor area, right",                     "Supplementary motor area",    "R",
  "G_Frontal_Sup_L",         "Frontal regions",                 "Superior frontal gyrus, left",                         "Superior frontal gyrus",      "L",
  "G_Frontal_Sup_R",         "Frontal regions",                 "Superior frontal gyrus, right",                        "Superior frontal gyrus",      "R",
  "G_Frontal_Sup_Orb_L",     "Frontal regions",                 "Superior frontal gyrus (orbital part), left",          "Superior frontal gyrus (orbital part)","L",
  "G_Frontal_Mid_L",         "Frontal regions",                 "Middle frontal gyrus, left",                           "Middle frontal gyrus",        "L",
  "G_Frontal_Mid_R",         "Frontal regions",                 "Middle frontal gyrus, right",                          "Middle frontal gyrus",        "R",
  "G_Frontal_Mid_Orb_L",     "Frontal regions",                 "Middle frontal gyrus (orbital part), left",            "Middle frontal gyrus (orbital part)","L",
  "G_Frontal_Inf_Tri_L",     "Frontal regions",                 "Inferior frontal gyrus (triangular part), left",       "Inferior frontal gyrus (triangular part)","L",
  "G_Frontal_Inf_Orb_L",     "Frontal regions",                 "Inferior frontal gyrus (orbital part), left",          "Inferior frontal gyrus (orbital part)","L",
  "G_Olfactory_L",           "Frontal regions",                 "Olfactory cortex, left",                               "Olfactory cortex",            "L",
  "G_Olfactory_R",           "Frontal regions",                 "Olfactory cortex, right",                              "Olfactory cortex",            "R",
  "G_Frontal_Sup_Med_L",     "Frontal regions",                 "Superior frontal gyrus (medial), left",                "Superior frontal gyrus (medial)","L",
  "G_Frontal_Med_Orb_L",     "Frontal regions",                 "Medial orbitofrontal cortex, left",                    "Medial orbitofrontal cortex", "L",
  "G_Frontal_Med_Orb_R",     "Frontal regions",                 "Medial orbitofrontal cortex, right",                   "Medial orbitofrontal cortex", "R",
  "G_Rectus_L",              "Frontal regions",                 "Gyrus rectus, left",                                   "Gyrus rectus",                "L",
  "G_Rectus_R",              "Frontal regions",                 "Gyrus rectus, right",                                  "Gyrus rectus",                "R",
  
  # Limbic and paralimbic regions
  "G_Insula_L",              "Limbic and paralimbic regions",   "Insula, left",                                         "Insula",                      "L",
  "G_Insula_R",              "Limbic and paralimbic regions",   "Insula, right",                                        "Insula",                      "R",
  "G_Cingulum_Ant_L",        "Limbic and paralimbic regions",   "Anterior cingulate cortex, left",                      "Anterior cingulate cortex",   "L",
  "G_Cingulum_Ant_R",        "Limbic and paralimbic regions",   "Anterior cingulate cortex, right",                     "Anterior cingulate cortex",   "R",
  "G_Cingulum_Mid_L",        "Limbic and paralimbic regions",   "Middle cingulate cortex, left",                        "Middle cingulate cortex",     "L",
  "G_Cingulum_Mid_R",        "Limbic and paralimbic regions",   "Middle cingulate cortex, right",                       "Middle cingulate cortex",     "R",
  "G_Hippocampus_L",         "Limbic and paralimbic regions",   "Hippocampus, left",                                    "Hippocampus",                 "L",
  "G_Hippocampus_R",         "Limbic and paralimbic regions",   "Hippocampus, right",                                   "Hippocampus",                 "R",
  "G_ParaHippocampal_L",     "Limbic and paralimbic regions",   "Parahippocampal gyrus, left",                          "Parahippocampal gyrus",       "L",
  "G_ParaHippocampal_R",     "Limbic and paralimbic regions",   "Parahippocampal gyrus, right",                         "Parahippocampal gyrus",       "R",
  "G_Amygdala_L",            "Limbic and paralimbic regions",   "Amygdala, left",                                       "Amygdala",                    "L",
  "G_Amygdala_R",            "Limbic and paralimbic regions",   "Amygdala, right",                                      "Amygdala",                    "R",
  
  # Temporal regions
  "G_Heschl_L",              "Temporal regions",                "Heschl's gyrus, left",                                 "Heschl's gyrus",              "L",
  "G_Temporal_Sup_L",        "Temporal regions",                "Superior temporal gyrus, left",                        "Superior temporal gyrus",     "L",
  "G_Temporal_Sup_R",        "Temporal regions",                "Superior temporal gyrus, right",                       "Superior temporal gyrus",     "R",
  "G_Temporal_Pole_Sup_L",   "Temporal regions",                "Temporal pole (superior), left",                       "Temporal pole (superior)",    "L",
  "G_Temporal_Pole_Sup_R",   "Temporal regions",                "Temporal pole (superior), right",                      "Temporal pole (superior)",    "R",
  "G_Temporal_Mid_L",        "Temporal regions",                "Middle temporal gyrus, left",                          "Middle temporal gyrus",       "L",
  "G_Temporal_Mid_R",        "Temporal regions",                "Middle temporal gyrus, right",                         "Middle temporal gyrus",       "R",
  "G_Temporal_Pol_Mid_L",    "Temporal regions",                "Temporal pole (middle), left",                         "Temporal pole (middle)",      "L",
  "G_Temporal_Pol_Mid_R",    "Temporal regions",                "Temporal pole (middle), right",                        "Temporal pole (middle)",      "R",
  "G_Temporal_Inf_L",        "Temporal regions",                "Inferior temporal gyrus, left",                        "Inferior temporal gyrus",     "L",
  "G_Temporal_Inf_R",        "Temporal regions",                "Inferior temporal gyrus, right",                       "Inferior temporal gyrus",     "R",
  
  # Parietal and occipital regions
  "G_Postcentral_L",         "Parietal and occipital regions",  "Postcentral gyrus, left",                              "Postcentral gyrus",           "L",
  "G_Parietal_Sup_L",        "Parietal and occipital regions",  "Superior parietal lobule, left",                       "Superior parietal lobule",    "L",
  "G_Parietal_Sup_R",        "Parietal and occipital regions",  "Superior parietal lobule, right",                      "Superior parietal lobule",    "R",
  "G_Parietal_Inf_L",        "Parietal and occipital regions",  "Inferior parietal lobule, left",                       "Inferior parietal lobule",    "L",
  "G_Parietal_Inf_R",        "Parietal and occipital regions",  "Inferior parietal lobule, right",                      "Inferior parietal lobule",    "R",
  "G_SupraMarginal_R",       "Parietal and occipital regions",  "Supramarginal gyrus, right",                           "Supramarginal gyrus",         "R",
  "G_Angular_L",             "Parietal and occipital regions",  "Angular gyrus, left",                                  "Angular gyrus",               "L",
  "G_Angular_R",             "Parietal and occipital regions",  "Angular gyrus, right",                                 "Angular gyrus",               "R",
  "G_Precuneus_L",           "Parietal and occipital regions",  "Precuneus, left",                                      "Precuneus",                   "L",
  "G_Precuneus_R",           "Parietal and occipital regions",  "Precuneus, right",                                     "Precuneus",                   "R",
  "G_Calcarine_L",           "Parietal and occipital regions",  "Calcarine cortex, left",                               "Calcarine cortex",            "L",
  "G_Calcarine_R",           "Parietal and occipital regions",  "Calcarine cortex, right",                              "Calcarine cortex",            "R",
  "G_Lingual_L",             "Parietal and occipital regions",  "Lingual gyrus, left",                                  "Lingual gyrus",               "L",
  "G_Lingual_R",             "Parietal and occipital regions",  "Lingual gyrus, right",                                 "Lingual gyrus",               "R",
  "G_Occipital_Sup_L",       "Parietal and occipital regions",  "Superior occipital gyrus, left",                       "Superior occipital gyrus",    "L",
  "G_Occipital_Mid_L",       "Parietal and occipital regions",  "Middle occipital gyrus, left",                         "Middle occipital gyrus",      "L",
  "G_Occipital_Mid_R",       "Parietal and occipital regions",  "Middle occipital gyrus, right",                        "Middle occipital gyrus",      "R",
  "G_Occipital_Inf_L",       "Parietal and occipital regions",  "Inferior occipital gyrus, left",                       "Inferior occipital gyrus",    "L",
  "G_Occipital_Inf_R",       "Parietal and occipital regions",  "Inferior occipital gyrus, right",                      "Inferior occipital gyrus",    "R",
  "G_Fusiform_L",            "Parietal and occipital regions",  "Fusiform gyrus, left",                                 "Fusiform gyrus",              "L",
  "G_Fusiform_R",            "Parietal and occipital regions",  "Fusiform gyrus, right",                                "Fusiform gyrus",              "R",
  
  # Subcortical regions
  "G_Caudate_L",             "Subcortical regions",             "Caudate nucleus, left",                                "Caudate nucleus",             "L",
  "G_Caudate_R",             "Subcortical regions",             "Caudate nucleus, right",                               "Caudate nucleus",             "R",
  "G_Putamen_L",             "Subcortical regions",             "Putamen, left",                                        "Putamen",                     "L",
  "G_Putamen_R",             "Subcortical regions",             "Putamen, right",                                       "Putamen",                     "R",
  "G_Pallidum_L",            "Subcortical regions",             "Globus pallidus, left",                                "Globus pallidus",             "L",
  
  # White matter lesions
  "Lesion_Volume_ml",        "White matter lesions",            "Lesion volume (mL)",                                   "Lesion volume (mL)",          NA,
  "Number_of_Lesions",       "White matter lesions",            "Number of lesions",                                    "Number of lesions",           NA
)

# Desired section order
section_levels <- c(
  "Frontal regions",
  "Limbic and paralimbic regions",
  "Temporal regions",
  "Parietal and occipital regions",
  "Subcortical regions",
  "White matter lesions"
)

# Within-section base structure order (controls the anatomical ordering)
base_order <- tibble::tribble(
  ~section,                          ~base_id,                                  ~base_rank,
  "Frontal regions",                 "Precentral gyrus",                         1,
  "Frontal regions",                 "Supplementary motor area",                 2,
  "Frontal regions",                 "Superior frontal gyrus",                   3,
  "Frontal regions",                 "Superior frontal gyrus (orbital part)",    4,
  "Frontal regions",                 "Middle frontal gyrus",                     5,
  "Frontal regions",                 "Middle frontal gyrus (orbital part)",      6,
  "Frontal regions",                 "Inferior frontal gyrus (triangular part)", 7,
  "Frontal regions",                 "Inferior frontal gyrus (orbital part)",    8,
  "Frontal regions",                 "Olfactory cortex",                         9,
  "Frontal regions",                 "Superior frontal gyrus (medial)",         10,
  "Frontal regions",                 "Medial orbitofrontal cortex",             11,
  "Frontal regions",                 "Gyrus rectus",                             12,
  
  "Limbic and paralimbic regions",   "Insula",                                   1,
  "Limbic and paralimbic regions",   "Anterior cingulate cortex",                2,
  "Limbic and paralimbic regions",   "Middle cingulate cortex",                  3,
  "Limbic and paralimbic regions",   "Hippocampus",                               4,
  "Limbic and paralimbic regions",   "Parahippocampal gyrus",                     5,
  "Limbic and paralimbic regions",   "Amygdala",                                  6,
  
  "Temporal regions",                "Heschl's gyrus",                            1,
  "Temporal regions",                "Superior temporal gyrus",                   2,
  "Temporal regions",                "Temporal pole (superior)",                  3,
  "Temporal regions",                "Middle temporal gyrus",                     4,
  "Temporal regions",                "Temporal pole (middle)",                    5,
  "Temporal regions",                "Inferior temporal gyrus",                   6,
  
  "Parietal and occipital regions",  "Postcentral gyrus",                         1,
  "Parietal and occipital regions",  "Superior parietal lobule",                  2,
  "Parietal and occipital regions",  "Inferior parietal lobule",                  3,
  "Parietal and occipital regions",  "Supramarginal gyrus",                       4,
  "Parietal and occipital regions",  "Angular gyrus",                             5,
  "Parietal and occipital regions",  "Precuneus",                                 6,
  "Parietal and occipital regions",  "Calcarine cortex",                          7,
  "Parietal and occipital regions",  "Lingual gyrus",                             8,
  "Parietal and occipital regions",  "Superior occipital gyrus",                  9,
  "Parietal and occipital regions",  "Middle occipital gyrus",                   10,
  "Parietal and occipital regions",  "Inferior occipital gyrus",                 11,
  "Parietal and occipital regions",  "Fusiform gyrus",                           12,
  
  "Subcortical regions",             "Caudate nucleus",                            1,
  "Subcortical regions",             "Putamen",                                    2,
  "Subcortical regions",             "Globus pallidus",                             3,
  
  "White matter lesions",            "Lesion volume (mL)",                          1,
  "White matter lesions",            "Number of lesions",                           2
)

#*******************
# 2) Build formatted table
#*******************
tab_core <- significant_rows %>%
  left_join(aal_lookup, by = "exposure") %>%
  left_join(base_order, by = c("section", "base_id")) %>%
  mutate(
    section = factor(section, levels = section_levels),
    hemi_order = case_when(hemi == "L" ~ 1L, hemi == "R" ~ 2L, TRUE ~ 3L),
    
    OR_fmt = sprintf("%.2f", OR),
    CI_fmt = paste0(sprintf("%.2f", OR_low95), "–", sprintf("%.2f", OR_high95)),
    z_fmt  = sprintf("%.2f", z),
    p_fmt  = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))
  ) %>%
  arrange(section, base_rank, hemi_order) %>%
  transmute(
    `Brain region (AAL)` = brain_region_aal,
    OR = OR_fmt,
    `95% CI` = CI_fmt,
    z = z_fmt,
    `p value` = p_fmt,
    section = as.character(section) # keep for inserting section headers
  )

# Safety check: exposures not mapped
unmapped <- significant_rows %>%
  anti_join(aal_lookup, by = "exposure") %>%
  distinct(exposure)

if (nrow(unmapped) > 0) {
  warning("Unmapped exposures found:\n", paste(unmapped$exposure, collapse = ", "))
}

#*******************
# 3) Insert section header rows (same columns, blank stats)
#*******************
add_section_headers <- function(df) {
  out <- list()
  
  for (sec in section_levels) {
    sub <- df %>%
      dplyr::filter(section == sec) %>%
      dplyr::select(-section)
    
    if (nrow(sub) == 0) next
    
    header <- tibble::tibble(
      `Brain region (AAL)` = sec,
      OR = "",
      `95% CI` = "",
      z = "",
      `p value` = "",
      .is_header = TRUE
    )
    
    sub <- sub %>% dplyr::mutate(.is_header = FALSE)
    
    out[[length(out) + 1]] <- dplyr::bind_rows(header, sub)
  }
  
  dplyr::bind_rows(out)
}

tab_final <- add_section_headers(tab_core)

#*******************
# 4) Flextable + export to Word
#*******************
ft <- flextable(tab_final %>% dplyr::select(-.is_header))

# Bold the section header rows
ft <- bold(ft, i = which(tab_final$.is_header), bold = TRUE)

# Optional: alignment
ft <- align(ft, j = 1, align = "left", part = "all")
ft <- align(ft, j = 2:5, align = "center", part = "all")

# Optional: auto-fit
ft <- autofit(ft)

# Export
doc_path <- "significant_rows_table.docx"

doc <- read_docx() %>%
  body_add_par("Significant associations (AAL regions)", style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = doc_path)




#*******************************************************************************
#### 5. BINARY LOGISTIC MODELS FOR FUNCTIONAL NETWORK STRENGTHS (OBH vs GBH) ####
#*******************************************************************************
# Goal:
#   Outcome  = General Brain Health (GBH) vs Optimal Brain Health (OBH)
#   Predictors = functional network strengths (columns 169 to 178 in df)
#     DefaultMode, Frontoparietal, DorsalAttention, VentralAttention,
#     Limbic, Somatomotor, Visual, Subcortical, Salience, Interoception
#
# Models:
#   - One exposure at a time (individual models)
#   - Unadjusted models
#   - Adjusted models (by adjust_vars defined above)
#
# Interpretation (given the outcome coding below):
#   OR > 1 => higher odds of being GBH (worse brain health)
#   OR < 1 => lower odds of being GBH (i.e., more likely OBH)
#*******************************************************************************

#*******************
##### 5.1. Ensure BINARY outcome exists in df: OBH vs GBH #####
#*******************

# IMPORTANT: glm with a 2-level factor models the log-odds of the SECOND level.
# Here we set levels = c("OBH","GBH") so the modeled event is "GBH".
# Any other groups (e.g., BHD) are set to NA and dropped by na.omit.
df$obh_gbh_bin <- factor(
  dplyr::case_when(
    df$tipo_grupo == "OBH" ~ "OBH",
    df$tipo_grupo == "GBH" ~ "GBH",
    TRUE                   ~ NA_character_
  ),
  levels = c("OBH", "GBH")
)

#*******************
##### 5.2. Select network predictors (no scaling) #####
#*******************
network_preds_all <- names(df)[169:178]

df_net <- df

df_net[network_preds_all] <- lapply(
  df_net[network_preds_all],
  function(x) as.numeric(as.character(x))
)

network_preds_final <- network_preds_all[
  sapply(
    df_net[network_preds_all],
    function(x) length(unique(stats::na.omit(x))) > 1
  )
]

#*******************
###### 5.2.1. Helper: fit a single BINARY logistic model and extract OR + 95% CI ######
#*******************
fit_single_glm_network <- function(
    exposure,
    adjust = NULL,
    data,
    outcome = "obh_gbh_bin"
) {
  exposure <- as.character(exposure)[1]
  rhs_terms <- c(exposure, adjust)
  rhs_terms <- rhs_terms[!is.na(rhs_terms) & rhs_terms != ""]
  
  form_str <- paste(outcome, "~", paste(rhs_terms, collapse = " + "))
  form <- as.formula(form_str)
  
  model <- glm(
    formula   = form,
    data      = data,
    family    = binomial(link = "logit"),
    na.action = na.omit
  )
  
  aic_val <- AIC(model)
  bic_val <- BIC(model)
  
  r2_mcfadden <- tryCatch({
    null_model <- update(model, . ~ 1)
    1 - (as.numeric(logLik(model)) / as.numeric(logLik(null_model)))
  }, error = function(e) NA_real_)
  
  coefs <- summary(model)$coefficients
  
  res <- data.frame(
    term      = rownames(coefs),
    estimate  = coefs[, "Estimate"],
    std.error = coefs[, "Std. Error"],
    z         = coefs[, "z value"],
    row.names = NULL
  ) %>%
    dplyr::mutate(
      p_value    = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
      OR         = exp(estimate),
      OR_low95   = exp(estimate - 1.6448536269514722 * std.error),
      OR_high95  = exp(estimate + 1.6448536269514722 * std.error),
      exposure   = exposure,
      adjustment = ifelse(
        is.null(adjust) || length(adjust) == 0,
        "none",
        paste(adjust, collapse = " + ")
      ),
      R2         = r2_mcfadden,
      AIC        = aic_val,
      BIC        = bic_val
    ) %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::filter(grepl(paste0("^", exposure), term)) %>%
    dplyr::arrange(p_value) %>%
    dplyr::slice(1)
  
  res %>%
    dplyr::select(
      exposure,
      adjustment,
      term,
      OR,
      OR_low95,
      OR_high95,
      estimate,
      std.error,
      z,
      p_value,
      R2,
      AIC,
      BIC
    )
}

#*******************
##### 5.3. Unadjusted models (networks, original units) #####
#*******************
# ORs are for odds of GBH (vs OBH)
results_network <- purrr::map_dfr(
  network_preds_final,
  ~ fit_single_glm_network(
    exposure = .x,
    adjust   = NULL,
    data     = df_net,
    outcome  = "obh_gbh_bin"
  )
)

#*******************
##### 5.4. Adjusted models (networks, original units) #####
#*******************
results_adjusted_network <- purrr::map_dfr(
  network_preds_final,
  ~ fit_single_glm_network(
    exposure = .x,
    adjust   = adjust_vars,
    data     = df_net,
    outcome  = "obh_gbh_bin"
  )
)

# results_network          -> individual unadjusted models (GBH vs OBH)
# results_adjusted_network -> individual adjusted models (GBH vs OBH)

#*******************
##### 5.5. Forest plot (Adjusted models, OBH vs GBH) #####
#*******************

net_forest <- results_adjusted_network %>%
  dplyr::mutate(
    network_label = dplyr::case_when(
      exposure == "DefaultMode"      ~ "Default mode",
      exposure == "Frontoparietal"   ~ "Frontoparietal",
      exposure == "DorsalAttention"  ~ "Dorsal attention",
      exposure == "VentralAttention" ~ "Ventral attention",
      exposure == "Salience"         ~ "Salience",
      exposure == "Limbic"           ~ "Limbic",
      exposure == "Somatomotor"      ~ "Somatomotor",
      exposure == "Visual"           ~ "Visual",
      exposure == "Subcortical"      ~ "Subcortical",
      exposure == "Interoception"    ~ "Interoception",
      TRUE                           ~ exposure
    ),
    OR_num = as.numeric(OR),
    significance = ifelse(p_value < 0.05, "p < 0.05", "Not significant"),
    or_ci_label  = sprintf("%.2f [%.2f–%.2f]", OR_num, OR_low95, OR_high95)
  ) %>%
  dplyr::filter(!is.na(OR_num), OR_num > 0) %>%
  dplyr::mutate(
    network = forcats::fct_reorder(network_label, OR_num),
    network = forcats::fct_rev(network)
  )

min_ci <- min(net_forest$OR_low95, na.rm = TRUE)
max_ci <- max(net_forest$OR_high95, na.rm = TRUE)
label_pos <- max_ci * 1.7

ggplot(net_forest, aes(x = OR_num, y = network)) +
  geom_vline(
    xintercept = 1,
    linetype   = "dashed",
    linewidth  = 0.4,
    colour     = "grey50"
  ) +
  geom_errorbar(
    aes(xmin = OR_low95, xmax = OR_high95, colour = significance),
    width     = 0,
    linewidth = 0.5
  ) +
  geom_point(
    aes(fill = significance, colour = significance),
    shape  = 21,
    size   = 2.8,
    stroke = 0.6
  ) +
  geom_text(
    aes(x = label_pos, y = network, label = or_ci_label),
    hjust  = 0,
    size   = 4.5,
    colour = "black"
  ) +
  scale_x_log10(
    name   = "Odds ratio (log scale)",
    breaks = c(0.05, 0.25, 1, 3, 8),
    limits = c(min_ci * 0.8, max_ci * 2),
    expand = expansion(mult = c(0.02, 0.40))
  ) +
  scale_fill_manual(
    values = c("Not significant" = "white", "p < 0.05" = "black"),
    name   = NULL
  ) +
  scale_colour_manual(
    values = c("Not significant" = "black", "p < 0.05" = "black"),
    guide  = "none"
  ) +
  labs(y = NULL) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
    axis.text.x        = element_text(size = 8, angle = 0, hjust = 0.5),
    legend.position    = "bottom",
    plot.margin        = margin(t = 5, r = -70, b = 5, l = 5)
  ) +
  coord_cartesian(clip = "off")







#*******************************************************************************
#### 6. TABLE OF OBH VS. GBH ####
#*******************************************************************************

library(dplyr)
library(purrr)
library(tibble)
library(flextable)
library(officer)
library(car)
library(DescTools)

#---------------------------
# 0) Keep columns only up to gds_total
#---------------------------
df <- data
df <- df[, 1:which(names(df) == "gds_total")]

#---------------------------
# 1) Restrict to OBH vs GBH only
#---------------------------
grp1 <- "OBH"   # shown first in table
grp0 <- "GBH"

df$bh_bin <- factor(
  dplyr::case_when(
    df$tipo_grupo == grp1 ~ grp1,
    df$tipo_grupo == grp0 ~ grp0,
    TRUE                  ~ NA_character_
  ),
  levels = c(grp0, grp1)
)

df <- df %>%
  dplyr::filter(!is.na(bh_bin)) %>%
  dplyr::mutate(bh_bin = factor(bh_bin, levels = c(grp0, grp1)))

#---------------------------
# 2) Safety checks
#---------------------------
stopifnot(exists("df"), exists("dictionary"))
stopifnot("bh_bin" %in% names(df))

exclude_vars <- c(
  "record_id", "grupo", "tipo_grupo",
  "pais_nacim", "pais_residen", "demo_age", "demo_sex"
)

#---------------------------
# 3) Identify variables + labels + types
#---------------------------
get_dic_var_names <- function(dic, df) {
  if ("Variable" %in% names(dic)) return(as.character(dic$Variable))
  if ("Column" %in% names(dic)) {
    cols <- dic$Column
    if (is.numeric(cols)) return(names(df)[cols])
    return(as.character(cols))
  }
  stop("dictionary must have either `Variable` or `Column`.")
}

dic_vars <- get_dic_var_names(dictionary, df)
vars_to_use <- intersect(dic_vars, names(df))
vars_to_use <- setdiff(vars_to_use, exclude_vars)

dic_tidy <- dictionary %>%
  dplyr::mutate(.var = get_dic_var_names(dictionary, df)) %>%
  dplyr::select(.var, Type, Label_en) %>%
  dplyr::distinct() %>%
  dplyr::filter(.var %in% vars_to_use) %>%
  dplyr::mutate(Label_en = ifelse(is.na(Label_en) | Label_en == "", .var, Label_en))

#---------------------------
# 4) Domain mapping
#---------------------------
domain_map <- function(v) {
  dplyr::case_when(
    v %in% c("social_index","aislamiento_score","uls_total","msoc_contact") ~ "Social factors",
    v %in% c("work_nigth","msoc_bas","msoc_fin","msoc_fin_rec","msoc_eat","msoc_bal",
             "msoc_ladder_35","msoc_care","msoc_91rev",
             "indice_econosocial","indice_econosocial_0_10","categoria_econosocial") ~ "Economic factors",
    v %in% c("msoc_att","msoc_humi","indice_violencia","z_violencia_score","violencia_score_0_10_r","totalsocial_score") ~ "Adversity and violence",
    v %in% c("education_livingston","aislamiento_social_livingston","visual_livingston","audit_livingston",
             "alcohol_livingston","demo_smoke_livingston","hipertension_livingston","dislipidemia_colesterol_livingston",
             "diabetes_livingston","obesidad_livingston","actividad_fisic_livingston","livingston","tec_livingston") ~ "Clinical and lifestyle factors",
    v %in% c("robustos","prefragiles","fragiles") ~ "Frailty phenotype",
    v %in% c("gds_total","gad_total","depression_livingston") ~ "Affective symptoms",
    TRUE ~ "Other"
  )
}

dic_tidy <- dic_tidy %>%
  dplyr::mutate(Domain = domain_map(.var)) %>%
  dplyr::arrange(
    factor(Domain, levels = c(
      "Social factors","Economic factors","Adversity and violence",
      "Clinical and lifestyle factors","Frailty phenotype","Affective symptoms","Other"
    )),
    Label_en
  )

#---------------------------
# 5) Helpers
#---------------------------
fmt_num <- function(x, digits = 2) formatC(x, format = "f", digits = digits)
fmt_p <- function(p) {
  if (is.na(p)) return("p=NA")
  if (p < 0.001) return("p<0.001")
  paste0("p=", formatC(p, format = "f", digits = 3))
}

boot_se_median <- function(x, R = 1500, seed = 123) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  set.seed(seed)
  meds <- replicate(R, median(sample(x, size = n, replace = TRUE), na.rm = TRUE))
  sd(meds, na.rm = TRUE)
}

shapiro_p <- function(x, max_n = 5000) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  if (n > max_n) x <- sample(x, max_n)
  suppressWarnings(shapiro.test(x)$p.value)
}

#---------------------------
# 6) Summaries/tests (OBH vs GBH)
#---------------------------
analyze_numeric <- function(df, var, label, domain) {
  d <- df %>% dplyr::select(bh_bin, dplyr::all_of(var)) %>% dplyr::filter(!is.na(.data[[var]]))
  x <- suppressWarnings(as.numeric(as.character(d[[var]])))
  g <- d$bh_bin
  keep <- is.finite(x) & !is.na(g)
  x <- x[keep]; g <- g[keep]
  
  if (length(x) < 3 || nlevels(g) != 2) {
    return(tibble::tibble(
      `Domain / Variable` = paste0(domain, " – ", label),
      !!grp1 := NA_character_,
      !!grp0 := NA_character_,
      Statistics = "Insufficient data"
    ))
  }
  
  x_1 <- x[g == grp1]
  x_0 <- x[g == grp0]
  n1 <- length(x_1); n0 <- length(x_0)
  
  p_shap_1 <- shapiro_p(x_1)
  p_shap_0 <- shapiro_p(x_0)
  approx_normal <- !is.na(p_shap_1) && !is.na(p_shap_0) && p_shap_1 > 0.05 && p_shap_0 > 0.05
  
  if (isTRUE(approx_normal)) {
    lev_p <- suppressWarnings(car::leveneTest(x ~ g)[["Pr(>F)"]][1])
    use_equal_var <- !is.na(lev_p) && lev_p > 0.05
    
    tt <- t.test(x ~ g, var.equal = use_equal_var)
    
    m1 <- mean(x_1); se1 <- sd(x_1) / sqrt(n1)
    m0 <- mean(x_0); se0 <- sd(x_0) / sqrt(n0)
    
    stat_txt <- paste0(
      "t=", fmt_num(unname(tt$statistic), 2),
      ", df=", fmt_num(unname(tt$parameter), 1),
      ", ", fmt_p(tt$p.value),
      if (!is.na(lev_p)) paste0(", Levene ", fmt_p(lev_p)) else ""
    )
    
    tibble::tibble(
      `Domain / Variable` = paste0(domain, " – ", label),
      !!grp1 := paste0(fmt_num(m1), " (", fmt_num(se1), ")"),
      !!grp0 := paste0(fmt_num(m0), " (", fmt_num(se0), ")"),
      Statistics = stat_txt
    )
  } else {
    wt <- suppressWarnings(wilcox.test(x ~ g, exact = FALSE))
    med1 <- median(x_1); se1 <- boot_se_median(x_1)
    med0 <- median(x_0); se0 <- boot_se_median(x_0)
    
    stat_txt <- paste0("Wilcoxon W=", fmt_num(unname(wt$statistic), 0), ", ", fmt_p(wt$p.value))
    
    tibble::tibble(
      `Domain / Variable` = paste0(domain, " – ", label),
      !!grp1 := paste0(fmt_num(med1), " (", fmt_num(se1), ")"),
      !!grp0 := paste0(fmt_num(med0), " (", fmt_num(se0), ")"),
      Statistics = stat_txt
    )
  }
}

analyze_binomial <- function(df, var, label, domain) {
  d <- df %>% dplyr::select(bh_bin, dplyr::all_of(var)) %>% dplyr::filter(!is.na(.data[[var]]))
  x <- as.factor(d[[var]])
  g <- d$bh_bin
  
  levs <- levels(x)
  event <- if (all(c("No","Yes") %in% levs)) "Yes" else tail(levs, 1)
  
  tab <- table(g, x)
  if (!(grp1 %in% rownames(tab)) || !(grp0 %in% rownames(tab)) || !(event %in% colnames(tab))) {
    return(tibble::tibble(
      `Domain / Variable` = paste0(domain, " – ", label),
      !!grp1 := NA_character_,
      !!grp0 := NA_character_,
      Statistics = "Insufficient data"
    ))
  }
  
  n1 <- sum(tab[grp1, ]); n0 <- sum(tab[grp0, ])
  c1 <- tab[grp1, event]; c0 <- tab[grp0, event]
  p1 <- 100 * c1 / n1;    p0 <- 100 * c0 / n0
  
  chisq_ok <- TRUE; exp_ok <- TRUE
  suppressWarnings({
    cs <- try(chisq.test(tab), silent = TRUE)
    if (inherits(cs, "try-error")) chisq_ok <- FALSE
    if (chisq_ok) exp_ok <- all(cs$expected >= 5)
  })
  
  stat_txt <- if (!chisq_ok || !exp_ok) {
    ft <- fisher.test(tab)
    paste0("Fisher exact, ", fmt_p(ft$p.value))
  } else {
    paste0("Chi-square X2=", fmt_num(cs$statistic, 2), ", df=", fmt_num(cs$parameter, 0), ", ", fmt_p(cs$p.value))
  }
  
  tibble::tibble(
    `Domain / Variable` = paste0(domain, " – ", label, " (", event, ")"),
    !!grp1 := paste0(c1, " (", fmt_num(p1, 1), "%)"),
    !!grp0 := paste0(c0, " (", fmt_num(p0, 1), "%)"),
    Statistics = stat_txt
  )
}

analyze_categorical <- function(df, var, label, domain, is_ordinal = FALSE) {
  d <- df %>% dplyr::select(bh_bin, dplyr::all_of(var)) %>% dplyr::filter(!is.na(.data[[var]]))
  x <- as.factor(d[[var]])
  g <- d$bh_bin
  
  tab <- table(g, x)
  if (nrow(tab) != 2 || ncol(tab) < 2) {
    return(tibble::tibble(
      `Domain / Variable` = paste0(domain, " – ", label),
      !!grp1 := NA_character_,
      !!grp0 := NA_character_,
      Statistics = "Insufficient data"
    ))
  }
  
  global_test_txt <- NULL
  
  if (isTRUE(is_ordinal) && ncol(tab) >= 3) {
    ca <- try(DescTools::CochranArmitageTest(tab), silent = TRUE)
    if (!inherits(ca, "try-error")) {
      global_test_txt <- paste0("Cochran-Armitage Z=", fmt_num(unname(ca$statistic), 2), ", ", fmt_p(ca$p.value))
    }
  }
  
  if (is.null(global_test_txt)) {
    chisq_ok <- TRUE; exp_ok <- TRUE
    suppressWarnings({
      cs <- try(chisq.test(tab), silent = TRUE)
      if (inherits(cs, "try-error")) chisq_ok <- FALSE
      if (chisq_ok) exp_ok <- all(cs$expected >= 5)
    })
    
    global_test_txt <- if (!chisq_ok || !exp_ok) {
      ft <- fisher.test(tab)
      paste0("Fisher exact, ", fmt_p(ft$p.value))
    } else {
      paste0("Chi-square X2=", fmt_num(cs$statistic, 2), ", df=", fmt_num(cs$parameter, 0), ", ", fmt_p(cs$p.value))
    }
  }
  
  cats <- colnames(tab)
  
  rows <- purrr::map_dfr(cats, function(cat) {
    n1 <- sum(tab[grp1, ]); n0 <- sum(tab[grp0, ])
    c1 <- tab[grp1, cat];   c0 <- tab[grp0, cat]
    p1 <- 100 * c1 / n1;    p0 <- 100 * c0 / n0
    
    tibble::tibble(
      `Domain / Variable` = paste0("    • ", cat),
      !!grp1 := paste0(c1, " (", fmt_num(p1, 1), "%)"),
      !!grp0 := paste0(c0, " (", fmt_num(p0, 1), "%)"),
      Statistics = ""
    )
  })
  
  header <- tibble::tibble(
    `Domain / Variable` = paste0(domain, " – ", label),
    !!grp1 := "",
    !!grp0 := "",
    Statistics = global_test_txt
  )
  
  dplyr::bind_rows(header, rows)
}

#---------------------------
# 7) Assemble final table (FIXED: use .y for Domain)
#---------------------------
table_rows <- dic_tidy %>%
  dplyr::mutate(.is_ordinal = Type %in% c("Categorical ordinal")) %>%
  dplyr::group_by(Domain) %>%
  dplyr::group_modify(~{
    dom <- .y$Domain[[1]]
    
    domain_header <- tibble::tibble(
      `Domain / Variable` = dom,
      !!grp1 := "",
      !!grp0 := "",
      Statistics = ""
    )
    
    var_blocks <- purrr::pmap_dfr(
      .x %>% dplyr::select(.var, Type, Label_en, .is_ordinal),
      function(.var, Type, Label_en, .is_ordinal) {
        if (!.var %in% names(df)) return(NULL)
        
        if (Type == "Numerical") {
          analyze_numeric(df, .var, Label_en, dom)
        } else if (Type == "Binomial") {
          analyze_binomial(df, .var, Label_en, dom)
        } else if (Type %in% c("Categorical nominal", "Categorical ordinal")) {
          analyze_categorical(df, .var, Label_en, dom, is_ordinal = .is_ordinal)
        } else {
          analyze_categorical(df, .var, Label_en, dom, is_ordinal = FALSE)
        }
      }
    )
    
    dplyr::bind_rows(domain_header, var_blocks)
  }) %>%
  dplyr::ungroup()

#---------------------------
# 8) Flextable + export (FIXED: no := ; use do.call)
#---------------------------
ft <- flextable::flextable(table_rows)

domain_header_idx <- which(table_rows[[grp1]] == "" & table_rows[[grp0]] == "" & table_rows$Statistics == "")

ft <- ft %>%
  flextable::bold(i = domain_header_idx, bold = TRUE, part = "body") %>%
  flextable::bg(i = domain_header_idx, bg = "#F2F2F2", part = "body") %>%
  flextable::autofit() %>%
  flextable::align(align = "left", part = "all") %>%
  flextable::align(j = c(grp1, grp0, "Statistics"), align = "center", part = "all")

# Dynamic header labels safely:
header_map <- list(
  `Domain / Variable` = "Domain / Variable",
  Statistics = "Statistics"
)
header_map[[grp1]] <- grp1
header_map[[grp0]] <- grp0

ft <- do.call(flextable::set_header_labels, c(list(x = ft), header_map))

# Export
out_file <- "Table1_OBH_vs_GBH.docx"
# create doc
doc <- officer::read_docx()

# add title
doc <- officer::body_add_par(doc,
                             "Table 1. Descriptive comparison between OBH and GBH",
                             style = "heading 1")

# add flextable (NOTE: from flextable, not officer)
doc <- flextable::body_add_flextable(doc, value = ft)

# save
print(doc, target = "Table1_OBH_vs_GBH.docx")

out_file







#*******************************************************************************
#### 7. INTEGRATIVE MODEL (OBH vs GBH) ####
#   - Uses composites comp_* + non-imaging: gad_total, gds_total,
#     demo_smoke_livingston, msoc_fin_rec
#   - Adjusters: demo_age, education_year
#*******************************************************************************
#
# Interpretation (given the outcome coding below):
#   OR > 1 => higher odds of being GBH (worse brain health)
#   OR < 1 => lower odds of being GBH (i.e., more likely OBH)
#*******************************************************************************


#*******************
# 0) Identify ROI columns (volumetrics) and ensure numeric
#*******************
roi_cols <- names(df)[49:164]
roi_cols <- intersect(roi_cols, names(df))
stopifnot(length(roi_cols) > 0)

df[roi_cols] <- lapply(df[roi_cols], function(x) as.numeric(as.character(x)))

#*******************
# 1) Z-score ROIs across subjects (recommended before averaging)
#*******************
df_z <- df
df_z[roi_cols] <- lapply(df_z[roi_cols], function(x) as.numeric(scale(x)))

#*******************
# 2) Build a LONG dataframe mapping each ROI -> anatomical group
#    (instead of a list)
#*******************
roi_to_group <- tibble::tibble(roi = roi_cols) %>%
  dplyr::mutate(
    group = dplyr::case_when(
      # Frontal
      grepl("^G_Frontal_", roi) | grepl("^G_Rectus_", roi) | grepl("^G_Olfactory_", roi) ~ "frontal",
      
      # Sensorimotor
      grepl("^G_Precentral_", roi) |
        grepl("^G_Postcentral_", roi) |
        grepl("^G_Paracentral_Lob_", roi) |
        grepl("^G_Rolandic_Oper_", roi) |
        grepl("^G_Supp_Motor_Area_", roi) ~ "sensorimotor",
      
      # Parietal
      grepl("^G_Parietal_", roi) |
        grepl("^G_SupraMarginal_", roi) |
        grepl("^G_Angular_", roi) |
        grepl("^G_Precuneus_", roi) ~ "parietal",
      
      # Temporal
      grepl("^G_Temporal_", roi) | grepl("^G_Heschl_", roi) | grepl("^G_Fusiform_", roi) ~ "temporal",
      
      # Occipital
      grepl("^G_Calcarine_", roi) |
        grepl("^G_Cuneus_", roi) |
        grepl("^G_Lingual_", roi) |
        grepl("^G_Occipital_", roi) ~ "occipital",
      
      # Limbic / paralimbic
      grepl("^G_Cingulum_", roi) |
        grepl("^G_Hippocampus_", roi) |
        grepl("^G_ParaHippocampal_", roi) |
        grepl("^G_Amygdala_", roi) |
        grepl("^G_Insula_", roi) ~ "limbic",
      
      # Subcortical
      grepl("^G_Caudate_", roi) |
        grepl("^G_Putamen_", roi) |
        grepl("^G_Pallidum_", roi) |
        grepl("^G_Thalamus_", roi) ~ "subcortical",
      
      # Cerebellum + Vermis
      grepl("^G_Cerebelum_", roi) | grepl("^G_Vermis_", roi) ~ "cerebellum_vermis",
      
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(group)) %>%
  dplyr::arrange(group, roi)

# View the mapping table (roi -> group)
roi_to_group

# Optional: make a WIDE "catalog" so you can visually inspect ROIs per group in columns
roi_catalog_wide <- roi_to_group %>%
  dplyr::group_by(group) %>%
  dplyr::mutate(row = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::select(group, row, roi) %>%
  tidyr::pivot_wider(names_from = group, values_from = roi) %>%
  dplyr::arrange(row)

roi_catalog_wide

#*******************
# 3) Helper: row mean with minimum completeness proportion
#*******************
row_mean_minprop <- function(data, vars, min_prop = 0.70) {
  vars <- intersect(vars, names(data))
  if (length(vars) == 0) return(rep(NA_real_, nrow(data)))
  
  X <- as.matrix(data[, vars, drop = FALSE])
  nonmiss <- rowSums(!is.na(X))
  needed  <- ceiling(min_prop * length(vars))
  
  out <- rowMeans(X, na.rm = TRUE)
  out[nonmiss < needed] <- NA_real_
  out
}

#*******************
# 4) Build composites using roi_to_group (no list needed)
#*******************
min_prop <- 0.70

groups <- sort(unique(roi_to_group$group))

# Create one composite column per group (comp_<group>)
for (g in groups) {
  vars_g   <- roi_to_group %>% dplyr::filter(group == g) %>% dplyr::pull(roi)
  new_name <- paste0("comp_", g)
  
  df_z[[new_name]] <- row_mean_minprop(df_z, vars_g, min_prop = min_prop)
}

# Quick sanity checks
cat("\nROIs per group:\n")
print(table(roi_to_group$group))

cat("\nComposite missingness (% NA):\n")
comp_cols <- grep("^comp_", names(df_z), value = TRUE)
print(round(colMeans(is.na(df_z[comp_cols])) * 100, 1))

# df_z now contains composite columns as separate variables:
# comp_frontal, comp_sensorimotor, comp_parietal, etc.

#*******************
# 5) Build data_aggregate: record_id + comp_* + selected non-imaging + adjusters
#*******************
data_aggregate <- df_z %>%
  dplyr::select(record_id, dplyr::starts_with("comp_")) %>%
  dplyr::left_join(
    df %>%
      dplyr::select(
        record_id,
        gad_total,
        gds_total,
        demo_smoke_livingston,
        msoc_fin_rec,
        demo_age,
        education_year,
        obh_gbh_bin
      ),
    by = "record_id"
  )

#---------------------------- Preconditions -----------------------------------
needed <- c("data_aggregate")
missing <- needed[!vapply(needed, exists, logical(1))]
if (length(missing) > 0) stop("Missing object(s): ", paste(missing, collapse = ", "))

# ---------------------------- 0) Ensure outcome exists -------------------------
if (!("obh_gbh_bin" %in% names(data_aggregate))) {
  stop("obh_gbh_bin not found in data_aggregate. Ensure it exists in df before this section.")
}

# Ensure direction: event = "GBH" (second level in glm)
data_aggregate$obh_gbh_bin <- factor(data_aggregate$obh_gbh_bin, levels = c("OBH", "GBH"))

# ---------------------------- 1) Define predictors ----------------------------
# Imaging composites
comp_vars <- grep("^comp_", names(data_aggregate), value = TRUE)

# Non-imaging variables (requested)
nonimg_vars <- c("gad_total", "gds_total", "demo_smoke_livingston", "msoc_fin_rec")

# Adjusters
adjust_vars <- c("demo_age", "education_year")

# Keep only existing columns (safeguard)
nonimg_vars <- intersect(nonimg_vars, names(data_aggregate))
adjust_vars <- intersect(adjust_vars, names(data_aggregate))
comp_vars   <- intersect(comp_vars,   names(data_aggregate))

preds_integral <- unique(c(adjust_vars, nonimg_vars, comp_vars))

# Drop any predictor with no variability
preds_integral <- preds_integral[
  sapply(data_aggregate[preds_integral], function(x) length(unique(na.omit(x))) > 1)
]

if (length(preds_integral) == 0) stop("No usable predictors found (all constant or missing).")

# ---------------------------- 2) Complete-case dataset -------------------------
vars_needed <- c("obh_gbh_bin", preds_integral)
dat_cc <- data_aggregate %>%
  dplyr::select(dplyr::all_of(vars_needed)) %>%
  dplyr::filter(stats::complete.cases(.))

cat("\nIntegral model predictors:\n")
print(preds_integral)
cat("\nComplete-case N:", nrow(dat_cc), "\n")
cat("Outcome counts:\n")
print(table(dat_cc$obh_gbh_bin))

# ---------------------------- 3) Fit integrative glm ---------------------------
form <- stats::as.formula(
  paste0("obh_gbh_bin ~ ", paste0("`", preds_integral, "`", collapse = " + "))
)

fit_integral <- stats::glm(
  formula   = form,
  data      = dat_cc,
  family    = stats::binomial(link = "logit"),
  na.action = stats::na.omit
)

# ---- Fit statistics (R2, AIC, BIC) ----
aic_integral <- stats::AIC(fit_integral)
bic_integral <- stats::BIC(fit_integral)

r2_integral <- tryCatch({
  null_model <- stats::update(fit_integral, . ~ 1)
  1 - (as.numeric(stats::logLik(fit_integral)) / as.numeric(stats::logLik(null_model)))
}, error = function(e) NA_real_)

# ---------------------------- 4) Extract ORs + 95% CIs -------------------------
coefs <- summary(fit_integral)$coefficients

results_integral <- data.frame(
  term      = rownames(coefs),
  estimate  = coefs[, "Estimate"],
  std.error = coefs[, "Std. Error"],
  z         = coefs[, "z value"],
  row.names = NULL
) %>%
  dplyr::mutate(
    p_value   = 2 * stats::pnorm(abs(z), lower.tail = FALSE),
    OR        = exp(estimate),
    OR_low95  = exp(estimate - 1.96 * std.error),
    OR_high95 = exp(estimate + 1.96 * std.error)
  ) %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate(
    exposure   = term,
    adjustment = paste(preds_integral, collapse = " + "),
    R2         = r2_integral,
    AIC        = aic_integral,
    BIC        = bic_integral
  ) %>%
  dplyr::select(exposure, adjustment, term, OR, OR_low95, OR_high95,
                estimate, std.error, z, p_value, R2, AIC, BIC)

# ---------------------------- 5) Apparent ROC / AUC ----------------------------
p_hat <- stats::predict(fit_integral, type = "response")

roc_app <- pROC::roc(
  response  = dat_cc$obh_gbh_bin,
  predictor = p_hat,
  levels    = c("OBH", "GBH"),
  direction = "<",
  quiet     = TRUE
)

auc_app <- as.numeric(pROC::auc(roc_app))
ci_app  <- as.numeric(pROC::ci.auc(roc_app, method = "delong"))

cat("\nApparent (in-sample) AUC (event = GBH):\n")
cat("AUC =", round(auc_app, 3), "\n")
cat("95% CI (DeLong) = [", round(ci_app[1], 3), ", ", round(ci_app[3], 3), "]\n", sep = "")

# ---------------------------- 6) Repeated stratified CV AUC + pooled OOF ROC ----
set.seed(123)
k_folds <- 10
n_reps  <- 5

make_strat_folds <- function(y, k) {
  idx0 <- which(y == "OBH")
  idx1 <- which(y == "GBH")
  idx0 <- sample(idx0); idx1 <- sample(idx1)
  folds0 <- split(idx0, rep(1:k, length.out = length(idx0)))
  folds1 <- split(idx1, rep(1:k, length.out = length(idx1)))
  folds <- vector("list", k)
  for (i in 1:k) folds[[i]] <- c(folds0[[i]], folds1[[i]])
  folds
}

cv_auc <- c()
oof_prob_mat <- matrix(NA_real_, nrow = nrow(dat_cc), ncol = n_reps)

for (r in 1:n_reps) {
  folds <- make_strat_folds(dat_cc$obh_gbh_bin, k_folds)
  
  for (k in 1:k_folds) {
    test_idx  <- folds[[k]]
    train_idx <- setdiff(seq_len(nrow(dat_cc)), test_idx)
    
    fit_cv <- tryCatch(
      stats::glm(formula = form, data = dat_cc[train_idx, , drop = FALSE], family = stats::binomial()),
      error = function(e) NULL
    )
    if (is.null(fit_cv)) next
    
    p_test <- tryCatch(
      stats::predict(fit_cv, newdata = dat_cc[test_idx, , drop = FALSE], type = "response"),
      error = function(e) rep(NA_real_, length(test_idx))
    )
    
    oof_prob_mat[test_idx, r] <- p_test
  }
  
  keep <- !is.na(oof_prob_mat[, r])
  if (sum(keep) > 10) {
    roc_r <- pROC::roc(
      response  = dat_cc$obh_gbh_bin[keep],
      predictor = oof_prob_mat[keep, r],
      levels    = c("OBH", "GBH"),
      direction = "<",
      quiet     = TRUE
    )
    cv_auc <- c(cv_auc, as.numeric(pROC::auc(roc_r)))
  }
}

oof_prob <- rowMeans(oof_prob_mat, na.rm = TRUE)
keep_oof <- !is.na(oof_prob)

roc_oof <- pROC::roc(
  response  = dat_cc$obh_gbh_bin[keep_oof],
  predictor = oof_prob[keep_oof],
  levels    = c("OBH", "GBH"),
  direction = "<",
  quiet     = TRUE
)

auc_oof <- as.numeric(pROC::auc(roc_oof))

cv_mean <- mean(cv_auc, na.rm = TRUE)
cv_sd   <- sd(cv_auc, na.rm = TRUE)
cv_ci   <- quantile(cv_auc, probs = c(0.025, 0.975), na.rm = TRUE)

cat("\nRepeated CV AUC (", n_reps, "x", k_folds, "-fold):\n", sep = "")
cat("Mean AUC =", round(cv_mean, 3), "\n")
cat("SD       =", round(cv_sd, 3), "\n")
cat("Empirical 95% interval = [", round(cv_ci[1], 3), ", ", round(cv_ci[2], 3), "]\n", sep = "")
cat("Pooled out-of-fold AUC =", round(auc_oof, 3), "\n")

# ---------------------------- 7) Classic ROC plot ------------------------------
plot(roc_app, legacy.axes = TRUE, main = "ROC curve (event = GBH)",
     xlab = "1 - Specificity", ylab = "Sensitivity", lwd = 2)
plot(roc_oof, add = TRUE, lty = 2, lwd = 2)

legend("bottomright",
       legend = c(
         paste0("Apparent AUC = ", round(auc_app, 3),
                " [", round(ci_app[1], 3), ", ", round(ci_app[3], 3), "]"),
         paste0("Out-of-fold (pooled) AUC = ", round(auc_oof, 3))
       ),
       lty = c(1, 2), lwd = 2, bty = "n")

# results_integral exists as a dataframe with OR/CI/p for the integrative model.



#*******************************************************************************
# Forest plot for the INTEGRATIVE model (results_integral)
# - No facets/categories (single panel)
# - Uses dictionary$Label_en for non-imaging variables
# - Creates custom labels for comp_* composites
# - Fixes exposures like "demo_smoke_livingstonYes" -> maps to base variable for labeling
#*******************************************************************************

#----------------------------- 1) Build label map --------------------------------
dict_labels <- dictionary %>%
  dplyr::select(Variable, Label_en) %>%
  dplyr::distinct() %>%
  dplyr::rename(exposure_base = Variable, label = Label_en)

comp_labels <- tibble::tibble(
  exposure_base = c(
    "comp_cerebellum_vermis",
    "comp_frontal",
    "comp_limbic",
    "comp_occipital",
    "comp_parietal",
    "comp_sensorimotor",
    "comp_subcortical",
    "comp_temporal"
  ),
  label = c(
    "Composite: Cerebellum/Vermis volume",
    "Composite: Frontal volume",
    "Composite: Limbic volume",
    "Composite: Occipital volume",
    "Composite: Parietal volume",
    "Composite: Sensorimotor volume",
    "Composite: Subcortical volume",
    "Composite: Temporal volume"
  )
)

label_map <- dplyr::bind_rows(dict_labels, comp_labels) %>%
  dplyr::distinct(exposure_base, .keep_all = TRUE)

#----------------------------- 2) Prepare results for plotting -------------------
res_plot <- results_integral %>%
  dplyr::mutate(
    exposure = as.character(exposure),
    
    exposure_base = dplyr::case_when(
      grepl("^comp_", exposure) ~ exposure,
      TRUE ~ {
        dict_vars <- sort(unique(as.character(dictionary$Variable)), decreasing = TRUE)
        base <- vapply(
          exposure,
          function(t) {
            hit <- dict_vars[startsWith(t, dict_vars)]
            if (length(hit) == 0) NA_character_ else hit[1]
          },
          character(1)
        )
        base
      }
    )
  ) %>%
  dplyr::left_join(label_map, by = "exposure_base") %>%
  dplyr::mutate(
    label = dplyr::if_else(is.na(label), exposure, label),
    significance = ifelse(p_value < 0.05, "p < 0.05", "Not significant"),
    significance = factor(significance, levels = c("Not significant", "p < 0.05")),
    ord_value = dplyr::coalesce(estimate, log(OR)),
    or_ci_label = sprintf("%.2f [%.2f–%.2f]", OR, OR_low95, OR_high95)
  ) %>%
  dplyr::filter(
    !is.na(OR), !is.na(OR_low95), !is.na(OR_high95),
    OR > 0, OR_low95 > 0, OR_high95 > 0
  )

# If any exposure appears multiple times (e.g., factor levels), keep the smallest p-value
res_plot <- res_plot %>%
  dplyr::group_by(exposure_base) %>%
  dplyr::arrange(p_value, .by_group = TRUE) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()

# Order y-axis from negative to positive effect (log-odds)
res_plot <- res_plot %>%
  dplyr::arrange(ord_value, label) %>%
  dplyr::mutate(
    label_key = factor(label, levels = rev(unique(label)))  # top-to-bottom
  )

# Position for the OR text to the right of the largest CI
max_ci    <- max(res_plot$OR_high95, na.rm = TRUE)
label_pos <- max_ci * 1.6
res_plot  <- res_plot %>% dplyr::mutate(x_label_pos = label_pos)

#----------------------------- 3) Plot ------------------------------------------
p_integral_forest <- ggplot2::ggplot(res_plot, ggplot2::aes(x = OR, y = label_key)) +
  ggplot2::geom_vline(
    xintercept = 1,
    linetype   = "dashed",
    linewidth  = 0.4,
    colour     = "grey50"
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = OR_low95, xmax = OR_high95, colour = significance),
    width     = 0,
    linewidth = 0.6
  ) +
  ggplot2::geom_point(
    ggplot2::aes(fill = significance, colour = significance),
    shape  = 21,
    size   = 2.6,
    stroke = 0.6
  ) +
  ggplot2::geom_text(
    ggplot2::aes(x = x_label_pos, label = or_ci_label, colour = significance),
    hjust = 0,
    size  = 4.7,
    show.legend = FALSE
  ) +
  ggplot2::scale_x_log10(
    name   = "Odds ratio (log scale)",
    breaks = c(0.25, 0.5, 1, 2, 4),
    expand = ggplot2::expansion(mult = c(0.02, 0.35))
  ) +
  ggplot2::scale_fill_manual(
    name   = NULL,
    values = c("Not significant" = "white", "p < 0.05" = "black")
  ) +
  ggplot2::scale_colour_manual(
    values = c("Not significant" = "black", "p < 0.05" = "black"),
    guide  = "none"
  ) +
  ggplot2::labs(
    title = "Integrative model (event = OBH): adjusted ORs",
    y     = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.minor   = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_line(linewidth = 0.25, colour = "grey90"),
    axis.ticks.y       = ggplot2::element_blank(),
    axis.text.y        = ggplot2::element_text(size = 15.5),
    axis.text.x        = ggplot2::element_text(size = 12),
    legend.position    = "bottom",
    legend.direction   = "horizontal",
    legend.text        = ggplot2::element_text(size = 15),
    plot.title         = ggplot2::element_blank(),
    plot.margin        = ggplot2::margin(t = 5, r = 5, b = 5, l = 5)
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::guides(
    fill = ggplot2::guide_legend(
      override.aes = list(size = 3, colour = "black", shape = 21)
    )
  )

print(p_integral_forest)

