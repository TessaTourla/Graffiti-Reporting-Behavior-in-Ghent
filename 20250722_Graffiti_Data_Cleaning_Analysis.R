
# ===============================================================================
# GRAFFITI PERCEPTION AND REPORTING ANALYSIS SCRIPT
# ===============================================================================
# 
# 1. CREATE PROJECT STRUCTURE:
#    - Create a new folder on your computer called "Graffiti_Perception"
#    - Inside this folder, create a subfolder called "Data"
#    - Place all your data files (SPSS file, shapefiles, etc.) in the "Data" folder
#
# 2. CREATE R PROJECT:
#    - Open RStudio
#    - Go to File > New Project > Existing Directory
#    - Browse to your "Graffiti_Perception" folder and select it
#    - This creates an R project file (.Rproj) in your folder
#
# 3. OPEN AND RUN THE SCRIPT:
#    - Double-click the .Rproj file to open the project in RStudio
#    - Go to File > Open File and select this script
#    - To run the script: either click "Run" button or press Ctrl+Enter for each line
#    - You can also run the entire script by pressing Ctrl+Shift+Enter
#
# 4. EXPECTED DATA FILES IN YOUR "Data" FOLDER:
#    - "vragenlijst Graffiti rapportering_merged.sav" (SPSS survey data)
#    - "statistische-sectoren-gent" folder (containing shapefiles)
#
# ===============================================================================

# ===============================================================================
# SECTION 1: LOAD REQUIRED LIBRARIES
# ===============================================================================
# Note: If any library is not installed, run install.packages("library_name")

# Data manipulation and file handling
library(haven)      # Read SPSS files (.sav)
library(here)       # Handle file paths in projects
library(dplyr)      # Data manipulation (filter, mutate, select, etc.)
library(tibble)     # Modern data frames

# Data visualization
library(ggplot2)    # Create plots and graphs

# Spatial data handling
library(sf)         # Work with shapefiles and spatial data

# Text processing
library(stringr)    # String manipulation

# Statistical analysis
library(psych)      # Reliability analysis (Cronbach's alpha)
library(lme4)       # Mixed-effects models
library(lmerTest)   # P-values for mixed-effects models
library(car)        # Variance Inflation Factor (VIF) tests

# Output formatting
library(flextable)  # Create formatted tables
library(officer)    # Export to Word documents

# ===============================================================================
# SECTION 2: OUTPUT FOLDER MANAGEMENT FUNCTIONS
# ===============================================================================

# Function to create a dated output folder
# This creates a folder with today's date to organize your results
make_folder <- function(date = Sys.Date(), subfolder = NULL) {
  # Convert the provided date to "YYYYMMDD" format
  date_string <- format(date, "%Y%m%d")
  
  # Create the main folder name with the date (e.g., "20250118_output")
  main_folder_name <- paste0(date_string, "_output")
  
  # If a subfolder is specified, append it to the main folder path
  if (!is.null(subfolder)) {
    full_folder_path <- here::here(main_folder_name, subfolder)
  } else {
    full_folder_path <- here::here(main_folder_name)
  }
  
  # Check if the folder exists, and create it if it doesn't
  if (!dir.exists(full_folder_path)) {
    dir.create(full_folder_path, recursive = TRUE)  # Create nested folders if necessary
  }
  
  
  return(full_folder_path)  # Return the folder path to use later
}


# Function to save output files as csv, docx, and png
custom_save <- function(data, folder, name, type = "csv", ...) {
  # Get current date in YYYYMMDD format
  date_prefix <- format(Sys.Date(), "%Y%m%d")
  
  # Construct file name with extension
  filename <- paste0(date_prefix, "_", name, ".", type)
  filepath <- here::here(folder, filename)
  
  # Choose the saving method
  if (type == "csv") {
    readr::write_csv(data, filepath, ...)
    
  } else if (type == "docx") {
    if (inherits(data, "rdocx")) {
      officer:::print.rdocx(data, target = filepath)
    } else if (inherits(data, "flextable")) {
      flextable::save_as_docx(data, path = filepath, ...)
    } else {
      stop("Data must be a 'rdocx' or 'flextable' object for DOCX export.")
    }
    
  } else if (type == "png") {
    if (!inherits(data, "gg")) stop("Data must be a ggplot object for PNG export.")
    ggplot2::ggsave(filename = filepath, plot = data, device = "png", ...)
    
  } else {
    stop("Unsupported file type. Use 'csv', 'docx', or 'png'.")
  }
  
  return(filepath)
}


# NOTE: Each time you run this script, a new output folder will be created with today's date.
#       This ensures that results from different days are saved separately and organized.
# Create the output folder for this analysis session
output_folder <- make_folder()

# ===============================================================================
# SECTION 3: DATA IMPORT AND INITIAL CLEANING
# ===============================================================================

# Load the SPSS survey data
# The 'user_na = FALSE' parameter treats user-defined missing values as regular values

df_raw <- read_sav(here("Data", "vragenlijst Graffiti rapportering_merged.sav"), user_na = FALSE)

# Check the structure of the raw data
# Filter to include only complete responses (Progress = 100%)
# This ensures we only analyze fully completed surveys
df_complete <- df_raw |>
  filter(Progress == 100)

# ===============================================================================
# SECTION 4: VARIABLE CREATION AND RENAMING
# ===============================================================================

# Create age variable and rename key variables for easier analysis
# NOTE: You will need to update the variable names (Q4, Q2, Q6, Q7) to match your actual SPSS variable names
df_clean <- df_complete |>
  mutate(
    # Calculate age from birth year (assuming current year is 2025)
    age = 2025 - as.numeric(QID1),
    
    # Convert sector to numeric for analysis
        Q4 = as.numeric(gsub("A", "", Q4))
  ) |>
  rename(
    # Rename variables to more meaningful names
    sector    = Q4,        # Statistical sector where participant lives
    gender    = Q2,        # Gender of participant
    education = Q6,        # Education level
    housing   = Q7         # Housing situation
  )
df_clean <- df_clean |> 
  mutate(
    gender = haven::as_factor(gender)  # maakt labels bruikbaar in R
  )

df_clean <- df_clean |> 
  mutate(
    housing = haven::as_factor(housing)  # maakt labels bruikbaar in R
  )

df_clean <- df_clean |> 
  mutate(
    education = haven::as_factor(education)  # maakt labels bruikbaar in R
  )

# Recode gender into 3 groups
df_clean <- df_clean |> mutate(
  gender_en = case_when(
    gender == "Man" ~ "Man",
    gender == "Vrouw" ~ "Woman",
    TRUE ~ "Other"
  )
) |> mutate(
  gender_en = factor(gender_en, levels = c("Man", "Woman", "Other"))
)

# Recode housing into 4 English groups
df_clean <- df_clean |> 
  mutate(
    housing_en = case_when(
      housing == "Eigenaar van een (koop)huis/appartement" ~ "Owner",
      housing == "Ik huur een woning/appartement" ~ "Tenant",
      housing == "Ik huur een kot" ~ "Student housing",
      TRUE ~ "Other"
    )
  ) |> 
  mutate(
    housing_en = factor(housing_en, levels = c("Owner", "Tenant", "Student housing", "Other"))
  )

# Recode education into 4 groups
df_clean <- df_clean |> mutate(
  education_en = case_when(
    education == "Diploma Lager Onderwijs" ~ "Elementary School Diploma",
    education == "Diploma Middelbaar" ~ "High School Diploma",
    education == "Diploma Hoger Onderwijs" ~ "Higher Education Diploma",
    TRUE ~ "Other"
  )
) |> mutate(
  education_en = factor(education_en, levels = c("Elementary School Diploma", "High School Diploma", "Higher Education Diploma", "Other"))
)


# Check the cleaned data structure

# ===============================================================================
# SECTION 5: DESCRIPTIVE STATISTICS AND SUMMARY TABLE
# ===============================================================================

# Calculate percentage distributions for categorical variables
gender_table <- prop.table(table(df_clean$gender_en)) * 100
housing_table <- prop.table(table(df_clean$housing_en)) * 100
education_table <- prop.table(table(df_clean$education_en)) * 100


# NOTE : This is just the sample table, you have to include more variables in this table.
# Build comprehensive summary table
# This table will be exported to Word for your report
summary_table <- tibble(
  Variable = c(
    "Age", 
    "Gender", 
    "\tMan", "\tWoman", "\tOther",
    "Housing",
    "\tOwner", "\tTenant", "\tStudent housing", "\tOther",
    "Education",
    "\tElementary school diploma,", "\tHigh school diploma", "\tHigher education diploma", "\tOther",
    "Graffiti perception score", 
    "Street art perception score",
    "Reporting score",
    "Reporting private score",
    "Reporting public score"
  ),
  Min = round(c(
    min(df_clean$age, na.rm = TRUE), 
    rep(NA, 14), 
    min(df_clean$graffiti_perception_sum, na.rm = TRUE), 
    min(df_clean$streetart_perception_sum, na.rm = TRUE), 
    min(df_clean$graffiti_reporting_sum, na.rm = TRUE),
    min(df_clean$graffiti_reporting_private_mean, na.rm = TRUE),
    min(df_clean$graffiti_reporting_public_mean, na.rm = TRUE)
  ), 2),
  Max = round(c(
    max(df_clean$age, na.rm = TRUE), 
    rep(NA, 14), 
    max(df_clean$graffiti_perception_sum, na.rm = TRUE), 
    max(df_clean$streetart_perception_sum, na.rm = TRUE), 
    max(df_clean$graffiti_reporting_sum, na.rm = TRUE),
    max(df_clean$graffiti_reporting_private_mean, na.rm = TRUE),
    max(df_clean$graffiti_reporting_public_mean, na.rm = TRUE)
  ), 2),
  Mean = round(c(
    mean(df_clean$age, na.rm = TRUE), 
    rep(NA, 14), 
    mean(df_clean$graffiti_perception_sum, na.rm = TRUE), 
    mean(df_clean$streetart_perception_sum, na.rm = TRUE), 
    mean(df_clean$graffiti_reporting_sum, na.rm = TRUE),
    mean(df_clean$graffiti_reporting_private_mean, na.rm = TRUE), 
    mean(df_clean$graffiti_reporting_public_mean, na.rm = TRUE)
  ), 2),
  SD = round(c(
    sd(df_clean$age, na.rm = TRUE), 
    rep(NA, 14), 
    sd(df_clean$graffiti_perception_sum, na.rm = TRUE), 
    sd(df_clean$streetart_perception_sum, na.rm = TRUE), 
    sd(df_clean$graffiti_reporting_sum, na.rm = TRUE),
    sd(df_clean$graffiti_reporting_private_mean, na.rm = TRUE),
    sd(df_clean$graffiti_reporting_public_mean, na.rm = TRUE)
  ), 2),
  Percentage = round(c(
   NA, NA,
    gender_table["Man"],
    gender_table["Woman"],
    gender_table["Other"],
   NA,
    housing_table["Owner"],
    housing_table["Tenant"],
    housing_table["Student housing"],
    housing_table["Other"],
   NA,
    education_table["Elementary School Diploma"],
    education_table["High School Diploma"],
    education_table["Higher Education Diploma"],
    education_table["Other"],
    NA, NA, NA, NA, NA
  ), 2)
)

# Format and export summary table to Word
bold_rows <- which(summary_table$Variable %in% c("Age", "Gender", "Housing", "Education", "Graffiti perception score", "Street art perception score", "Reporting score", "Reporting private score", "Reporting public score"))

ft <- flextable(summary_table) |>
  set_header_labels(
    Variable = "",
    Min = "Min",
    Max = "Max",
    Mean = "Mean",
    SD = "SD",
    Percentage = "Percentage"
  ) |>
  bold(i = bold_rows, bold = TRUE) |>
  autofit()

# Save as Word document in output folder
custom_save(ft, output_folder, "summary_table", type = "docx")



# ===============================================================================
# SECTION 6: SCALE CREATION AND RELIABILITY ANALYSIS
# ===============================================================================

# Define the Likert scale items for graffiti perception and reporting
# NOTE: Update these variable names to match your actual SPSS variables
likert_vars_q9 <- paste0("Q9_", c(1, 2, 3, 4, 6, 7))     # Graffiti perception items
likert_vars_q10 <- paste0("Q10_", c(1, 2, 3, 4, 6, 7))   # street art perception items
likert_vars_q18 <- paste0("Q18_", c(1, 2, 3, 4, 5, 6))   # Graffiti reporting items
likert_vars_q17.1 <- paste0("Q17_", c(1, 2))             # graffiti reporting private
likert_vars_q17.2 <- paste0("Q17_", c(3, 4, 5, 6, 7))    # graffiti reporting public

# Identify which variables are labelled (from SPSS) but NOT our Likert items
all_likert_vars <- c(likert_vars_q9, likert_vars_q10, likert_vars_q18, likert_vars_q17.1, likert_vars_q17.2)
labelled_cols_to_convert <- names(df_clean)[
  sapply(df_clean, haven::is.labelled) & !(names(df_clean) %in% all_likert_vars)
]

# Convert labelled variables (except Likert items) to factors
df_clean <- df_clean |>
  mutate(across(all_of(labelled_cols_to_convert), haven::as_factor))

# Convert Likert items to numeric and calculate sum scores
df_clean <- df_clean |>
  mutate(across(
    all_of(all_likert_vars),
    ~ as.numeric(as.character(.))  # Convert to numeric for calculations
  )) |>
  rowwise() |>
  mutate(
    # Sum scores for graffiti perception (lower = more negative perception)
    graffiti_perception_sum = sum(c_across(all_of(likert_vars_q9)), na.rm = TRUE),
    
    # Sum scores for street art perception (lower = more negative perception)
    streetart_perception_sum = sum(c_across(all_of(likert_vars_q10)), na.rm = TRUE),
    
    # Sum scores for graffiti reporting (lower = more likely to report)
    graffiti_reporting_sum  = sum(c_across(all_of(likert_vars_q18)), na.rm = TRUE),
    
    # Mean scores for graffiti reporting private (lower = more likely to report)
    graffiti_reporting_private_mean  = mean(c_across(all_of(likert_vars_q17.1)), na.rm = TRUE),
    
    # Mean scores for graffiti reporting public(lower = more likely to report)
    graffiti_reporting_public_mean  = mean(c_across(all_of(likert_vars_q17.2)), na.rm = TRUE)
  ) |>
  ungroup()


#scatterplot linearity graffiti on house facade and graffiti on store front 
linearity_plot <- ggplot(df_clean, aes(x = Q17_1, y = Q17_2)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    x = "Likelihood of Reporting Graffiti on House Facade", 
    y = "Likelihood of Reporting Graffiti on Store Front",
    title = "Linearity Check: graffiti reporting on house facade vs store front"
  ) +
  theme_minimal()

print(linearity_plot)
custom_save(linearity_plot, output_folder, "graffiti_private_public_linearity", type = "png", width = 8, height = 6, dpi = 1200)
# ===============================================================================
# SECTION 7: RELIABILITY ANALYSIS (CRONBACH'S ALPHA)
# ===============================================================================

# Calculate Cronbach's alpha for internal consistency
# Alpha > 0.8 indicates good reliability
# Alpha > 0.9 indicates excellent reliability


# Reliability for graffiti perception scale ( 0.8874576)
alpha_q9 <- psych::alpha(df_clean |> dplyr::select(all_of(likert_vars_q9)))
alpha_q9$total$raw_alpha

# Reliability for graffiti reporting scale ( 0.9007154)
alpha_q10 <- psych::alpha(df_clean |> dplyr::select(all_of(likert_vars_q10)))
alpha_q10$total$raw_alpha

# Reliability for graffiti reporting private scale ( 0.830398)
alpha_q17.1 <- psych::alpha(df_clean |> dplyr::select(all_of(likert_vars_q17.1)))
alpha_q17.1$total$raw_alpha

# Reliability for graffiti reporting scale ( 0.9342191)
alpha_q17.2 <- psych::alpha(df_clean |> dplyr::select(all_of(likert_vars_q17.2)))
alpha_q17.2$total$raw_alpha

# ===============================================================================
# SECTION 8: MIXED-EFFECTS MODEL FOR GRAFFITI PERCEPTION
# ===============================================================================

# Fit mixed-effects model with random intercept for sector
# This accounts for the fact that people in the same sector might be more similar
model_perception <- lmer(
  graffiti_perception_sum ~ age + gender_en + education_en + housing_en + (1 | sector),
  data = df_clean
)

# Display model summary
summary(model_perception)
# ===============================================================================
# SECTION 8B: MODEL RESULTS REPORTING AND FLEXTABLE EXPORT (PERCEPTION MODEL)
# ===============================================================================
# - Always report the fixed effects (coefficients, SE, t, p) for each predictor.
# - Report the variance of the random effect (sector).
# - Report model fit (marginal and conditional R²).
# - Interpret significant predictors in plain language (see example below).
# - Use the flextable below to create a formatted table for your report.

# Extract fixed effects summary
fixed_effects <- summary(model_perception)$coefficients
fixed_df <- as.data.frame(fixed_effects)
fixed_df$Predictor <- rownames(fixed_df)
fixed_df <- fixed_df[, c("Predictor", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]

# Extract random effect variance
random_var <- as.data.frame(VarCorr(model_perception))$vcov[1]

# Add model fit statistics (marginal and conditional R²)
if (!requireNamespace("MuMIn", quietly = TRUE)) install.packages("MuMIn")
library(MuMIn)
r2 <- MuMIn::r.squaredGLMM(model_perception)

# Create summary table for flextable
model_table <- tibble::tibble(
  Predictor = fixed_df$Predictor,
  Estimate = round(fixed_df$Estimate, 2),
  `Std. Error` = round(fixed_df$`Std. Error`, 2),
  `t value` = round(fixed_df$`t value`, 2),
  `p value` = format.pval(fixed_df$`Pr(>|t|)`, digits = 3, eps = .001)
)

# Create flextable and add random effect and R² as footnotes
ft_model <- flextable::flextable(model_table) |>
  flextable::set_header_labels(
    Predictor = "Predictor",
    Estimate = "Estimate",
    `Std. Error` = "Std. Error",
    `t value` = "t value",
    `p value` = "p value"
  ) |>
  flextable::autofit() |>
  flextable::add_footer_lines(
    values = c(
      paste0("Random intercept variance (sector): ", round(random_var, 3)),
      paste0("Marginal R²: ", round(r2[1], 3), "; Conditional R²: ", round(r2[2], 3))
    )
  )


# Save as Word document in output folder
# IMPORTANT: If you get an error about the file being open, please close the Word document before running this code again.
# R cannot overwrite a .docx file that is open in Microsoft Word or another program.
# Note: body_add_flextable() is from the flextable package, not officer. Use flextable::body_add_flextable.
custom_save(ft_model, output_folder, "model_perception_results", type = "docx")

# Example reporting paragraph for model reporting

# We fitted a linear mixed-effects model to examine predictors of negative graffiti perception, 
# including age, gender, education, and housing, with a random intercept for sector. In this model, 
#higher scores reflect more positive perceptions of graffiti. Older participants reported 
#less negative perceptions (b = -0.05, p = .018), meaning that as age increases, 
#participants tend to view graffiti less negatively. Participants identifying as 'other' gender 
#reported significantly lower perception scores (b = -13.96, p = .016), indicating a more positive or tolerant 
# view of graffiti. In contrast, those renting an apartment reported higher scores (b = 1.74, p = .020), 
# reflecting a more negative perception. The random effect for sector indicated moderate between-sector variability (variance = 0.33)



# Extract random effects (sector-level effects)
ranefs_perception <- ranef(model_perception)$sector |>
  rownames_to_column("sector") |>
  rename(intercept = `(Intercept)`)

# Categorize sectors by their effect size
ranefs_perception <- ranefs_perception |>
  mutate(
    sector = as.numeric(sector),
    effect_category = case_when(
      intercept > 0.05  ~ "Positive",    # Higher than average perception
      intercept < -0.05 ~ "Negative",    # Lower than average perception
      TRUE              ~ "Neutral"      # Close to average
    )
  )

# Create color scheme for the plot
effect_colors <- c(
  "Positive" = "steelblue",
  "Negative" = "firebrick", 
  "Neutral"  = "gray50"
)

# Create and save plot of sector effects
perception_plot <- ggplot(ranefs_perception, aes(x = factor(sector, levels = 0:873), y = intercept, fill = effect_category)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = effect_colors) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Statistical Sector (1–30)",
    y = "Random Intercept (Sector Effect)",
    fill = "Effect",
    title = "Graffiti Perception Deviation by Sector",
    subtitle = "Positive values indicate more positive perception than average"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(perception_plot)
custom_save(perception_plot, output_folder, "graffiti_perception_by_sector", type = "png", width = 8, height = 6, dpi = 1200)



# ===============================================================================
# SECTION 9: MODEL ASSUMPTIONS TESTING FOR PERCEPTION MODEL
# ===============================================================================
# 1. Linearity and Homoscedasticity
# The residuals vs fitted plot should show random scatter (no patterns)
png(file.path(output_folder, paste0(format(Sys.Date(), "%Y%m%d"), "_perception_model_diagnostics.png")), 
    width = 800, height = 600)
plot(model_perception, main = "Residuals vs Fitted: Perception Model")
dev.off()

# 2. Normality of residuals
# Q-Q plot: points should follow the diagonal line
png(file.path(output_folder, paste0(format(Sys.Date(), "%Y%m%d"), "_perception_qq_plot.png")), 
    width = 800, height = 600)
qqnorm(residuals(model_perception), main = "Q-Q Plot: Perception Model Residuals")
qqline(residuals(model_perception), col = "red")
dev.off()

# 3. Linearity check with scatterplot
linearity_plot <- ggplot(df_clean, aes(x = age, y = graffiti_perception_sum)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    x = "Age", 
    y = "Graffiti Perception Sum",
    title = "Linearity Check: Age vs Graffiti Perception"
  ) +
  theme_minimal()

print(linearity_plot)
custom_save(linearity_plot, output_folder, "age_perception_linearity", type = "png", width = 8, height = 6, dpi = 1200)

# 4. Multicollinearity check
vif_values <- vif(lm(graffiti_perception_sum ~ age + gender_en + education_en + housing_en, data = df_clean))
# VIF values > 5 indicate problematic multicollinearity
# (Silent: see object 'vif_values' for results)


# 5. Outliers 
# Model
model_lm <- lm(graffiti_perception_sum ~ age + gender_en + education_en + housing_en, data = df_clean)

# Standardized residuals calculations 
standardized_resid <- rstandard(model_lm)

# Dataframe with residuals and observation numbers
resid_df <- data.frame(
  Observation = 1:length(standardized_resid),
  Standardized_Residual = standardized_resid
)

# Flag: possible outliers
resid_df$Outlier_Flag <- ifelse(abs(resid_df$Standardized_Residual) > 2, "Possible", "OK")
resid_df$Severe_Outlier <- ifelse(abs(resid_df$Standardized_Residual) > 3, "Likely", "")

# Export to CSV
write.csv(
  dw_df,
  file = file.path(output_folder, paste0(format(Sys.Date(), "%Y%m%d"), "_reporting_standardized_residuals_output.csv")),
  row.names = FALSE
)

# 6. Autocorrelation via Durbin-Watson
library(lmtest)
dw_result <- dwtest(model_lm)

dw_df <- data.frame(
  Statistic = as.numeric(dw_result$statistic),
  P_value = as.numeric(dw_result$p.value),
  Method = as.character(dw_result$method),
  Alternative = as.character(dw_result$alternative)
)

# Export to CSV
write.csv(
  dw_df,
  file = file.path(output_folder, paste0(format(Sys.Date(), "%Y%m%d"), "_durbin_watson_perceptions_output.csv")),
  row.names = FALSE
)

# ===============================================================================
# SECTION 10: MIXED-EFFECTS MODEL FOR GRAFFITI REPORTING
# ===============================================================================
# Fit mixed-effects model for reporting behavior
model_reporting <- lmer(
  graffiti_reporting_sum ~ age + graffiti_perception_sum + gender_en + education_en + housing_en + (1 | sector),
  data = df_clean
)

summary(model_reporting)

# Check if sector effect is meaningful
sector_variance <- as.data.frame(VarCorr(model_reporting))$vcov[1]
# If variance is very small, the random effect may not be necessary
if (sector_variance < 0.01) {
  # Fit model without random effect
  model_reporting_fixed <- lm(
    graffiti_reporting_sum ~ age + graffiti_perception_sum + gender_en + education_en + housing_en,
    data = df_clean
  )
  summary(model_reporting_fixed)
} else {
  # Extract and plot random effects for reporting
  ranefs_reporting <- ranef(model_reporting)$sector |>
    rownames_to_column("sector") |>
    rename(intercept = `(Intercept)`)
  
  ranefs_reporting <- ranefs_reporting |>
    mutate(
      sector = as.numeric(sector),
      effect_category = case_when(
        intercept > 0.05  ~ "Positive",
        intercept < -0.05 ~ "Negative",
        TRUE              ~ "Neutral"
      )
    )
  
  # Create plot for reporting effects
  reporting_plot <- ggplot(ranefs_reporting, aes(x = factor(sector, levels = 0:873), y = intercept, fill = effect_category)) +
    geom_col(width = 0.7) +
    scale_fill_manual(values = effect_colors) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      x = "Statistical Sector (1–30)",
      y = "Random Intercept (Sector Effect)",
      fill = "Effect",
      title = "Graffiti Reporting Deviation by Sector",
      subtitle = "Positive values indicate more reporting than average"
    ) +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(reporting_plot)
  custom_save(reporting_plot, output_folder, "graffiti_reporting_by_sector", type = "png", width = 8, height = 6, dpi = 1200)
}

# ===============================================================================
# SECTION 11: MODEL ASSUMPTIONS TESTING FOR REPORTING MODEL
# ===============================================================================
# Use the appropriate model (with or without random effects)
reporting_model_final <- if (exists("model_reporting_fixed")) model_reporting_fixed else model_reporting

# 1. Linearity and Homoscedasticity
png(file.path(output_folder, paste0(format(Sys.Date(), "%Y%m%d"), "_reporting_model_diagnostics.png")), 
    width = 800, height = 600)
plot(reporting_model_final, main = "Residuals vs Fitted: Reporting Model")
dev.off()

# 2. Normality of residuals
png(file.path(output_folder, paste0(format(Sys.Date(), "%Y%m%d"), "_reporting_qq_plot.png")), 
    width = 800, height = 600)
qqnorm(residuals(reporting_model_final), main = "Q-Q Plot: Reporting Model Residuals")
qqline(residuals(reporting_model_final), col = "red")
dev.off()

# 3. Linearity check
reporting_linearity_plot <- ggplot(df_clean, aes(x = age, y = graffiti_reporting_sum)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(
    x = "Age", 
    y = "Graffiti Reporting Sum",
    title = "Linearity Check: Age vs Graffiti Reporting"
  ) 
  theme_minimal()

print(reporting_linearity_plot)
custom_save(reporting_linearity_plot, output_folder, "age_reporting_linearity", type = "png", width = 8, height = 6, dpi = 1200)

# 4. Multicollinearity check
vif_values_reporting <- vif(lm(graffiti_reporting_sum ~ age + graffiti_perception_sum + gender_en + education_en + housing_en, data = df_clean))
# (Silent: see object 'vif_values_reporting' for results)


# 5. Durbin-Watson test (independence of residuals)
library(lmtest)
model_lm_reporting <- lm(graffiti_reporting_sum ~ age + graffiti_perception_sum + gender_en + education_en + housing_en, data = df_clean)
dw_result_reporting <- dwtest(model_lm_reporting)

# Zet testresultaten om naar data frame
dw_df_reporting <- data.frame(
  Statistic = as.numeric(dw_result_reporting$statistic),
  P_value = as.numeric(dw_result_reporting$p.value),
  Method = as.character(dw_result_reporting$method),
  Alternative = as.character(dw_result_reporting$alternative)
)

# Export naar CSV
write.csv(
  dw_df, 
  file = file.path(output_folder, paste0(format(Sys.Date(), "%Y%m%d"), "_durbin_watson_reporting_output.csv")), 
  row.names = FALSE
)

# 6. Outliers 

# Lineair model gebruiken (zonder random effects)
model_lm_reporting <- lm(graffiti_reporting_sum ~ age + graffiti_perception_sum + gender_en + education_en + housing_en, data = df_clean)

# Standardized residuals berekenen
standardized_resid_reporting <- rstandard(model_lm_reporting)

# Dataframe met observaties en residuen
resid_df_reporting <- data.frame(
  Observation = 1:length(standardized_resid_reporting),
  Standardized_Residual = standardized_resid_reporting
)

# Outlier flags toevoegen
resid_df_reporting$Outlier_Flag <- ifelse(abs(resid_df_reporting$Standardized_Residual) > 2, "Possible", "OK")
resid_df_reporting$Severe_Outlier <- ifelse(abs(resid_df_reporting$Standardized_Residual) > 3, "Likely", "")

# Exporteren naar CSV
write.csv(
  resid_df_reporting, 
  file = file.path(output_folder, paste0(format(Sys.Date(), "%Y%m%d"), "_reporting_standardized_residuals_output.csv")), 
  row.names = FALSE
)

# ===============================================================================
# SECTION 12: SPATIAL DATA VISUALIZATION
# ===============================================================================
# Load shapefile for Ghent statistical sectors
# NOTE: Make sure the shapefile folder is in your Data directory
sf_gent_sectors <- st_read(here("Data", "statistische-sectoren-gent", "statistische-sectoren-gent.shp"))

# Filter to sectors starting with "A" (presumably a specific area of interest)
sf_gent_A <- sf_gent_sectors |>
  filter(str_starts(sectorcode, "A"))

# Remove excluded sectors (you may need to adjust this list based on your data)
excluded_sectors <- c(
  "A35", "A33", "A40", "A42", "A46", "A51", "A342",
  "A410", "A485", "A492", "A521", "A932"
)

sf_gent_filtered <- sf_gent_A |>
  filter(!sectorcode %in% excluded_sectors)

# Create map with both sector codes and sector names
map_plot <- ggplot(sf_gent_filtered) +
  geom_sf(fill = "lightblue", color = "white", size = 0.5) +
  geom_sf_text(aes(label = paste0(sectorcode, "\n")), size = 3, color = "black") +
  labs(
    title = "Study Area: Statistical Sectors in Ghent",
    subtitle = paste("Analysis includes", nrow(sf_gent_filtered), "sectors")
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(map_plot)
custom_save(map_plot, output_folder, "study_area_map", type = "png", width = 8, height = 6, dpi = 1200)

# ===============================================================================
# SECTION 13: SAVE FINAL DATASETS
# ===============================================================================
# Save the cleaned dataset for future use
custom_save(df_clean, output_folder, "cleaned_graffiti_data", type = "csv")

# Save model results summary
model_summary <- data.frame(
  Model = c("Perception", "Reporting"),
  N_Observations = c(nrow(df_clean), nrow(df_clean)),
  Cronbach_Alpha = c(round(alpha_q9$total$raw_alpha, 3), round(alpha_q10$total$raw_alpha, 3)),
  Random_Effect_Variance = c(
    as.data.frame(VarCorr(model_perception))$vcov[1],
    if(exists("model_reporting_fixed")) 0 else as.data.frame(VarCorr(model_reporting))$vcov[1]
  )
)
custom_save(model_summary, output_folder, "model_summary", type = "csv")

# ===============================================================================
# SECTION 14: SAVE ALL MODEL RESULTS AND DIAGNOSTICS (CSV & FLEXTABLE)
# ===============================================================================
# This section saves all model results (fixed effects, random effects, R2, VIF, etc.)
# as CSV and creates a flextable for each model, including correlation matrix and diagnostics.

# --- 1. Perception Model: Fixed Effects Table ---
perception_fixed <- summary(model_perception)$coefficients
perception_fixed_df <- as.data.frame(perception_fixed)
perception_fixed_df$Predictor <- rownames(perception_fixed_df)
perception_fixed_df <- perception_fixed_df[, c("Predictor", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]

# Round all numeric columns to 3 decimal places
perception_fixed_df <- perception_fixed_df |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))
custom_save(perception_fixed_df, output_folder, "perception_model_fixed_effects", type = "csv")

# --- 2. Perception Model: Random Effects Table ---
perception_random <- as.data.frame(VarCorr(model_perception))

# Round all numeric columns to 3 decimal places
perception_random <- perception_random |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))
custom_save(perception_random, output_folder, "perception_model_random_effects", type = "csv")


# --- 3. Perception Model: VIF Table ---
# Robust VIF table creation: only create/save if both names and values are present and lengths match
if (!is.null(vif_values) && length(vif_values) > 0 && length(names(vif_values)) == length(as.numeric(vif_values)) && length(names(vif_values)) > 0) {
  vif_perception_df <- data.frame(Variable = names(vif_values), VIF = as.numeric(vif_values))
  custom_save(vif_perception_df, output_folder, "perception_model_vif", type = "csv")
} else if (!is.null(vif_values) && length(as.numeric(vif_values)) > 0 && (is.null(names(vif_values)) || length(names(vif_values)) == 0)) {
  # VIF values present but no names: assign generic variable names
  vif_perception_df <- data.frame(Variable = paste0("V", seq_along(vif_values)), VIF = as.numeric(vif_values))
  custom_save(vif_perception_df, output_folder, "perception_model_vif", type = "csv")
} else {
  warning("No VIF values calculated for perception model. VIF table will not be saved.")
}



# --- 4. Perception Model: Correlation Matrix ---
cor_vars <- c("age", "graffiti_perception_sum", "gender", "education", "housing")
cor_data <- df_clean[, cor_vars]
# Convert factors to numeric for correlation
cor_data[] <- lapply(cor_data, function(x) if(is.factor(x)) as.numeric(x) else x)
cor_matrix <- round(cor(cor_data, use = "pairwise.complete.obs"), 2)
cor_matrix_df <- as.data.frame(as.table(cor_matrix))
names(cor_matrix_df) <- c("Var1", "Var2", "Correlation")
custom_save(cor_matrix_df, output_folder, "perception_model_correlation_matrix", type = "csv")


# --- 5. Perception Model: Flextable (Fixed Effects) ---
ft_perception <- flextable::flextable(perception_fixed_df) |>
  flextable::set_header_labels(
    Predictor = "Predictor",
    Estimate = "Estimate",
    `Std. Error` = "Std. Error",
    `t value` = "t value",
    `Pr(>|t|)` = "p value"
  ) |>
  flextable::autofit() |>
  flextable::add_footer_lines(
    values = c(
      paste0("Random intercept variance (sector): ", round(perception_random$vcov[1], 3)),
      paste0("Marginal R²: ", round(r2[1], 3), "; Conditional R²: ", round(r2[2], 3))
    )
  )

custom_save(ft_perception, output_folder, "perception_model_flextable", type = "docx")

# --- 6. Reporting Model: Fixed Effects Table ---
reporting_fixed <- summary(reporting_model_final)$coefficients
reporting_fixed_df <- as.data.frame(reporting_fixed)
reporting_fixed_df$Predictor <- rownames(reporting_fixed_df)
reporting_fixed_df <- reporting_fixed_df[, c("Predictor", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]

# Round all numeric columns to 3 decimal places
reporting_fixed_df <- reporting_fixed_df |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

custom_save(reporting_fixed_df, output_folder, "reporting_model_fixed_effects", type = "csv")

# --- 7. Reporting Model: Random Effects Table ---
if (inherits(reporting_model_final, "lmerMod")) {
  reporting_random <- as.data.frame(VarCorr(reporting_model_final))
  custom_save(reporting_random, output_folder, "reporting_model_random_effects", type = "csv")
} else {
  reporting_random <- data.frame() # Empty if not mixed model
}


# --- 8. Reporting Model: VIF Table ---
# Robust VIF table creation: only create/save if both names and values are present and lengths match
if (!is.null(vif_values_reporting) && length(vif_values_reporting) > 0 && length(names(vif_values_reporting)) == length(as.numeric(vif_values_reporting)) && length(names(vif_values_reporting)) > 0) {
  vif_reporting_df <- data.frame(Variable = names(vif_values_reporting), VIF = as.numeric(vif_values_reporting))
  custom_save(vif_reporting_df, output_folder, "reporting_model_vif", type = "csv")
} else if (!is.null(vif_values_reporting) && length(as.numeric(vif_values_reporting)) > 0 && (is.null(names(vif_values_reporting)) || length(names(vif_values_reporting)) == 0)) {
  # VIF values present but no names: assign generic variable names
  vif_reporting_df <- data.frame(Variable = paste0("V", seq_along(vif_values_reporting)), VIF = as.numeric(vif_values_reporting))
  custom_save(vif_reporting_df, output_folder, "reporting_model_vif", type = "csv")
} else {
  warning("No VIF values calculated for reporting model. VIF table will not be saved.")
}

# --- 9. Reporting Model: Correlation Matrix ---
cor_vars_rep <- c("age", "graffiti_reporting_sum", "graffiti_perception_sum" ,"gender", "education", "housing")
cor_data_rep <- df_clean[, cor_vars_rep]
cor_data_rep[] <- lapply(cor_data_rep, function(x) if(is.factor(x)) as.numeric(x) else x)
cor_matrix_rep <- round(cor(cor_data_rep, use = "pairwise.complete.obs"), 2)
cor_matrix_rep_df <- as.data.frame(as.table(cor_matrix_rep))
names(cor_matrix_rep_df) <- c("Var1", "Var2", "Correlation")

# Round all numeric columns to 3 decimal places
cor_matrix_rep_df <- cor_matrix_rep_df |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

custom_save(cor_matrix_rep_df, output_folder, "reporting_model_correlation_matrix", type = "csv")

# --- 10. Reporting Model: Flextable (Fixed Effects) ---
ft_reporting <- flextable::flextable(reporting_fixed_df) |>
  flextable::set_header_labels(
    Predictor = "Predictor",
    Estimate = "Estimate",
    `Std. Error` = "Std. Error",
    `t value` = "t value",
    `Pr(>|t|)` = "p value"
  ) |>
  flextable::autofit()
if (inherits(reporting_model_final, "lmerMod")) {
  ft_reporting <- flextable::add_footer_lines(
    ft_reporting,
    values = c(
      paste0("Random intercept variance (sector): ", round(reporting_random$vcov[1], 3)),
      paste0("Marginal R²: ", round(MuMIn::r.squaredGLMM(reporting_model_final)[1], 3), "; Conditional R²: ", round(MuMIn::r.squaredGLMM(reporting_model_final)[2], 3))
    )
  )
}
custom_save(ft_reporting, output_folder, "reporting_model_flextable", type = "docx")


# --- 11. tables result section 3

library(tidyr)
library(ggplot2)
library(dplyr)

#Q11 

q11_vars <- paste0("Q11") 

df_long <- df_clean %>%
  select(all_of(q11_vars)) %>%
  pivot_longer(cols = everything(),
               names_to = "Series",
               values_to = "Category") %>%
  filter(!is.na(Category))  # verwijder lege rijen

df_long <- df_long %>%
  mutate(Category = case_when(
    Category == "Neen, ik heb geen graffiti gerapporteerd aan één van de bevoegde instanties" ~ "No, I have not reported graffiti to any of the authorities",
    Category == "Ja, ik heb een melding gemaakt bij Stad Gent" ~ "Yes, I made a notification to Stad Gent",
    Category == "Ja, ik heb het gerapporteerd aan de politie" ~ "Yes, I have reported it to the police",
    Category == "Ja, ik heb een melding gemaakt bij de verhuurder" ~ "Yes, I made a notification to the landlord",
    TRUE ~ Category  # behoud rest
  ))

df_long$Category <- str_wrap(df_long$Category, width = 20)

df_long$Category <- factor(df_long$Category, levels = unique(df_long$Category))

df_long$Category <- factor(df_long$Category, levels = sort(unique(df_long$Category)))

df_counts <- df_long %>%
  count(Category, Series)


reportingq11_plot <- ggplot(df_counts, aes(x = Category, y = n, group = Series)) +
  geom_col(fill = "steelblue", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = n, y = n / 2),   # label op de helft van de balkhoogte
            position = position_dodge(width = 0.9),
            color = "Black",
            size = 3) +
  labs(
    x = "Category",
    y = "Count"
  ) +
  theme_minimal() +  # eventueel toevoegen voor strak uiterlijk
  theme(
    panel.grid.major.x = element_blank(),  # grote verticale lijnen weg
    panel.grid.minor.x = element_blank()   # kleine verticale lijnen weg
  )

print(reportingq11_plot)

ggsave(
  filename = file.path(output_folder, paste0(format(Sys.Date(), "%Y%m%d"), "_reporting_authorities.png")),
  plot = reportingq11_plot,
  width = 8,
  height = 6,
  dpi = 1200
)


#Q15

q15_vars <- paste0("Q15_", 1:7)

df_long <- df_clean %>%
  select(all_of(q15_vars)) %>%
  pivot_longer(cols = everything(),
               names_to = "Series",
               values_to = "Category") %>%
  filter(!is.na(Category))  # verwijder NA rijen

df_long <- df_long %>%
  mutate(Category = case_when(
    Category == "Het type van graffiti" ~ "The type of graffiti",
    Category == "De locatie waar graffiti aanwezig is" ~ "The location of the graffiti",
    Category == "De grote hoeveelheid aan graffiti die al aanwezig is" ~ "The amount of graffiti already present",
    Category == "De graffiti viel op" ~ "The graffiti stood out",
    Category == "Graffiti is lelijk" ~ "Graffiti is ugly",
    Category == "Het is een misdrijf en het is een goede daad om het te melden" ~ "It is a crime and it is the right thing to do",
    Category == "Andere:" ~ "Other",
    TRUE ~ Category  # behoud rest
  ))

df_long$Category <- str_wrap(df_long$Category, width = 20)

df_long$Category <- factor(df_long$Category, levels = unique(df_long$Category))

df_long$Category <- factor(df_long$Category, levels = sort(unique(df_long$Category)))

df_counts <- df_long %>%
  count(Category, Series)

# Plot met labels exact in het midden van de balken
motivators_plot <- ggplot(df_counts, aes(x = Category, y = n, group = Series)) +
  geom_col(fill = "steelblue", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = n, y = n / 2),   # label op de helft van de balkhoogte
            position = position_dodge(width = 0.9),
            color = "Black",
            size = 3) +
  labs(
    x = "Category",
    y = "Count"
  ) +
  theme_minimal() +  # eventueel toevoegen voor strak uiterlijk
  theme(
    panel.grid.major.x = element_blank(),  # grote verticale lijnen weg
    panel.grid.minor.x = element_blank()   # kleine verticale lijnen weg
  )

print(motivators_plot)

ggsave(
  filename = file.path(output_folder, paste0(format(Sys.Date(), "%Y%m%d"), "_motivators_reporting.png")),
  plot = motivators_plot,
  width = 8,
  height = 6,
  dpi = 1200
)


#Q16

q16_vars <- paste0("Q16_", 1:5)

df_long <- df_clean %>%
  select(all_of(q16_vars)) %>%
  pivot_longer(cols = everything(),
               names_to = "Series",
               values_to = "Category") %>%
  filter(!is.na(Category))  # verwijder NA rijen

df_long <- df_long %>%
  mutate(Category = case_when(
    Category == "Ik weet niet wie ik moet contacteren" ~ "I do not know who to contact",
    Category == "Ik denk niet dat het een impact zal hebben" ~ "I do not think it will have an impact",
    Category == "Ik denk dat het deel uit maakt van een cultuur" ~ "I think it is part of a culture",
    Category == "Er zijn andere dingen waar ik me zorgen over maak" ~ "I have better things to worry about",
    Category == "Andere:" ~ "Other",
    TRUE ~ Category  # behoud rest
  ))

df_long$Category <- str_wrap(df_long$Category, width = 20)

category_order <- df_long %>%
  group_by(Series) %>%
  summarise(Category = unique(Category), .groups = "drop") %>%
  arrange(factor(Series, levels = q16_vars)) %>%
  pull(Category)

df_long$Category <- factor(df_long$Category, levels = category_order)


df_counts <- df_long %>%
  count(Category, Series)

# Plot met labels exact in het midden van de balken
demotivators_plot <- ggplot(df_counts, aes(x = Category, y = n, group = Series)) +
  geom_col(fill = "steelblue", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = n, y = n / 2),   # label op de helft van de balkhoogte
            position = position_dodge(width = 0.9),
            color = "Black",
            size = 3) +
  labs(
    x = "Category",
    y = "Count"
  ) +
  theme_minimal() +  # eventueel toevoegen voor strak uiterlijk
  theme(
    panel.grid.major.x = element_blank(),  # grote verticale lijnen weg
    panel.grid.minor.x = element_blank()   # kleine verticale lijnen weg
  )

print(demotivators_plot)

ggsave(
  filename = file.path(output_folder, paste0(format(Sys.Date(), "%Y%m%d"), "_demotivators_reporting.png")),
  plot = demotivators_plot,
  width = 8,
  height = 6,
  dpi = 1200
)


#Q19

q19_vars <- paste0("Q19")


df_long <- df_clean %>%
  select(all_of(q19_vars)) %>%
  pivot_longer(cols = everything(),
               names_to = "Series",
               values_to = "Category") %>%
  filter(!is.na(Category))  # verwijder lege rijen

df_long <- df_long %>%
  mutate(Category = case_when(
    Category == "Ja" ~ "Yes",
    Category == "Neen" ~ "No",
    TRUE ~ Category  # behoud rest
  ))


df_long$Category <- str_wrap(df_long$Category, width = 20)

df_long$Category <- factor(df_long$Category, levels = unique(df_long$Category))

df_long$Category <- factor(df_long$Category, levels = sort(unique(df_long$Category)))

df_counts <- df_long %>%
  count(Category, Series)


stadgent_plot <- ggplot(df_counts, aes(x = Category, y = n, group = Series)) +
  geom_col(fill = "steelblue", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = n, y = n / 2),   # label op de helft van de balkhoogte
            position = position_dodge(width = 0.9),
            color = "Black",
            size = 3) +
  labs(
    x = "Category",
    y = "Count"
  ) +
  theme_minimal() +  # eventueel toevoegen voor strak uiterlijk
  theme(
    panel.grid.major.x = element_blank(),  # grote verticale lijnen weg
    panel.grid.minor.x = element_blank()   # kleine verticale lijnen weg
  )

print(stadgent_plot)

ggsave(
  filename = file.path(output_folder, paste0(format(Sys.Date(), "%Y%m%d"), "_free_removal.png")),
  plot = stadgent_plot,
  width = 8,
  height = 6,
  dpi = 1200
)

#Q21

q21_vars <- paste0("Q21")   

df_long <- df_clean %>%
  select(all_of(q21_vars)) %>%
  pivot_longer(cols = everything(),
               names_to = "Series",
               values_to = "Category") %>%
  filter(!is.na(Category))  # verwijder lege rijen

df_long <- df_long %>%
  mutate(Category = case_when(
    Category == "Ja" ~ "Yes",
    Category == "Neen" ~ "No",
    TRUE ~ Category  # behoud rest
  ))


df_long$Category <- str_wrap(df_long$Category, width = 20)

df_long$Category <- factor(df_long$Category, levels = unique(df_long$Category))

df_long$Category <- factor(df_long$Category, levels = sort(unique(df_long$Category)))

df_counts <- df_long %>%
  count(Category, Series)


contribution_plot <- ggplot(df_counts, aes(x = Category, y = n, group = Series)) +
  geom_col(fill = "steelblue", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = n, y = n / 2),   # label op de helft van de balkhoogte
            position = position_dodge(width = 0.9),
            color = "Black",
            size = 3) +
  labs(
    x = "Category",
    y = "Count"
  ) +
  theme_minimal() +  # eventueel toevoegen voor strak uiterlijk
  theme(
    panel.grid.major.x = element_blank(),  # grote verticale lijnen weg
    panel.grid.minor.x = element_blank()   # kleine verticale lijnen weg
  )

print(contribution_plot)

ggsave(
  filename = file.path(output_folder, paste0(format(Sys.Date(), "%Y%m%d"), "_contribution_removal.png")),
  plot = contribution_plot,
  width = 8,
  height = 6,
  dpi = 1200
)


#Q23

q23_vars <- paste0("Q23_", 1:8)

df_long <- df_clean %>%
  select(all_of(q23_vars)) %>%
  pivot_longer(cols = everything(),
               names_to = "Series",
               values_to = "Category") %>%
  filter(!is.na(Category))  # verwijder NA rijen

df_long <- df_long %>%
  mutate(Category = case_when(
    Category == "Nultolerantie: elke persoon dat betrapt wordt op graffiti zou direct gestraft moeten worden" ~ "Zero-tolerance: every person caught doing graffiti should be punished",
    Category == "Nultolerantie: elk type graffiti moet direct verwijderd worden" ~ "Zero-tolerance: each type of graffiti should be immediatly removed",
    Category == "Het aanstellen van erkende graffiti artiesten die kunstwerken maken in de graffiti stijl, als manier om illegale graffiti tegen te gaan" ~ "The appointment of recognized graffiti artists who create graffiti artwork as a way to combat illegal graffiti",
    Category == "Meer gebruik maken van muren die beschikbaar worden gesteld zodat graffiti artiesten deze kunnen gebruiken om graffiti te spuiten" ~ "Make more wallls available for graffiti artists to use for spraying graffiti",
    Category == "De overheid moet bepaalde richtlijnen creëren die op basis van de meningen van de bevolking zijn gebaseerd, om graffiti te verwijderen. Hierbij wordt er een systeem gebruik van lage en hoge prioriteit waarbij hoge prioriteit graffiti sneller worden verwi" ~ "The government must create guidelines based on public opinion for graffiti removal: a system of low and high priority graffiti",
    Category == "Eigenaars van gebouwen die weigeren om graffiti gratis te laten verwijderen, moeten gesanctioneerd worden" ~ "Building owners who refuse to allow graffiti to be removed for free, should be sanctioned",
    Category == "Stad Gent zou niet alleen moeten instaan voor het verwijderen van graffiti, andere overheidsinstanties moeten instaan voor de verwijdering op hun eigendom (spoorwegbruggen - > Infrabel, bus- en traminfrastructuur -> De Lijn)" ~ "Stad Gent should not be the only one responsible for removing graffiti, other government agencies should also be responsible for its removal on their property (Infrabel, De Lijn)",
    Category == "Andere:" ~ "Other",
    TRUE ~ Category  # behoud rest
  ))


df_long$Category <- str_wrap(df_long$Category, width = 15)

category_order <- df_long %>%
  group_by(Series) %>%
  summarise(Category = unique(Category), .groups = "drop") %>%
  arrange(factor(Series, levels = q16_vars)) %>%
  pull(Category)

df_long$Category <- factor(df_long$Category, levels = category_order)


df_counts <- df_long %>%
  count(Category, Series)

# Plot met labels exact in het midden van de balken
policy_plot <- ggplot(df_counts, aes(x = Category, y = n, group = Series)) +
  geom_col(fill = "steelblue", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = n, y = n / 2),   # label op de helft van de balkhoogte
            position = position_dodge(width = 0.9),
            color = "Black",
            size = 3) +
  labs(
    x = "Category",
    y = "Count"
  ) +
  theme_minimal() +  # eventueel toevoegen voor strak uiterlijk
  theme(
    panel.grid.major.x = element_blank(),  # grote verticale lijnen weg
    panel.grid.minor.x = element_blank()   # kleine verticale lijnen weg
  )

print(policy_plot)

ggsave(
  filename = file.path(output_folder, paste0(format(Sys.Date(), "%Y%m%d"), "_suggesting_policy.png")),
  plot = policy_plot,
  width = 8,
  height = 6,
  dpi = 1200
)



