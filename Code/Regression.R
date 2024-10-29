# Install and load necessary libraries
library(readxl)
library(writexl)

# Read the dataset
file_path <- "/Users/canaandrinkwater/Desktop/Thesis_Creativity/10252024/Data/imslp/imslp.xlsx"
data <- read_excel(file_path)


# Define key variables to check for missing values
key_vars <- c("birth_cleaned", "work_year_cleaned", "duration_cleaned", "max_downloads")

# Convert necessary columns to numeric
data$birth <- as.numeric(data$birth)
data$work_year_cleaned <- as.numeric(data$work_year_cleaned)
data$duration_cleaned <- as.numeric(data$duration_cleaned)
data$max_downloads <- as.numeric(data$max_downloads)




# --- Step 1: Create pre and post 1874 variables based on birth year ---

data$pre_i <- ifelse(data$birth_cleaned < 1874, 1, 0)
data$post_i <- ifelse(data$birth_cleaned >= 1874, 1, 0)


# --- Step 2: Calculate age of the composer at the time of composition ---

data$age_ij <- as.numeric(data$work_year_cleaned) - as.numeric(data$birth_cleaned)

# --- Step 3: Create higher-order polynomial terms for age ---

data$age_ij2 <- data$age_ij^2
data$age_ij3 <- data$age_ij^3
data$age_ij4 <- data$age_ij^4

# --- Step 4: Create interaction terms for pre and post cohorts ---

data$pre_age_ij <- data$pre_i * data$age_ij
data$pre_age_ij2 <- data$pre_i * data$age_ij2
data$pre_age_ij3 <- data$pre_i * data$age_ij3
data$pre_age_ij4 <- data$pre_i * data$age_ij4

data$post_age_ij <- data$post_i * data$age_ij
data$post_age_ij2 <- data$post_i * data$age_ij2
data$post_age_ij3 <- data$post_i * data$age_ij3
data$post_age_ij4 <- data$post_i * data$age_ij4

# --- Step 5: Create the log of duration as a control variable ---

data$log_duration_ij <- log(as.numeric(data$duration_cleaned))

# --- Step 6: Create dummy variables for instrumentation ---


data$instr_piano <- ifelse(data$Piano == 1, 1, 0)
data$instr_orchestra <- ifelse(data$Orchestra == 1, 1, 0)
data$instr_violin <- ifelse(data$Violin == 1, 1, 0)
data$instr_cello <- ifelse(data$Cello == 1, 1, 0)
data$instr_voice <- ifelse(data$Voice == 1, 1, 0)
data$instr_strings <- ifelse(data$Strings == 1, 1, 0)
data$instr_woodwinds <- ifelse(data$Woodwinds == 1, 1, 0)
data$instr_brass <- ifelse(data$Brass == 1, 1, 0)
data$instr_percussion <- ifelse(data$Percussion == 1, 1, 0)
data$instr_keyboard <- ifelse(data$Keyboard == 1, 1, 0)
data$instr_chorus <- ifelse(data$Chorus == 1, 1, 0)
data$instr_plucked <- ifelse(data$Plucked == 1, 1, 0)
data$instr_electronic <- ifelse(data$Electronic == 1, 1, 0)
data$instr_other <- ifelse(data$Other == 1, 1, 0)


# --- Step 7: Create composer fixed effects using dummy variables ---

composer_fixed_effects <- model.matrix(~ as.factor(data$composer) - 1)
# Rename columns to reflect composer names or identifiers if necessary

# --- Step 8: Create year fixed effects based on the composition year ---

year_fixed_effects <- model.matrix(~ as.factor(data$work_year_cleaned) - 1)




# Create a temporary column for the log-transformed variable, excluding non-positive values
data$log_max_downloads <- ifelse(data$max_downloads > 0, log(data$max_downloads), NA)

# Set the output file path
output_file <- "/Users/canaandrinkwater/Desktop/Thesis_Creativity/10252024/Output/regression_output.txt"

# Start recording output to the specified file
sink(output_file)




# --- Step 9: Run the regression models with NA exclusion and save output ---

# Model 1: age_ij
cat("Model 1.1: log(max_downloads) ~ age_ij\n")
print(summary(lm(log_max_downloads ~ age_ij, data = data, na.action = na.exclude)))
cat("\n")

cat("Model 1.2: log(max_downloads) ~ age_ij + age_ij2\n")
print(summary(lm(log_max_downloads ~ age_ij + age_ij2, data = data, na.action = na.exclude)))
cat("\n")

cat("Model 1.3: log(max_downloads) ~ age_ij + age_ij2 + age_ij3\n")
print(summary(lm(log_max_downloads ~ age_ij + age_ij2 + age_ij3, data = data, na.action = na.exclude)))
cat("\n")


# Model 2: pre_age_ij, post_age_ij
cat("Model 2.1: log(max_downloads) ~ pre_age_ij + post_age_ij\n")
print(summary(lm(log_max_downloads ~ pre_age_ij + post_age_ij, data = data, na.action = na.exclude)))
cat("\n")

cat("Model 2.2: log(max_downloads) ~ pre_age_ij + pre_age_ij2 + post_age_ij + post_age_ij2\n")
print(summary(lm(log_max_downloads ~ pre_age_ij + pre_age_ij2 + post_age_ij + post_age_ij2, data = data, na.action = na.exclude)))
cat("\n")

cat("Model 2.3: log(max_downloads) ~ pre_age_ij + pre_age_ij2 + pre_age_ij3 + post_age_ij + post_age_ij2 + post_age_ij3\n")
print(summary(lm(log_max_downloads ~ pre_age_ij + pre_age_ij2 + pre_age_ij3 + post_age_ij + post_age_ij2 + post_age_ij3, data = data, na.action = na.exclude)))
cat("\n")



# Model 3_1: age_ij polynomial terms and log_duration_ij
cat("Model 3_1: log(max_downloads) ~ age_ij + log_duration_ij\n")
print(summary(lm(log_max_downloads ~ age_ij + log_duration_ij, data = data, na.action = na.exclude)))
cat("\n")

# Model 3_2: age_ij polynomial terms and log_duration_ij
cat("Model 3_2: log(max_downloads) ~ age_ij + age_ij2 + log_duration_ij\n")
print(summary(lm(log_max_downloads ~ age_ij + age_ij2 + log_duration_ij, data = data, na.action = na.exclude)))
cat("\n")

# Model 3_3: age_ij polynomial terms and log_duration_ij
cat("Model 3_3: log(max_downloads) ~ age_ij + age_ij2 + age_ij3 + log_duration_ij\n")
print(summary(lm(log_max_downloads ~ age_ij + age_ij2 + age_ij3 + log_duration_ij, data = data, na.action = na.exclude)))
cat("\n")


# Model 4_1: pre_age_ij, post_age_ij interaction terms and log_duration_ij
cat("Model 4_1: log(max_downloads) ~ pre_age_ij + post_age_ij + log_duration_ij\n")
print(summary(lm(log_max_downloads ~ pre_age_ij + post_age_ij + log_duration_ij, data = data, na.action = na.exclude)))
cat("\n")

# Model 4_2: pre_age_ij, post_age_ij interaction terms and log_duration_ij
cat("Model 4_2: log(max_downloads) ~ pre_age_ij + pre_age_ij2 + post_age_ij + post_age_ij2 + log_duration_ij\n")
print(summary(lm(log_max_downloads ~ pre_age_ij + pre_age_ij2 + post_age_ij + post_age_ij2 + log_duration_ij, data = data, na.action = na.exclude)))
cat("\n")

# Model 4_3: pre_age_ij, post_age_ij interaction terms and log_duration_ij
cat("Model 4_3: log(max_downloads) ~ pre_age_ij + pre_age_ij2 + pre_age_ij3 + post_age_ij + post_age_ij2 + post_age_ij3 + log_duration_ij\n")
print(summary(lm(log_max_downloads ~ pre_age_ij + pre_age_ij2 + pre_age_ij3 + post_age_ij + post_age_ij2 + post_age_ij3 + log_duration_ij, data = data, na.action = na.exclude)))
cat("\n")


# Model 5_1: age_ij polynomial terms, log_duration_ij, and instrumentation dummies
cat("Model 5_1: log(max_downloads) ~ age_ij + log_duration_ij + instrumentation dummies\n")
print(summary(lm(log_max_downloads ~ age_ij + log_duration_ij + instr_piano + instr_orchestra + instr_violin + instr_cello + instr_voice + instr_strings + instr_woodwinds + instr_brass + instr_percussion + instr_keyboard + instr_chorus + instr_plucked + instr_electronic + instr_other, data = data, na.action = na.exclude)))
cat("\n")

# Model 5_2: age_ij polynomial terms, log_duration_ij, and instrumentation dummies
cat("Model 5_2: log(max_downloads) ~ age_ij + age_ij2 + log_duration_ij + instrumentation dummies\n")
print(summary(lm(log_max_downloads ~ age_ij + age_ij2 + log_duration_ij + instr_piano + instr_orchestra + instr_violin + instr_cello + instr_voice + instr_strings + instr_woodwinds + instr_brass + instr_percussion + instr_keyboard + instr_chorus + instr_plucked + instr_electronic + instr_other, data = data, na.action = na.exclude)))
cat("\n")

# Model 5_3: age_ij polynomial terms, log_duration_ij, and instrumentation dummies
cat("Model 5_3: log(max_downloads) ~ age_ij + age_ij2 + age_ij3 + log_duration_ij + instrumentation dummies\n")
print(summary(lm(log_max_downloads ~ age_ij + age_ij2 + age_ij3 + log_duration_ij + instr_piano + instr_orchestra + instr_violin + instr_cello + instr_voice + instr_strings + instr_woodwinds + instr_brass + instr_percussion + instr_keyboard + instr_chorus + instr_plucked + instr_electronic + instr_other, data = data, na.action = na.exclude)))
cat("\n")



# Model 6_1: pre_age_ij, post_age_ij, log_duration_ij and instrumentation dummies
cat("Model 6_1: log(max_downloads) ~ pre_age_ij + post_age_ij + log_duration_ij + instrumentation dummies\n")
print(summary(lm(log_max_downloads ~ pre_age_ij + post_age_ij + log_duration_ij + instr_piano + instr_orchestra + instr_violin + instr_cello + instr_voice + instr_strings + instr_woodwinds + instr_brass + instr_percussion + instr_keyboard + instr_chorus + instr_plucked + instr_electronic + instr_other, data = data, na.action = na.exclude)))
cat("\n")

# Model 6_2: pre_age_ij, post_age_ij, log_duration_ij and instrumentation dummies
cat("Model 6_2: log(max_downloads) ~ pre_age_ij + pre_age_ij2 + post_age_ij + post_age_ij2 + log_duration_ij + instrumentation dummies\n")
print(summary(lm(log_max_downloads ~ pre_age_ij + pre_age_ij2 + post_age_ij + post_age_ij2 + log_duration_ij + instr_piano + instr_orchestra + instr_violin + instr_cello + instr_voice + instr_strings + instr_woodwinds + instr_brass + instr_percussion + instr_keyboard + instr_chorus + instr_plucked + instr_electronic + instr_other, data = data, na.action = na.exclude)))
cat("\n")

# Model 6_3: pre_age_ij, post_age_ij, log_duration_ij and instrumentation dummies
cat("Model 6_3: log(max_downloads) ~ pre_age_ij + pre_age_ij2 + pre_age_ij3 + post_age_ij + post_age_ij2 + post_age_ij3 + log_duration_ij + instrumentation dummies\n")
print(summary(lm(log_max_downloads ~ pre_age_ij + pre_age_ij2 + pre_age_ij3 + post_age_ij + post_age_ij2 + post_age_ij3 + log_duration_ij + instr_piano + instr_orchestra + instr_violin + instr_cello + instr_voice + instr_strings + instr_woodwinds + instr_brass + instr_percussion + instr_keyboard + instr_chorus + instr_plucked + instr_electronic + instr_other, data = data, na.action = na.exclude)))
cat("\n")


# Model 7_1: age_ij polynomial terms, log_duration_ij, instrumentation dummies, and composer fixed effects
cat("Model 7_1: log(max_downloads) ~ age_ij + log_duration_ij + instrumentation dummies + composer fixed effects\n")
print(summary(lm(log_max_downloads ~ age_ij + log_duration_ij + instr_piano + instr_orchestra + instr_violin + instr_cello + instr_voice + instr_strings + instr_woodwinds + instr_brass + instr_percussion + instr_keyboard + instr_chorus + instr_plucked + instr_electronic + instr_other + composer_fixed_effects, data = data, na.action = na.exclude)))
cat("\n")

# Model 7_2: age_ij polynomial terms, log_duration_ij, instrumentation dummies, and composer fixed effects
cat("Model 7_2: log(max_downloads) ~ age_ij + age_ij2 + log_duration_ij + instrumentation dummies + composer fixed effects\n")
print(summary(lm(log_max_downloads ~ age_ij + age_ij2 + log_duration_ij + instr_piano + instr_orchestra + instr_violin + instr_cello + instr_voice + instr_strings + instr_woodwinds + instr_brass + instr_percussion + instr_keyboard + instr_chorus + instr_plucked + instr_electronic + instr_other + composer_fixed_effects, data = data, na.action = na.exclude)))
cat("\n")

# Model 7_3: age_ij polynomial terms, log_duration_ij, instrumentation dummies, and composer fixed effects
cat("Model 7_3: log(max_downloads) ~ age_ij + age_ij2 + age_ij3 + log_duration_ij + instrumentation dummies + composer fixed effects\n")
print(summary(lm(log_max_downloads ~ age_ij + age_ij2 + age_ij3 + log_duration_ij + instr_piano + instr_orchestra + instr_violin + instr_cello + instr_voice + instr_strings + instr_woodwinds + instr_brass + instr_percussion + instr_keyboard + instr_chorus + instr_plucked + instr_electronic + instr_other + composer_fixed_effects, data = data, na.action = na.exclude)))
cat("\n")



# Model 8_1: pre_age_ij, post_age_ij, log_duration_ij, instrumentation dummies, and composer fixed effects
cat("Model 8_1: log(max_downloads) ~ pre_age_ij + post_age_ij + log_duration_ij + instrumentation dummies + composer fixed effects\n")
print(summary(lm(log_max_downloads ~ pre_age_ij + post_age_ij + log_duration_ij + instr_piano + instr_orchestra + instr_violin + instr_cello + instr_voice + instr_strings + instr_woodwinds + instr_brass + instr_percussion + instr_keyboard + instr_chorus + instr_plucked + instr_electronic + instr_other + composer_fixed_effects, data = data, na.action = na.exclude)))
cat("\n")

# Model 8_2: pre_age_ij, post_age_ij, log_duration_ij, instrumentation dummies, and composer fixed effects
cat("Model 8_2: log(max_downloads) ~ pre_age_ij + pre_age_ij2 + post_age_ij + post_age_ij2 + log_duration_ij + instrumentation dummies + composer fixed effects\n")
print(summary(lm(log_max_downloads ~ pre_age_ij + pre_age_ij2 + post_age_ij + post_age_ij2 + log_duration_ij + instr_piano + instr_orchestra + instr_violin + instr_cello + instr_voice + instr_strings + instr_woodwinds + instr_brass + instr_percussion + instr_keyboard + instr_chorus + instr_plucked + instr_electronic + instr_other + composer_fixed_effects, data = data, na.action = na.exclude)))
cat("\n")

# Model 8_3: pre_age_ij, post_age_ij, log_duration_ij, instrumentation dummies, and composer fixed effects
cat("Model 8_3: log(max_downloads) ~ pre_age_ij + pre_age_ij2 + pre_age_ij3 + post_age_ij + post_age_ij2 + post_age_ij3 + log_duration_ij + instrumentation dummies + composer fixed effects\n")
print(summary(lm(log_max_downloads ~ pre_age_ij + pre_age_ij2 + pre_age_ij3 + post_age_ij + post_age_ij2 + post_age_ij3 + log_duration_ij + instr_piano + instr_orchestra + instr_violin + instr_cello + instr_voice + instr_strings + instr_woodwinds + instr_brass + instr_percussion + instr_keyboard + instr_chorus + instr_plucked + instr_electronic + instr_other + composer_fixed_effects, data = data, na.action = na.exclude)))
cat("\n")


# Load necessary packages
library(broom)
library(dplyr)
# Model 9_1: age_ij polynomial terms, log_duration_ij, instrumentation dummies, composer, and year fixed effects
cat("Model 9_1: log(max_downloads) ~ age_ij + log_duration_ij + instrumentation dummies + composer and year fixed effects\n")
print(summary(lm(log_max_downloads ~ age_ij + log_duration_ij + instr_piano + instr_orchestra + instr_violin + instr_cello + instr_voice + instr_strings + instr_woodwinds + instr_brass + instr_percussion + instr_keyboard + instr_chorus + instr_plucked + instr_electronic + instr_other + composer_fixed_effects + year_fixed_effects, data = data, na.action = na.exclude)))
cat("\n")

# Model 9_2: age_ij polynomial terms (up to age_ij2), log_duration_ij, instrumentation dummies, composer, and year fixed effects
cat("Model 9_2: log(max_downloads) ~ age_ij + age_ij2 + log_duration_ij + instrumentation dummies + composer and year fixed effects\n")
print(summary(lm(log_max_downloads ~ age_ij + age_ij2 + log_duration_ij + instr_piano + instr_orchestra + instr_violin + instr_cello + instr_voice + instr_strings + instr_woodwinds + instr_brass + instr_percussion + instr_keyboard + instr_chorus + instr_plucked + instr_electronic + instr_other + composer_fixed_effects + year_fixed_effects, data = data, na.action = na.exclude)))
cat("\n")

# Model 9_3: age_ij polynomial terms (up to age_ij3), log_duration_ij, instrumentation dummies, composer, and year fixed effects
cat("Model 9_3: log(max_downloads) ~ age_ij + age_ij2 + age_ij3 + log_duration_ij + instrumentation dummies + composer and year fixed effects\n")
print(summary(lm(log_max_downloads ~ age_ij + age_ij2 + age_ij3 + log_duration_ij + instr_piano + instr_orchestra + instr_violin + instr_cello + instr_voice + instr_strings + instr_woodwinds + instr_brass + instr_percussion + instr_keyboard + instr_chorus + instr_plucked + instr_electronic + instr_other + composer_fixed_effects + year_fixed_effects, data = data, na.action = na.exclude)))
cat("\n")



# Model 10_1: pre_age_ij, post_age_ij, log_duration_ij, instrumentation dummies, composer, and year fixed effects
cat("Model 10_1: log(max_downloads) ~ pre_age_ij + post_age_ij + log_duration_ij + instrumentation dummies + composer and year fixed effects\n")
print(summary(lm(log_max_downloads ~ pre_age_ij + post_age_ij + log_duration_ij + instr_piano + instr_orchestra + instr_violin + instr_cello + instr_voice + instr_strings + instr_woodwinds + instr_brass + instr_percussion + instr_keyboard + instr_chorus + instr_plucked + instr_electronic + instr_other + composer_fixed_effects + year_fixed_effects, data = data, na.action = na.exclude)))
cat("\n")

# Model 10_2: pre_age_ij and polynomial terms, post_age_ij and polynomial terms, log_duration_ij, instrumentation dummies, composer, and year fixed effects
cat("Model 10_2: log(max_downloads) ~ pre_age_ij + pre_age_ij2 + post_age_ij + post_age_ij2 + log_duration_ij + instrumentation dummies + composer and year fixed effects\n")
print(summary(lm(log_max_downloads ~ pre_age_ij + pre_age_ij2 + post_age_ij + post_age_ij2 + log_duration_ij + instr_piano + instr_orchestra + instr_violin + instr_cello + instr_voice + instr_strings + instr_woodwinds + instr_brass + instr_percussion + instr_keyboard + instr_chorus + instr_plucked + instr_electronic + instr_other + composer_fixed_effects + year_fixed_effects, data = data, na.action = na.exclude)))
cat("\n")


# Model 10_3: pre_age_ij and polynomial terms (up to pre_age_ij3), post_age_ij and polynomial terms (up to post_age_ij3), log_duration_ij, instrumentation dummies, composer, and year fixed effects
cat("Model 10_3: log(max_downloads) ~ pre_age_ij + pre_age_ij2 + pre_age_ij3 + post_age_ij + post_age_ij2 + post_age_ij3 + log_duration_ij + instrumentation dummies + composer and year fixed effects\n")
print(summary(lm(log_max_downloads ~ pre_age_ij + pre_age_ij2 + pre_age_ij3 + post_age_ij + post_age_ij2 + post_age_ij3 + log_duration_ij + instr_piano + instr_orchestra + instr_violin + instr_cello + instr_voice + instr_strings + instr_woodwinds + instr_brass + instr_percussion + instr_keyboard + instr_chorus + instr_plucked + instr_electronic + instr_other + composer_fixed_effects + year_fixed_effects, data = data, na.action = na.exclude)))
cat("\n")


# Stop recording output
sink()

cat("Regression output saved to:", output_file, "\n")






# --- Step 10: (Optional) Save the updated dataset ---

# Save the updated dataset back to Excel if needed
output_file <- "/Users/canaandrinkwater/Desktop/Thesis_Creativity/10252024/Data/imslp/imslp_used"
write_xlsx(data, path = output_file)
cat("Dataset with new variables saved to", output_file)

