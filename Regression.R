# Install and load necessary libraries
library(readxl)
library(writexl)
library(dplyr)



# Read the dataset
file_path <- "/Users/canaandrinkwater/Desktop/Thesis_Creativity/11162024/Data/imslp/imslp.xlsx"
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
output_file <- "/Users/canaandrinkwater/Desktop/Thesis_Creativity/11162024/Output/regression_output.txt"

# Start recording output to the specified file
sink(output_file)






# --- Step 9: Summary statistics ---

library(dplyr)

# Ensure the log-transformed downloads variable exists
data$log_max_downloads <- ifelse(data$max_downloads > 0, log(data$max_downloads), NA)

# Summary statistics for all composers
summary_stats_all <- data %>%
  summarise(
    Mean_Birth = mean(birth_cleaned, na.rm = TRUE),
    SD_Birth = sd(birth_cleaned, na.rm = TRUE),
    Mean_Work_Year = mean(work_year_cleaned, na.rm = TRUE),
    SD_Work_Year = sd(work_year_cleaned, na.rm = TRUE),
    Mean_Age = mean(age_ij, na.rm = TRUE),
    SD_Age = sd(age_ij, na.rm = TRUE),
    Mean_Popularity = mean(max_downloads, na.rm = TRUE),
    SD_Popularity = sd(max_downloads, na.rm = TRUE),
    Mean_Duration = mean(duration_cleaned, na.rm = TRUE),
    SD_Duration = sd(duration_cleaned, na.rm = TRUE),
    Mean_Piano = mean(instr_piano, na.rm = TRUE),
    SD_Piano = sd(instr_piano, na.rm = TRUE),
    Mean_Violin = mean(instr_violin, na.rm = TRUE),
    SD_Violin = sd(instr_violin, na.rm = TRUE),
    Mean_Cello = mean(instr_cello, na.rm = TRUE),
    SD_Cello = sd(instr_cello, na.rm = TRUE),
    Mean_Orchestra = mean(instr_orchestra, na.rm = TRUE),
    SD_Orchestra = sd(instr_orchestra, na.rm = TRUE),
    Mean_Chorus = mean(instr_chorus, na.rm = TRUE),
    SD_Chorus = sd(instr_chorus, na.rm = TRUE),
    Mean_Voice = mean(instr_voice, na.rm = TRUE),
    SD_Voice = sd(instr_voice, na.rm = TRUE),
    Mean_Strings = mean(instr_strings, na.rm = TRUE),
    SD_Strings = sd(instr_strings, na.rm = TRUE),
    Mean_Woodwinds = mean(instr_woodwinds, na.rm = TRUE),
    SD_Woodwinds = sd(instr_woodwinds, na.rm = TRUE),
    Mean_Brass = mean(instr_brass, na.rm = TRUE),
    SD_Brass = sd(instr_brass, na.rm = TRUE),
    Mean_Percussion = mean(instr_percussion, na.rm = TRUE),
    SD_Percussion = sd(instr_percussion, na.rm = TRUE),
    Mean_Keyboard = mean(instr_keyboard, na.rm = TRUE),
    SD_Keyboard = sd(instr_keyboard, na.rm = TRUE),
    Mean_Plucked = mean(instr_plucked, na.rm = TRUE),
    SD_Plucked = sd(instr_plucked, na.rm = TRUE),
    Mean_Electronic = mean(instr_electronic, na.rm = TRUE),
    SD_Electronic = sd(instr_electronic, na.rm = TRUE),
    Mean_Other = mean(instr_other, na.rm = TRUE),
    SD_Other = sd(instr_other, na.rm = TRUE)
  )

# Summary statistics for pre-1874 cohort
summary_stats_pre <- data %>%
  filter(birth_cleaned < 1874) %>%
  summarise(
    Mean_Birth = mean(birth_cleaned, na.rm = TRUE),
    SD_Birth = sd(birth_cleaned, na.rm = TRUE),
    Mean_Work_Year = mean(work_year_cleaned, na.rm = TRUE),
    SD_Work_Year = sd(work_year_cleaned, na.rm = TRUE),
    Mean_Age = mean(age_ij, na.rm = TRUE),
    SD_Age = sd(age_ij, na.rm = TRUE),
    Mean_Popularity = mean(max_downloads, na.rm = TRUE),
    SD_Popularity = sd(max_downloads, na.rm = TRUE),
    Mean_Duration = mean(duration_cleaned, na.rm = TRUE),
    SD_Duration = sd(duration_cleaned, na.rm = TRUE),
    Mean_Piano = mean(instr_piano, na.rm = TRUE),
    SD_Piano = sd(instr_piano, na.rm = TRUE),
    Mean_Violin = mean(instr_violin, na.rm = TRUE),
    SD_Violin = sd(instr_violin, na.rm = TRUE),
    Mean_Cello = mean(instr_cello, na.rm = TRUE),
    SD_Cello = sd(instr_cello, na.rm = TRUE),
    Mean_Orchestra = mean(instr_orchestra, na.rm = TRUE),
    SD_Orchestra = sd(instr_orchestra, na.rm = TRUE),
    Mean_Chorus = mean(instr_chorus, na.rm = TRUE),
    SD_Chorus = sd(instr_chorus, na.rm = TRUE),
    Mean_Voice = mean(instr_voice, na.rm = TRUE),
    SD_Voice = sd(instr_voice, na.rm = TRUE),
    Mean_Strings = mean(instr_strings, na.rm = TRUE),
    SD_Strings = sd(instr_strings, na.rm = TRUE),
    Mean_Woodwinds = mean(instr_woodwinds, na.rm = TRUE),
    SD_Woodwinds = sd(instr_woodwinds, na.rm = TRUE),
    Mean_Brass = mean(instr_brass, na.rm = TRUE),
    SD_Brass = sd(instr_brass, na.rm = TRUE),
    Mean_Percussion = mean(instr_percussion, na.rm = TRUE),
    SD_Percussion = sd(instr_percussion, na.rm = TRUE),
    Mean_Keyboard = mean(instr_keyboard, na.rm = TRUE),
    SD_Keyboard = sd(instr_keyboard, na.rm = TRUE),
    Mean_Plucked = mean(instr_plucked, na.rm = TRUE),
    SD_Plucked = sd(instr_plucked, na.rm = TRUE),
    Mean_Electronic = mean(instr_electronic, na.rm = TRUE),
    SD_Electronic = sd(instr_electronic, na.rm = TRUE),
    Mean_Other = mean(instr_other, na.rm = TRUE),
    SD_Other = sd(instr_other, na.rm = TRUE)
  )

# Summary statistics for post-1874 cohort
summary_stats_post <- data %>%
  filter(birth_cleaned >= 1874) %>%
  summarise(
    Mean_Birth = mean(birth_cleaned, na.rm = TRUE),
    SD_Birth = sd(birth_cleaned, na.rm = TRUE),
    Mean_Work_Year = mean(work_year_cleaned, na.rm = TRUE),
    SD_Work_Year = sd(work_year_cleaned, na.rm = TRUE),
    Mean_Age = mean(age_ij, na.rm = TRUE),
    SD_Age = sd(age_ij, na.rm = TRUE),
    Mean_Popularity = mean(max_downloads, na.rm = TRUE),
    SD_Popularity = sd(max_downloads, na.rm = TRUE),
    Mean_Duration = mean(duration_cleaned, na.rm = TRUE),
    SD_Duration = sd(duration_cleaned, na.rm = TRUE),
    Mean_Piano = mean(instr_piano, na.rm = TRUE),
    SD_Piano = sd(instr_piano, na.rm = TRUE),
    Mean_Violin = mean(instr_violin, na.rm = TRUE),
    SD_Violin = sd(instr_violin, na.rm = TRUE),
    Mean_Cello = mean(instr_cello, na.rm = TRUE),
    SD_Cello = sd(instr_cello, na.rm = TRUE),
    Mean_Orchestra = mean(instr_orchestra, na.rm = TRUE),
    SD_Orchestra = sd(instr_orchestra, na.rm = TRUE),
    Mean_Chorus = mean(instr_chorus, na.rm = TRUE),
    SD_Chorus = sd(instr_chorus, na.rm = TRUE),
    Mean_Voice = mean(instr_voice, na.rm = TRUE),
    SD_Voice = sd(instr_voice, na.rm = TRUE),
    Mean_Strings = mean(instr_strings, na.rm = TRUE),
    SD_Strings = sd(instr_strings, na.rm = TRUE),
    Mean_Woodwinds = mean(instr_woodwinds, na.rm = TRUE),
    SD_Woodwinds = sd(instr_woodwinds, na.rm = TRUE),
    Mean_Brass = mean(instr_brass, na.rm = TRUE),
    SD_Brass = sd(instr_brass, na.rm = TRUE),
    Mean_Percussion = mean(instr_percussion, na.rm = TRUE),
    SD_Percussion = sd(instr_percussion, na.rm = TRUE),
    Mean_Keyboard = mean(instr_keyboard, na.rm = TRUE),
    SD_Keyboard = sd(instr_keyboard, na.rm = TRUE),
    Mean_Plucked = mean(instr_plucked, na.rm = TRUE),
    SD_Plucked = sd(instr_plucked, na.rm = TRUE),
    Mean_Electronic = mean(instr_electronic, na.rm = TRUE),
    SD_Electronic = sd(instr_electronic, na.rm = TRUE),
    Mean_Other = mean(instr_other, na.rm = TRUE),
    SD_Other = sd(instr_other, na.rm = TRUE)
  )

# Print results
cat("\nSummary for All Composers:\n")
print(as.data.frame(summary_stats_all), row.names = FALSE)

cat("\nSummary for Pre-1874 Cohort:\n")
print(as.data.frame(summary_stats_pre), row.names = FALSE)

cat("\nSummary for Post-1874 Cohort:\n")
print(as.data.frame(summary_stats_post), row.names = FALSE)



# --- Calculate Mean Square Error (MSE) by composer and create weights ---
data <- data %>%
  group_by(composer) %>%
  mutate(
    residual = log_max_downloads - mean(log_max_downloads, na.rm = TRUE),
    mse_composer = mean(residual^2, na.rm = TRUE)
  ) %>%
  ungroup()

# Create weights as the inverse of MSE
data$weights <- 1 / data$mse_composer





# --- Step 10: Run the regression models with NA exclusion and save output ---


# Model 1: pre_age_ij, post_age_ij

# --- Model 1.1: Weighted Least Squares Regression (WLS) ---
cat("Model 1.1 (WLS): log(max_downloads) ~ pre_age_ij + post_age_ij\n")
model_1.1_wls <- lm(log_max_downloads ~ pre_age_ij + post_age_ij, 
                    data = data, 
                    weights = data$weights, 
                    na.action = na.exclude)
print(summary(model_1.1_wls))
cat("\n")



# --- Model 1.2: Weighted Least Squares Regression ---
cat("Model 1.2 (WLS): log(max_downloads) ~ pre_age_ij + pre_age_ij2 + post_age_ij + post_age_ij2\n")
model_1.2_wls <- lm(log_max_downloads ~ pre_age_ij + pre_age_ij2 + 
                      post_age_ij + post_age_ij2, 
                    data = data, 
                    weights = data$weights, 
                    na.action = na.exclude)
print(summary(model_1.2_wls))
cat("\n")


# --- Model 1.3: Weighted Least Squares Regression ---
cat("Model 1.3 (WLS): log(max_downloads) ~ pre_age_ij + pre_age_ij2 + pre_age_ij3 + post_age_ij + post_age_ij2 + post_age_ij3\n")
model_1.3_wls <- lm(log_max_downloads ~ pre_age_ij + pre_age_ij2 + pre_age_ij3 + 
                      post_age_ij + post_age_ij2 + post_age_ij3, 
                    data = data, 
                    weights = data$weights, 
                    na.action = na.exclude)
print(summary(model_1.3_wls))
cat("\n")



# Model 2: Adding log_duration_ij

# --- Model 2_1: Weighted Least Squares Regression (WLS) ---
cat("Model 2_1 (WLS): log(max_downloads) ~ pre_age_ij + post_age_ij + log_duration_ij\n")
model_2_1_wls <- lm(log_max_downloads ~ pre_age_ij + post_age_ij + log_duration_ij, 
                    data = data, 
                    weights = data$weights, 
                    na.action = na.exclude)
print(summary(model_2_1_wls))
cat("\n")


# --- Model 2_2: Weighted Least Squares Regression (WLS) ---
cat("Model 2_2 (WLS): log(max_downloads) ~ pre_age_ij + pre_age_ij2 + post_age_ij + post_age_ij2 + log_duration_ij\n")
model_2_2_wls <- lm(log_max_downloads ~ pre_age_ij + pre_age_ij2 + post_age_ij + post_age_ij2 + log_duration_ij, 
                    data = data, 
                    weights = data$weights, 
                    na.action = na.exclude)
print(summary(model_2_2_wls))
cat("\n")


# --- Model 2_3: Weighted Least Squares Regression (WLS) ---
cat("Model 2_3 (WLS): log(max_downloads) ~ pre_age_ij + pre_age_ij2 + pre_age_ij3 + post_age_ij + post_age_ij2 + post_age_ij3 + log_duration_ij\n")
model_2_3_wls <- lm(log_max_downloads ~ pre_age_ij + pre_age_ij2 + pre_age_ij3 + 
                      post_age_ij + post_age_ij2 + post_age_ij3 + log_duration_ij, 
                    data = data, 
                    weights = data$weights, 
                    na.action = na.exclude)
print(summary(model_2_3_wls))
cat("\n")




# --- Model 3_1: Weighted Least Squares Regression (WLS) ---
cat("Model 3_1 (WLS): log(max_downloads) ~ pre_age_ij + post_age_ij + log_duration_ij + instrumentation dummies\n")
model_3_1_wls <- lm(log_max_downloads ~ pre_age_ij + post_age_ij + log_duration_ij + 
                      instr_piano + instr_orchestra + instr_violin + instr_cello + instr_voice + 
                      instr_strings + instr_woodwinds + instr_brass + instr_percussion + 
                      instr_keyboard + instr_chorus + instr_plucked + instr_electronic + 
                      instr_other, 
                    data = data, 
                    weights = data$weights, 
                    na.action = na.exclude)
print(summary(model_3_1_wls))
cat("\n")


# --- Model 3_2: Weighted Least Squares Regression (WLS) ---
cat("Model 3_2 (WLS): log(max_downloads) ~ pre_age_ij + pre_age_ij2 + post_age_ij + post_age_ij2 + log_duration_ij + instrumentation dummies\n")
model_3_2_wls <- lm(log_max_downloads ~ pre_age_ij + pre_age_ij2 + post_age_ij + post_age_ij2 + 
                      log_duration_ij + instr_piano + instr_orchestra + instr_violin + instr_cello + 
                      instr_voice + instr_strings + instr_woodwinds + instr_brass + instr_percussion + 
                      instr_keyboard + instr_chorus + instr_plucked + instr_electronic + 
                      instr_other, 
                    data = data, 
                    weights = data$weights, 
                    na.action = na.exclude)
print(summary(model_3_2_wls))
cat("\n")


# --- Model 3_3: Weighted Least Squares Regression (WLS) ---
cat("Model 3_3 (WLS): log(max_downloads) ~ pre_age_ij + pre_age_ij2 + pre_age_ij3 + post_age_ij + post_age_ij2 + post_age_ij3 + log_duration_ij + instrumentation dummies\n")
model_3_3_wls <- lm(log_max_downloads ~ pre_age_ij + pre_age_ij2 + pre_age_ij3 + 
                      post_age_ij + post_age_ij2 + post_age_ij3 + log_duration_ij + 
                      instr_piano + instr_orchestra + instr_violin + instr_cello + instr_voice + 
                      instr_strings + instr_woodwinds + instr_brass + instr_percussion + 
                      instr_keyboard + instr_chorus + instr_plucked + instr_electronic + 
                      instr_other, 
                    data = data, 
                    weights = data$weights, 
                    na.action = na.exclude)
print(summary(model_3_3_wls))
cat("\n")





# --- Model 4_1: Weighted Least Squares Regression (WLS) ---
cat("Model 4_1 (WLS): log(max_downloads) ~ pre_age_ij + post_age_ij + log_duration_ij + instrumentation dummies + composer fixed effects\n")
model_4_1_wls <- lm(log_max_downloads ~ pre_age_ij + post_age_ij + log_duration_ij + 
                      instr_piano + instr_orchestra + instr_violin + instr_cello + instr_voice + 
                      instr_strings + instr_woodwinds + instr_brass + instr_percussion + 
                      instr_keyboard + instr_chorus + instr_plucked + instr_electronic + 
                      instr_other + composer_fixed_effects, 
                    data = data, 
                    weights = data$weights, 
                    na.action = na.exclude)
print(summary(model_4_1_wls))
cat("\n")


# --- Model 4_2: Weighted Least Squares Regression (WLS) ---
cat("Model 4_2 (WLS): log(max_downloads) ~ pre_age_ij + pre_age_ij2 + post_age_ij + post_age_ij2 + log_duration_ij + instrumentation dummies + composer fixed effects\n")
model_4_2_wls <- lm(log_max_downloads ~ pre_age_ij + pre_age_ij2 + post_age_ij + post_age_ij2 + 
                      log_duration_ij + instr_piano + instr_orchestra + instr_violin + instr_cello + 
                      instr_voice + instr_strings + instr_woodwinds + instr_brass + instr_percussion + 
                      instr_keyboard + instr_chorus + instr_plucked + instr_electronic + 
                      instr_other + composer_fixed_effects, 
                    data = data, 
                    weights = data$weights, 
                    na.action = na.exclude)
print(summary(model_4_2_wls))
cat("\n")


# --- Model 4_3: Weighted Least Squares Regression (WLS) ---
cat("Model 4_3 (WLS): log(max_downloads) ~ pre_age_ij + pre_age_ij2 + pre_age_ij3 + post_age_ij + post_age_ij2 + post_age_ij3 + log_duration_ij + instrumentation dummies + composer fixed effects\n")
model_4_3_wls <- lm(log_max_downloads ~ pre_age_ij + pre_age_ij2 + pre_age_ij3 + 
                      post_age_ij + post_age_ij2 + post_age_ij3 + log_duration_ij + 
                      instr_piano + instr_orchestra + instr_violin + instr_cello + instr_voice + 
                      instr_strings + instr_woodwinds + instr_brass + instr_percussion + 
                      instr_keyboard + instr_chorus + instr_plucked + instr_electronic + 
                      instr_other + composer_fixed_effects, 
                    data = data, 
                    weights = data$weights, 
                    na.action = na.exclude)
print(summary(model_4_3_wls))
cat("\n")





# --- Model 5_1: Weighted Least Squares Regression (WLS) ---
cat("Model 5_1 (WLS): log(max_downloads) ~ pre_age_ij + post_age_ij + log_duration_ij + instrumentation dummies + composer and year fixed effects\n")
model_5_1_wls <- lm(log_max_downloads ~ pre_age_ij + post_age_ij + log_duration_ij + 
                      instr_piano + instr_orchestra + instr_violin + instr_cello + instr_voice + 
                      instr_strings + instr_woodwinds + instr_brass + instr_percussion + 
                      instr_keyboard + instr_chorus + instr_plucked + instr_electronic + 
                      instr_other + composer_fixed_effects + year_fixed_effects, 
                    data = data, 
                    weights = data$weights, 
                    na.action = na.exclude)
print(summary(model_5_1_wls))
cat("\n")


# --- Model 5_2: Weighted Least Squares Regression (WLS) ---
cat("Model 5_2 (WLS): log(max_downloads) ~ pre_age_ij + pre_age_ij2 + post_age_ij + post_age_ij2 + log_duration_ij + instrumentation dummies + composer and year fixed effects\n")
model_5_2_wls <- lm(log_max_downloads ~ pre_age_ij + pre_age_ij2 + post_age_ij + post_age_ij2 + 
                      log_duration_ij + instr_piano + instr_orchestra + instr_violin + instr_cello + 
                      instr_voice + instr_strings + instr_woodwinds + instr_brass + instr_percussion + 
                      instr_keyboard + instr_chorus + instr_plucked + instr_electronic + 
                      instr_other + composer_fixed_effects + year_fixed_effects, 
                    data = data, 
                    weights = data$weights, 
                    na.action = na.exclude)
print(summary(model_5_2_wls))
cat("\n")


# --- Model 5_3: Weighted Least Squares Regression (WLS) ---
cat("Model 5_3 (WLS): log(max_downloads) ~ pre_age_ij + pre_age_ij2 + pre_age_ij3 + post_age_ij + post_age_ij2 + post_age_ij3 + log_duration_ij + instrumentation dummies + composer and year fixed effects\n")
model_5_3_wls <- lm(log_max_downloads ~ pre_age_ij + pre_age_ij2 + pre_age_ij3 + 
                      post_age_ij + post_age_ij2 + post_age_ij3 + log_duration_ij + 
                      instr_piano + instr_orchestra + instr_violin + instr_cello + instr_voice + 
                      instr_strings + instr_woodwinds + instr_brass + instr_percussion + 
                      instr_keyboard + instr_chorus + instr_plucked + instr_electronic + 
                      instr_other + composer_fixed_effects + year_fixed_effects, 
                    data = data, 
                    weights = data$weights, 
                    na.action = na.exclude)
print(summary(model_5_3_wls))
cat("\n")



# --- Step 11: Analyze if deleted observations have significantly less max_downloads ---

# Separate data into two groups: deleted (NA log_duration_ij) and undeleted
deleted_obs <- subset(data, is.na(log_duration_ij))
undeleted_obs <- subset(data, !is.na(log_duration_ij))

# Calculate the number of observations in each group
num_deleted <- nrow(deleted_obs)
num_undeleted <- nrow(undeleted_obs)

# Compare the means of max_downloads between the two groups
cat("Comparison of max_downloads between deleted and undeleted observations:\n")

# Calculate mean and standard deviation for both groups
mean_deleted <- mean(deleted_obs$max_downloads, na.rm = TRUE)
sd_deleted <- sd(deleted_obs$max_downloads, na.rm = TRUE)
mean_undeleted <- mean(undeleted_obs$max_downloads, na.rm = TRUE)
sd_undeleted <- sd(undeleted_obs$max_downloads, na.rm = TRUE)

cat("Deleted observations: Number =", num_deleted, ", Mean =", mean_deleted, ", SD =", sd_deleted, "\n")
cat("Undeleted observations: Number =", num_undeleted, ", Mean =", mean_undeleted, ", SD =", sd_undeleted, "\n")

# Perform a t-test to see if the difference in means is statistically significant
t_test_result <- t.test(deleted_obs$max_downloads, undeleted_obs$max_downloads, na.rm = TRUE)
print(t_test_result)



# Stop recording output
sink()

cat("Regression output saved to:", output_file, "\n")












# --- Step 12: (Optional) Save the updated dataset ---

# Save the updated dataset back to Excel if needed
output_file <- "/Users/canaandrinkwater/Desktop/Thesis_Creativity/11162024/Data/imslp/imslp_used"
write_xlsx(data, path = output_file)
cat("Dataset with new variables saved to", output_file)

