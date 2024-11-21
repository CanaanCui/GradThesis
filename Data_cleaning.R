# Install the readxl package if you don't have it already
# install.packages("readxl")

# Load the necessary package
library(readxl)

# Specify the file path
file_path <- "/Users/canaandrinkwater/Desktop/Thesis_Creativity/Data/imslp/imslp_1.xlsx"

# Read the Excel file, ensuring the first row is used as column names
data <- read_excel(file_path, col_names = TRUE)



# 1. Overview

# Print the structure of the dataset
str(data)

# Print a summary of the dataset
summary(data)





# 2. Cleaning the 'composer' variable

# Modify the 'composer' variable by removing "Category:" from each entry
data$composer <- gsub("^Category:", "", data$composer)





# 3. Cleaning the 'work_year' variable

# Copy values from 'work_year' to 'work_year_cleaned'
data$work_year_cleaned <- data$work_year

# Remove observations with empty 'work_year_cleaned' values
data <- data[!is.na(data$work_year_cleaned) & data$work_year_cleaned != "", ]

# Remove rows where no four-digit year exists
# The regular expression '\\d{4}' checks for the presence of a four-digit number
data <- data[grepl("\\d{4}", data$work_year_cleaned), ]

# Extract only the first four-digit year and remove all other text
data$work_year_cleaned <- sub(".*?(\\d{4}).*", "\\1", data$work_year_cleaned)

# View the first few rows to verify the changes
head(data[c("work_year", "work_year_cleaned")])

# Print the structure of the 'work_year_cleaned' variable
str(data$work_year_cleaned)

# Check for entries that are not four-digit numbers
non_four_digit <- grep("^[0-9]{4}$", data$work_year_cleaned, invert = TRUE, value = TRUE)

# Print entries that do not match the four-digit format
if (length(non_four_digit) > 0) {
  cat("Entries not matching four-digit format:\n")
  print(non_four_digit)
} else {
  cat("All entries are four-digit numbers.\n")
}

# Convert 'work_year_cleaned' from character to numeric
data$work_year_cleaned_numeric <- as.numeric(data$work_year_cleaned)

# Check for NAs which may result from conversion issues (e.g., non-numeric characters)
sum(is.na(data$work_year_cleaned_numeric))  # This will tell you how many failed to convert

# If there are no NAs, or you're okay with ignoring them, print the summary of numeric data
print(summary(data$work_year_cleaned_numeric))





# 4. Categorizing the instruments, creating dummy variables

# Initialize the new columns with 0 values for all rows
data$Piano <- 0
data$Orchestra <- 0
data$Violin <- 0
data$Cello <- 0
data$Voice <- 0
data$Strings <- 0
data$Woodwinds <- 0
data$Brass <- 0
data$Percussion <- 0
data$Keyboard <- 0
data$Chorus <- 0
data$Plucked <- 0
data$Electronic <- 0
data$Other <- 0

# Function to check for the presence of instruments or categories
detect_instruments <- function(instrument_value) {
  return(c(
    Piano = as.integer(grepl("\\bpianos?\\b", instrument_value, ignore.case = TRUE)),
    Orchestra = as.integer(grepl("orchestra", instrument_value, ignore.case = TRUE)),
    Violin = as.integer(grepl("\\bviolins?\\b", instrument_value, ignore.case = TRUE)),
    Cello = as.integer(grepl("\\bcellos?\\b", instrument_value, ignore.case = TRUE)),
    Voice = as.integer(grepl("\\b(voice|soprano|mezzo|alto|tenor|baritone|bass|counter-tenor|multiple soloists)\\b", instrument_value, ignore.case = TRUE)),
    Strings = as.integer(grepl("\\b(violas?|double bass|harps?|mandolins?|lutes?|viola da gamba|viola d'amore|violones?|arpeggione|zithers?)\\b", instrument_value, ignore.case = TRUE)),
    Woodwinds = as.integer(grepl("\\b(flutes?|piccolos?|alto flutes?|bass flutes?|recorders?|oboes?|english horns?|bassoons?|contrabassoons?|clarinets?|bass clarinets?|saxophones?|basset horns?|basset clarinets?|chalumeaus?|sarrusophones?|crumhorns?|shawms?)\\b", instrument_value, ignore.case = TRUE)),
    Brass = as.integer(grepl("\\b(trumpets?|cornets?|piccolo trumpets?|trombones?|horns?|wagner tubas?|tubas?|euphoniums?|flugelhorns?|bugles?|saxhorns?|serpents?|ophicleides?)\\b", instrument_value, ignore.case = TRUE)),
    Percussion = as.integer(grepl("\\b(timpani|xylophones?|vibraphones?|marimbas?|glockenspiels?|cymbals?|drums?|celestas?)\\b", instrument_value, ignore.case = TRUE)),
    Keyboard = as.integer(grepl("\\b(electric pianos?|organs?|harpsichords?|clavichords?|accordions?|harmoniums?|synthesizers?)\\b", instrument_value, ignore.case = TRUE)),
    Chorus = as.integer(grepl("\\b(chorus|female chorus|male chorus|children's chorus|unison chorus)\\b", instrument_value, ignore.case = TRUE)),
    Plucked = as.integer(grepl("\\b(guitars?|banjos?|ukuleles?|domras?|pipas?|sitars?)\\b", instrument_value, ignore.case = TRUE)),
    Electronic = as.integer(grepl("\\b(electric guitars?|electric pianos?|synthesizers?|ondes martenot|theremins?)\\b", instrument_value, ignore.case = TRUE)),
    Other = as.integer(grepl("\\b(harmonicas?|bagpipes?|dulcimers?|cimbaloms?|concertinas?|pan flutes?|glass harmonicas?|narrators?)\\b", instrument_value, ignore.case = TRUE))
  ))
}

# Apply the function to each observation in the 'instrument' column
for (i in 1:nrow(data)) {
  result <- detect_instruments(data$instrument[i])
  data$Piano[i] <- result["Piano"]
  data$Orchestra[i] <- result["Orchestra"]
  data$Violin[i] <- result["Violin"]
  data$Cello[i] <- result["Cello"]
  data$Voice[i] <- result["Voice"]
  data$Strings[i] <- result["Strings"]
  data$Woodwinds[i] <- result["Woodwinds"]
  data$Brass[i] <- result["Brass"]
  data$Percussion[i] <- result["Percussion"]
  data$Keyboard[i] <- result["Keyboard"]
  data$Chorus[i] <- result["Chorus"]
  data$Plucked[i] <- result["Plucked"]
  data$Electronic[i] <- result["Electronic"]
  data$Other[i] <- result["Other"]
}

# Print the summary statistics
print(summary(data[c("Piano", "Orchestra", "Violin", "Cello", "Voice", 
                     "Strings", "Woodwinds", "Brass", "Percussion", 
                     "Keyboard", "Chorus", "Plucked", "Electronic", "Other")]))







# 5. Create a new variable 'max_downloads'

# Assuming 'top_3_downloads' is stored as a character column with lists in the form of strings
# Create a new variable 'max_downloads'

data$max_downloads <- sapply(data$top_3_downloads, function(x) {
  # Remove square brackets and split by comma
  numbers <- as.numeric(unlist(strsplit(gsub("\\[|\\]", "", x), ",")))
  
  # Return the maximum value, or 0 if the list is empty
  if (length(numbers) == 0 || all(is.na(numbers))) {
    return(0)
  } else {
    return(max(numbers, na.rm = TRUE))
  }
})


# Print the statistical summary of the 'max_downloads' variable
print(summary(data$max_downloads))







# 6. Cleaning the "duration" variable

# Create a new variable 'duration_cleaned' to store the cleaned values
data$duration_cleaned <- sapply(data$duration, function(x) {
  # Remove the "minutes" text
  x <- gsub(" minutes", "", x)
  x <- gsub(" minute", "", x)
  # Check if there is a range (i.e., contains "-")
  if (grepl("-", x)) {
    # Split the range and calculate the average
    range_values <- as.numeric(unlist(strsplit(x, "-")))
    return(mean(range_values, na.rm = TRUE))
  } else {
    # If no range, return the numeric value
    return(as.numeric(x))
  }
})

# Print the statistical summary of the 'duration_cleaned' variable
print(summary(data$duration_cleaned))






# 6. Converting birth and death to numeric

# Convert the 'birth_cleaned' variable to numeric
data$birth_cleaned <- as.numeric(substr(data$birth, 1, 4))

# Convert the 'death_cleaned' variable to numeric
data$death_cleaned <- as.numeric(substr(data$death, 1, 4))


# Print the statistical summary of the 'birth_cleaned' variable
print(summary(data$birth_cleaned))

# Print the statistical summary of the 'death_cleaned' variable
print(summary(data$death_cleaned))






# 7. Getting the primary opus/catalogue number

# Define a function to get only the first part of opus_number before the first ";" or ","
get_first_opus_number <- function(opus_number) {
  # Extract content before the first ";" or ","
  sub("[;,].*", "", opus_number)
}

# Create the new variable "opus_number_cleaned"
data <- data %>%
  mutate(
    # Generate the cleaned opus number by extracting the first part of opus_number
    opus_number_cleaned = get_first_opus_number(opus_number)
  )




# 8. Remove the bracket and its content from composition

# Define a function to remove the bracket and its content if it contains the composer
remove_bracketed_content <- function(composition) {
  # Use a regular expression to remove text within parentheses
  gsub("\\s*\\(.*\\)", "", composition)
}

# Apply the function to the "composition" column
data <- data %>%
  mutate(composition = remove_bracketed_content(composition))





# Save the updated data as imslp_2.xlsx in the same location
output_path <- "/Users/canaandrinkwater/Desktop/Thesis_Creativity/Data/imslp/imslp_2.xlsx"
write.xlsx(data, output_path)

# Display a success message
cat("opus_number_cleaned has been generated and saved in imslp_2.xlsx. Please manually check before appending to composition.")


