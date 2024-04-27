#Viewing data
crime_data <- read_excel('Hate Crime in Chicago.xlsx')
View(crime_data)

# Determining the size of the data
dimensions <- dim(crime_data)

num_rows <- dimensions[1]
num_cols <- dimensions[2]

print(paste("Number of rows:", num_rows))
print(paste("Number of columns:", num_cols))

#Checking for missing data 
complete_rows <- complete.cases(crime_data)

# Counting the number of rows with missing data
num_rows_with_missing <- sum(!complete_rows)

print(paste("Number of rows with missing data:", num_rows_with_missing))


# Checking for missing values in each column
missing_values <- colSums(is.na(crime_data))

# Getting the names of columns with missing data
columns_with_missing <- names(missing_values[missing_values > 0])

print("Columns with missing data:")
print(columns_with_missing)

#Removing unnecessary columns
cleaned_crime_data <- crime_data[, !(names(crime_data) %in% c("RD", "TIME","UCR DODE", "CPD Area","DIST", "BEAT", "Victims", "Arrested", "Juveniles", "Adults", "Disposition"))]
View(cleaned_crime_data)


#Removing time from Date column
cleaned_crime_data$DATE <- substr(cleaned_crime_data$DATE, 1, 10)


# Defining a function to check if a string contains only digits
is_integer <- function(x) {
  grepl("^\\d+$", x)
}

# Replacing integers or blank cells in DATE column with "N/A"
cleaned_crime_data$DATE <- ifelse(is.na(cleaned_crime_data$DATE) | cleaned_crime_data$DATE == "" | is_integer(cleaned_crime_data$DATE), 
                                  "N/A", 
                                  cleaned_crime_data$DATE)



# Removing rows with "N/A" in the DATE column
cleaned_crime_data <- subset(cleaned_crime_data, DATE != "N/A")

# Convert 'DATE' column to Date format with MM/DD/YYYY format
cleaned_crime_data$DATE <- as.Date(cleaned_crime_data$DATE, format = "%m/%d/%Y")

# Extract years from 'DATE' column
cleaned_crime_data$YEARS <- format(cleaned_crime_data$DATE, "%Y")

# Rearrange columns to place 'YEARS' column after 'DATE' column
cleaned_crime_data <- cleaned_crime_data[, c("DATE", "YEARS", setdiff(names(cleaned_crime_data), c("DATE", "YEARS")))]

# Checking for missing values in the dataset
if (any(is.na(cleaned_crime_data))) {
  print("There are missing values in the dataset.")
} else {
  print("There are no missing values in the dataset.")
}


# Identifying rows with missing values
rows_with_missing <- which(rowSums(is.na(cleaned_crime_data)) > 0)

# Displaying rows with missing values
cleaned_crime_data[rows_with_missing, ]


# Finding duplicated rows
duplicate_rows <- duplicated(cleaned_crime_data)

print(cleaned_crime_data[duplicate_rows, ]) #(Displayed results but were not duplicates each row had unique data)


# Standardizing column names

# Renaming column names
cleaned_crime_data <- cleaned_crime_data %>%
  rename(date = DATE,
         years = YEARS,
         offense = OFFENSE,
         motivation_bias = Motivation,
         community = Community)


# Renaming some values in the motivation_bias column
cleaned_crime_data$motivation_bias <- ifelse(grepl("ANTI-GAY", cleaned_crime_data$motivation_bias) | 
                                               grepl("ANTI-LESBIAN", cleaned_crime_data$motivation_bias) | 
                                               grepl("ANTI-TRANSGENDER", cleaned_crime_data$motivation_bias) |
                                               cleaned_crime_data$motivation_bias %in% c("ANTI-BISEXUAL", 
                                                                                         "ANTI-GAY/LESBIAN/BISEXUAL/TRANSGENDER (MIXED GROUP)", 
                                                                                         "ANTI-TRANSGENDER NON-CONFORMING", 
                                                                                         "ANTI- GAY/LESBIAN/BISEXUAL/TRANSGENDER (MIXED GROUP)"), 
                                             "Anti-LGBTQIA", cleaned_crime_data$motivation_bias)

cleaned_crime_data$motivation_bias <- ifelse(cleaned_crime_data$motivation_bias == "ANTI-HISPANIC/LATINO", "Anti-Hispanic", cleaned_crime_data$motivation_bias)
cleaned_crime_data$motivation_bias <- ifelse(cleaned_crime_data$motivation_bias %in% c("ANTI-OTHER ETHNICITY/NATIONAL ORIGIN", 
                                                                                       "ANTI-MULTIPLE RACES/GROUP"), 
                                             "Xenophobic", 
                                             cleaned_crime_data$motivation_bias)

cleaned_crime_data$motivation_bias <- ifelse(cleaned_crime_data$motivation_bias %in% c("ANTI-ISLAMIC/MUSLIM", "ANTI-OTHER RELIGION", "ANTI-MULTIPLE RELIGIONS/GROUPS", "ANTI-PROTESTANT", "ANTI-CATHOLIC", "ANTI-OTHER CHRISTIAN","ANTI-EASTERN ORTHODOX (GREEK, RUSSIAN)"), 
                                             "Religious Supremacist", 
                                             cleaned_crime_data$motivation_bias)

cleaned_crime_data$motivation_bias <- ifelse(cleaned_crime_data$motivation_bias == "ANTI-IMMIGRATION STATUS", "Anti-Immigrant", cleaned_crime_data$motivation_bias)

cleaned_crime_data$motivation_bias <- ifelse(cleaned_crime_data$motivation_bias == "ANTI-BLACK/AFRICAN-AMERICAN", "Anti-Black", cleaned_crime_data$motivation_bias)

cleaned_crime_data$motivation_bias <- gsub("ANTI ARAB", "ANTI-ARAB", cleaned_crime_data$motivation_bias)
cleaned_crime_data$motivation_bias <- gsub("ANTI- JEWISH", "ANTI-JEWISH", cleaned_crime_data$motivation_bias)



# Create a mapping of offenses to their updated names
offense_mapping <- c(
  "CRIMINAL DAMAGE: CRIMINAL DEFACEMENT" = "VANDALISM",
  "CRIMINAL DAMAGE: INSTITUTIONAL VANDALISM" = "VANDALISM",
  "OTHER OFFENSE: HARASSMENT BY TELEPHONE" = "CYBER HARASSMENT",
  "OTHER OFFENSE: HARASSMENT BY ELECTRONIC MEANS" = "CYBER HARASSMENT",
  "ASSAULT: SIMPLE" = "ASSAULT: AGGRAVATED",
  "ASSAULT: AGGRAVATED: HANDGUN" = "ASSAULT: AGGRAVATED",
  "ASSAULT: AGGRAVATED: OTHER DANG WEAPON" = "ASSAULT: AGGRAVATED",
  "ASSAULT: AGGRAVATED:KNIFE/CUTTING INSTR" = "ASSAULT: AGGRAVATED",
  "ASSAULT: AGGRAVATED: OTHER FIREARM" = "ASSAULT: AGGRAVATED",
  "ASSAULT: AGGRAVATED-HANDGUN" = "ASSAULT: AGGRAVATED",
  "BATTERY: SIMPLE" = "BATTERY: AGGRAVATED",
  "BATTERY: AGGRAVATED OF A SENIOR CITIZEN" = "BATTERY: AGGRAVATED",
  "BATTERY: AGG PRO EMP HANDS SERIOUS INJ" = "BATTERY: AGGRAVATED",
  "BATTERY: AGG PRO EMP HANDS NO/MIN INJURY" = "BATTERY: AGGRAVATED",
  "BATTERY: AGGRAVATED: OTHER DANG WEAPON" = "BATTERY: AGGRAVATED",
  "BATTERY: AGGRAVATED: HANDS/FIST/FEET SERIOUS INJURY" = "BATTERY: AGGRAVATED",
  "BATTERY: AGGRAVATED: HANDS/FIST/FEET NO/MINOR INJURY" = "BATTERY: AGGRAVATED",
  "BATTERY: AGGRAVATED DOMESTIC BATTERY: KNIFE/CUTTING INST" = "BATTERY: AGGRAVATED",
  "BATTERY: AGGRAVATED: HANDGUN" = "BATTERY: AGGRAVATED",
  "CRIM SEXUAL ASSAULT: ATTEMPT NON-AGGRAVATED" = "SEXUAL ASSAULT: ATTEMPT",
  "CRIM SEXUAL ASSAULT: ATTEMPT AGG: OTHER" = "SEXUAL ASSAULT: ATTEMPT",
  "HOMICIDE: FIRST DEGREE MURDER" = "HOMICIDE",
  "HOMICIDE: RECKLESS HOMICIDE" = "HOMICIDE",
  "ROBBERY: STRONGARM - NO WEAPON" = "ROBBERY: ARMED",
  "ROBBERY: ARMED: HANDGUN" = "ROBBERY: ARMED",
  "ROBBERY: ARMED: OTHER DANGEROUS WEAPON" = "ROBBERY: ARMED",
  "ROBBERY: ARMED:KNIFE/CUTTING INSTRUMENT" = "ROBBERY: ARMED",
  "ROBBERY: ATTEMPT: STRONGARM-NO WEAPON" = "ROBBERY: ARMED",
  "ROBBERY: ATTEMPT: ARMED-OTHER DANG WEAP" = "ROBBERY: ARMED"
)

# Replace offense names using the mapping
cleaned_crime_data$offense <- offense_mapping[cleaned_crime_data$offense]

# Remove rows with missing data
cleaned_crime_data <- cleaned_crime_data[complete.cases(cleaned_crime_data), ]


#Standardizing all values in dataset to one format
cleaned_crime_data[] <- lapply(cleaned_crime_data, as.character)
cleaned_crime_data[] <- lapply(cleaned_crime_data, tolower)


# Defining a function to capitalize the first letter of each word
capitalize_first_letter <- function(x) {
  sapply(x, function(y) {
    words <- unlist(strsplit(y, " "))
    capitalized_words <- paste(toupper(substring(words, 1, 1)), substring(words, 2), sep = "", collapse = " ")
    return(capitalized_words)
  })
}

# Apply the function to columns with string values
columns_to_capitalize <- c("date", "offense", "motivation_bias", "community")
cleaned_crime_data[columns_to_capitalize] <- lapply(cleaned_crime_data[columns_to_capitalize], capitalize_first_letter)

# Save cleaned crime data as a CSV file
write.csv(cleaned_crime_data, "cleaned_crime_data.csv", row.names = FALSE)
