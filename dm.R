library(tidyverse)
library(dplyr)
library(haven) #use read_dta()
library(Hmisc) #use to add column labels

#load in data
data <- read_dta(file = "housing_insec_data_deidentified.dta")

## .do file (lines 7-14)
#Sort the data by start date
data <- data[order(data$StartDate),]

## Create a new variable named directoutreach
  #This variable describes the method of contact with the person of interest
  # Whether they were contacted directly by the research team to take the survey, 
  # Or if they took the survey via a sampled class
data$directoutreach = 1
data$directoutreach[1:1098] <- 0
label(data$directoutreach) <- "contacted student directly via email to take survey"

## .do file lines(15-27 and 44-50)
  #Factor Ethnicities (and rename)  - No need to have a new column of factors. Factor same column
data$Ethnicity <- factor(data$Ethnicity, labels=c("American Indian Alaskan Native", "Asian Non-Hispanic", "African American",
                                                     "Hispanic/Latino", "Native Hawaiian/Other", "No Value","Multiple Races", "Unknown", "White/Non-Hipanic") 
)
# Collapse the Unknown and No Value levels since these are kind of the same.
data$Ethnicity <- forcats::fct_collapse(data$Ethnicity, 
                               Unknown = c("Unknown", "No Value")) 

## .do file (lines 27-30)
 #Generate age in years from birthdate
data$age <- round((Sys.Date() - data$BIRTH_DT)/365.25)
data$age <- as.numeric(data$age)
label(data$age) <- "Age in years"

## .d file (lines 31-44)
 #Categorize ages in age column in a new variable for special analysis
labs <- c("17andUnder", "18-19", "20-21", "22-24", "25-29", "30-34", "35-49", "50-64", "65+")
breakpts <- c(0, 18, 20, 22, 25, 30, 35, 50, 65, 150)
data$age_column <- cut(data$age, breaks = breakpts, labels = labs, right = FALSE)
data$age_column <- as.factor(data$age_column)
rm(list = c("breakpts","labs" ))  #remove unneeded variables from env
levels(data$age_column) <- gsub("^17andUnder$", "17 and Under", levels(data$age_column))
label(data$age_column) <- "Ages Categorical"


## .do file (lines 55-74 are unnecessary since using factors)

## .do file (lines 75-79)
   #set a few labels
label(data$ChicoCumltvGPA) <- "Chico Cumulative GPA"
label(data$HS_GPA) <- "High School GPA"
label(data$TRF_GPA) <- "Transfer GPA"

## .do file (lines 79-84)
   #Generate new binary column US Citizen = 1, Non-US Citizen = 0
data$Citizenship[data$Citizenship == "United States citizen\r\n"] <- "US Citizen"
data$US_Citizen <- ifelse(data$Citizenship == "US Citizen", 1, 0)
label(data$US_Citizen) <- "1 = US Citizen, 0 = Non-US Citizen"

## .do file (lines 84-89)
   #Turn FirstGen column into indicator variable (yes = 1, no = 0)
   #Replace missing values with NA
data$FirstGen <- ifelse(data$FirstGen == "Y", 1, data$FirstGen)
data$FirstGen <- ifelse(data$FirstGen == "N", 0, data$FirstGen)
data$FirstGen <- ifelse(data$FirstGen == "U", NA, data$FirstGen)
data$FirstGen <- ifelse(data$FirstGen == "", NA, data$FirstGen)
class(data$FirstGen) <- "numeric" #change class to numeric
label(data$FirstGen) <- "1st Generation student = 1, Not 1st Generation student = 0"

# .do file (lines 91-128)
   #Rename columns
   #Recode missing data to NA's.

data$D2_1[is.na(data$D2_1)] <- 0
data$D2_2[is.na(data$D2_2)] <- 0
data$D2_3[is.na(data$D2_3)] <- 0
data$D2_4[is.na(data$D2_4)] <- 0
data$D2_5[is.na(data$D2_5)] <- 0
data$D2_6[is.na(data$D2_6)] <- 0
data$D2_7[is.na(data$D2_7)] <- 0
data$D2_8[is.na(data$D2_8)] <- 0
data$D2_9[is.na(data$D2_9)] <- 0
data$D2_10[is.na(data$D2_10)] <- 0

   #Recode classes to work with NA values
classes <- c("D2_1", "D2_2", "D2_3", "D2_4", "D2_5",
             "D2_6", "D2_7", "D2_8", "D2_9", "D2_10", "D2_12")
class(data[,classes]) <- c("haven_labelled", "numeric")
rm(classes)  #remove classes from environment

   #set NA's for people who didn't want to disclose this information
data$D2_12[is.na(data$D2_12)] <- 0  #recode na values to 0
data$D2_1 <- ifelse(data$D2_12 == 1, NA, data$D2_1)
data$D2_2 <- ifelse(data$D2_12 == 1, NA, data$D2_2)
data$D2_3 <- ifelse(data$D2_12 == 1, NA, data$D2_3)
data$D2_4 <- ifelse(data$D2_12 == 1, NA, data$D2_4)
data$D2_5 <- ifelse(data$D2_12 == 1, NA, data$D2_5)
data$D2_6 <- ifelse(data$D2_12 == 1, NA, data$D2_6)
data$D2_7 <- ifelse(data$D2_12 == 1, NA, data$D2_7)
data$D2_8 <- ifelse(data$D2_12 == 1, NA, data$D2_8)
data$D2_9 <- ifelse(data$D2_12 == 1, NA, data$D2_9)
data$D2_10 <- ifelse(data$D2_12 == 1, NA, data$D2_10)

   #label columns so they are easier to understand
label(data$D2_1) <- "Differently Abled? (0/1 = no/yes)"
label(data$D2_2) <- "First Generation student? (0/1 = no/yes)"
label(data$D2_3) <- "Foster Child? (0/1 = no/yes)"
label(data$D2_4) <- "Youth Homelessness? (0/1 = no/yes)"
label(data$D2_5) <- "English as a 2nd Language? (0/1 = no/yes)"
label(data$D2_6) <- "Veteran Status? (0/1 = no/yes)"
label(data$D2_7) <- "Active Duty Military? (0/1 = no/yes)"
label(data$D2_8) <- "Student Athelete? (0/1 = no/yes)"
label(data$D2_9) <- "Student Parent? (0/1 = no/yes)"
label(data$D2_10) <- "Non-traditional Student? (0/1 = no/yes)"
label(data$D2_12) <- "Decline to Say (0/1 = no/yes)"

## .do file (lines 128-134)
   #recode sexual orientation as a factor
data$D3[data$D3 == 6] <- NA
data$D3 <- factor(data$D3, labels=c("Heterosexual", "Gay/Lesbian", "Bisexual", "Unsure", "Other"))
label(data$D3) <- "Sexual Orientation"

## .do file (lines 135-137)
   #recode identified gender
data$D4[data$D4 == 5] <- NA
data$D4 <- factor(data$D4, labels=c("Male", "Female", "Transgender", "Other"))
label(data$D4) <- "Identified Gender"

## .do file (lines 140 - 158)
   #Change D5 to indicate working or non-working students
   #Add column to describe number of hours worked per week 
data$HoursWorked <- data$D5
data$D5 <- data$D5 - 1
data$D5 <- ifelse(data$D5 > 0, 1, data$D5)
label(data$D5) <- "Working? (0/1 = no/yes)"

data$HoursWorked[data$HoursWorked == 1] <- NA
data$HoursWorked <- factor(data$HoursWorked, labels=c("< 10", "10-19", "20-29", "30-39", "40+"))
label(data$HoursWorked) <- "Weekly hours worked"
   
   # Create column to see which working students are being paid
data$paidEmployed <- data$D5
data$paidEmployed[data$D6 == 1] <- 0
label(data$paidEmployed) <- "Working a paid job? (no/yes = 0/1)"
  
  # Create a column to show ranges of hours worked (less than 20, 20 or more)
data$workingTime <- fct_collapse(data$HoursWorked, 
                                 Less_Than_20 = c("< 10", "10-19"),
                                 Twenty_or_More = c("20-29", "30-39", "40+"))
label(data$workingTime) <- "Generalized Time Spent Working per Week"

## .do file (lines 159-169) not necessary because we already have this info

## .do file (lines 175-178)
   #Code D7 to be binary indicator (no/yes = 0/1)
data$D7[data$D7 == 3] <- NA
data$D7[data$D7 == 2] <- 0
label(data$D7) <- "Help support dependents or family? (no/yes = 0/1)"

## .do file (lines 179-183)
   #factor H1 (rent or own)
data$H1 <- factor(data$H1, labels = c("Own", "Rent", "Family Owned", "Family Rented", "Unstable Housing"))
label(data$H1) <- "Current Housing Status"

