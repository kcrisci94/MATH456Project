library(tidyverse)
library(dplyr)
library(haven) #use read_dta()
library(Hmisc) #use to add column labels

###---------- Start Kaleb Section -----------###
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
data$Ethnicity2 <- factor(data$Ethnicity, labels=c("Native American", "Asian", "African American",
                                                   "Hispanic/Latino", "Pacific Islander", "No Value","Two or more races", "Unknown", "White/Non-Hipanic") 
)
# Collapse the Unknown and No Value levels since these are kind of the same.
data$Ethnicity2 <- droplevels(data$Ethnicity2, "No Value")

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
#classes <- c("D2_1", "D2_2", "D2_3", "D2_4", "D2_5",
#             "D2_6", "D2_7", "D2_8", "D2_9", "D2_10", "D2_12")
#class(data[,classes]) <- c("haven_labelled", "numeric")
#rm(classes)  #remove classes from environment

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
data$D4 <- factor(data$D4, labels=c("Male", "Female", "Transgender", "Other")) %>% 
  fct_collapse("Transgender/nonbinary" = c("Transgender", "Other"))
label(data$D4) <- "Identified Gender"

## .do file (lines 140 - 158)
#Change D5 to indicate working or non-working students
#Add column to describe number of hours worked per week 
data$employed <- ifelse(data$D5 > 1, 1, 0)
data$HoursWorked <- data$D5
data$HoursWorked[data$HoursWorked == 1] <- NA
data$HoursWorked <- factor(data$HoursWorked, labels=c("< 10", "10-19", "20-29", "30-39", "40+"))
label(data$HoursWorked) <- "Weekly hours worked"

# Create column to see which working students are being paid
data$paidEmployed <- data$employed
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


## .do file (lines 184-186)
#This is the way Jen coded it
#create dummy rent variable: does the student rent (no/yes = 0/1)
#data$rentdummy <- ifelse(data$H1 == 2, 1, 0)

## .do file (lines 184-186)
#This is the way we think it should be done
#create dummy rent variable: does the student rent (no/yes = 0/1)
data$rentdummy <- ifelse(data$H1 == 2 | data$H1 == 4, 1, 0)

## .do file (lines 179-183)
#factor H1 (rent or own)
data$H1 <- factor(data$H1, labels = c("Own", "Rent", "Family Owned", "Family Rented", "Unstable Housing"))
label(data$H1) <- "Current Housing Status"

## .do file (lines 187-191)
#factor and label % of income used for housing expenses
data$housingcosts <- factor(data$H2, labels = c("none", "<30%", "30%", "30% - 50%", ">50%"))
label(data$housingcosts) <- "% of income used for housing expenses"

## .do file (lines 192-195)
#Create generalization of H2 variable (30% or more = 1, otherwise 0)
data$housingburdendummy <- ifelse(data$H2 < 4, 0, 1)
label(data$housingburdendummy) <- "Pays 30% or more of housing expenses per month"

## .do file (lines 196-201)
#generate who pays for housing variable
data$whopayshousing <- factor(data$H3, labels=c("studentPaysAllHousing", "studentPaysAllHousing", "shared", "othersPayHousing", "Other"))
label(data$whopayshousing) <- "Who pays for housing expenses?"

## .do file (lines 202-204)
#generate shared expenses dummy variable
data$sharedExpenses <- ifelse(data$H3 == 3, 1, 0)

## .do file (lines 205-210)
#Recode H4 variable to binary
data$H4[data$H4 == 2] <- 0
label(data$H4) <- "Rentor morgate increase that made it difficult to pay (no/yes = 0/1)"

## .do file (lines 211-249)
#Recode several columns at once to binary and rename variables
cols <- c("H5", "H6", "H7", "H8", "H9", "H10", "H11")
data[,cols] <- ifelse(data[,cols] == 2, 0, 1)
newcols <- c("unableToPayRent", "lateRent", "unableToPayBill", "exceedCapacity", "askedToLeave", "evicted", "unsafeHSG")
data <- data %>% rename_at(vars(cols), ~ newcols)
label(data$unableToPayRent) <- "Unable to pay or underpaid your rent or mortgage?"
label(data$unableToPayBill) <- "Unable to pay or underpaid a utility bill?"
label(data$lateRent) <- "Late paying your rent or mortgage?"
label(data$exceedCapacity) <- "Have you lived with others beyond the expected capacity of your house or apartment?"
label(data$askedToLeave) <- "Have you been asked to leave your home by someone you lived with?"
label(data$evicted) <- "Have you been evicted?"
label(data$unsafeHSG) <- "Have you had to stay in a hostile housing environment or abusive relationship bc you had no other place to live?"
rm(cols, newcols)

## .do file (lines 251-256)
#Factor H12 variable
data$unsureSleep <- factor(data$H12, labels = c("Never", "Once", "Twice", "3+ times"))
label(data$unsureSleep) <- "How many times have you been unsure of where you were going to sleep at night"

## .do file (lines 257-259)
data$unsureSleepDummy <- ifelse(data$H12 > 1, 1, 0)
label(data$unsureSleepDummy) <- "Ever been unsure of where you were going to sleep?"

## .do file (lines 260-264)
data$numberOfMoves <- factor(data$H13, labels = c("None", "Once", "Twice", "3-5 times", "6+ times"))
label(data$numberOfMoves) <- "How many times have you moved in the past 12 months"

###----------- End Kaleb Section -----------###

###------ Start Constantin Section ---------###


## .do file (lines 266-267)
data <- data %>% mutate(housingindex = select(., unableToPayRent:unsafeHSG, unsureSleepDummy) %>% rowSums(na.rm = TRUE))
label(data$housingindex) <- "Index of housing insecurity measures"

## .do file (lines 269-281) dummy HI values
data$HIdummy1 <- ifelse(data$housingindex > 1, 1, data$housingindex)
label(data$HIdummy1) <- "experienced one or more incident of housing insecurity"

data$HIdummy2 <- ifelse(data$housingindex > 1, 1, 0)
label(data$HIdummy2) <- "experienced two or more incidents of housing insecurity"

data$HIdummy3 <- ifelse(data$housingindex > 2, 1, 0)
label(data$HIdummy3) <- "experienced 3 or more incidents of housing insecurity"

## .do file (lines 280-281)
# Variable H14_1
data$oncampus <- data$H14_1
data$oncampus <- ifelse(data$H14_1 < 3, 1, 0) 
label(data$oncampus) <- "Campus or University Housing"

## .do file (lines 283-307)
data$shelter30 <- ifelse(data$H14_5 > 1, 0, data$H14_5)
data$couchsurf30 <- ifelse(data$H14_6 > 1, 0, data$H14_6)
data$hotmot30 <- ifelse(data$H14_7 > 1, 0, data$H14_7)
data$transithousing30 <- ifelse(data$H14_8 > 1, 0, data$H14_8)
data$grouphome30 <- ifelse(data$H14_9 > 1, 0, data$H14_9)
data$outdoor30 <- ifelse(data$H14_10 > 1, 0, data$H14_10)
data$car30 <- ifelse(data$H14_11 > 1, 0, data$H14_11)
data$bldg30 <- ifelse(data$H14_12 > 1, 0, data$H14_12)

label(data$shelter30) <- "in a shelter the last 30 days? (no/yes = 0/1)"
label(data$couchsurf30) <- "Temporarily stayed with friend of family member until R. could find other hsg - past 30 days)"
label(data$hotmot30) <- "Stayed in a hotel or motel without having a permanent home, in past 30 days"
label(data$transithousing30) <- "Stayed in a transitional housing program, in past 30 days"
label(data$grouphome30) <- "Stayed in a grouphome, in past 30 days"
label(data$outdoor30) <- "Stayed in an outdoor location, in past 30 days"
label(data$car30) <- "Stayed in a car, truck, van, RV or Camper, in past 30 days"
label(data$bldg30) <- "Stayed in a closed area or space not meant for human habitation, in past 30 days"

##.do file (lines 309-312)
   # Generate summation dummy variable
data <- data %>% mutate(homeless30 = select(., shelter30:bldg30) %>% rowSums(na.rm = TRUE))
data$homeless30 <- ifelse(data$homeless30 > 1, 1, data$homeless30)
label(data$homeless30) <- "Experienced homelessness in past 30 days"

## .do file (lines 313-337)
data$shelter12 <- data$H14_5
data$shelter12 <- ifelse(data$H14_5 == 1, 0, data$shelter12)
data$shelter12 <- ifelse(data$H14_5 == 3, 0, data$shelter12)
data$shelter12 <- ifelse(data$H14_5 == 2, 1, data$shelter12)

data$couchsurf12 <- data$H14_6
data$couchsurf12 <- ifelse(data$H14_6 == 1, 0, data$couchsurf12)
data$couchsurf12 <- ifelse(data$H14_6 == 3, 0, data$couchsurf12)
data$couchsurf12 <- ifelse(data$H14_6 == 2, 1, data$couchsurf12)

data$hotmot12 <- data$H14_7
data$hotmot12 <- ifelse(data$H14_7 == 1, 0, data$hotmot12)
data$hotmot12 <- ifelse(data$H14_7 == 3, 0, data$hotmot12)
data$hotmot12 <- ifelse(data$H14_7 == 2, 1, data$hotmot12)

data$transithousing12 <- data$H14_8
data$transithousing12 <- ifelse(data$H14_8 == 1, 0, data$transithousing12 )
data$transithousing12 <- ifelse(data$H14_8 == 3, 0, data$transithousing12 )
data$transithousing12 <- ifelse(data$H14_8 == 2, 1, data$transithousing12 )

data$grouphome12 <- data$H14_9
data$grouphome12 <- ifelse(data$H14_9 == 1, 0, data$grouphome12 )
data$grouphome12 <- ifelse(data$H14_9 == 3, 0, data$grouphome12 )
data$grouphome12 <- ifelse(data$H14_9 == 2, 1, data$grouphome12 )

data$outdoor12 <- data$H14_10
data$outdoor12 <- ifelse(data$H14_10 == 1, 0, data$outdoor12 )
data$outdoor12 <- ifelse(data$H14_10 == 3, 0, data$outdoor12 )
data$outdoor12 <- ifelse(data$H14_10 == 2, 1, data$outdoor12 )

data$car12 <- data$H14_11
data$car12 <- ifelse(data$H14_11 == 1, 0, data$car12)
data$car12 <- ifelse(data$H14_11 == 3, 0, data$car12)
data$car12 <- ifelse(data$H14_11 == 2, 1, data$car12)

data$bldg12 <- data$H14_12
data$bldg12 <- ifelse(data$H14_12 == 1, 0, data$bldg12)
data$bldg12 <- ifelse(data$H14_12 == 3, 0, data$bldg12)
data$bldg12 <- ifelse(data$H14_12 == 2, 1, data$bldg12)

label(data$shelter12) <- "in a shelter the last 30 days? (no/yes = 0/1)"
label(data$couchsurf12) <- "Temporarily stayed with friend of family member until R. could find other hsg - past 30 days)"
label(data$hotmot12) <- "Stayed in a hotel or motel without having a permanent home, in past 30 days"
label(data$transithousing12) <- "Stayed in a transitional housing program, in past 30 days"
label(data$grouphome12) <- "Stayed in a grouphome, in past 30 days"
label(data$outdoor12) <- "Stayed in an outdoor location, in past 30 days"
label(data$car12) <- "Stayed in a car, truck, van, RV or Camper, in past 30 days"
label(data$bldg12) <- "Stayed in a closed area or space not meant for human habitation, in past 30 days"

## .do file (lines 339-342)

data <- data %>% mutate(homeless12 = select(., shelter12:bldg12) %>% rowSums(na.rm = TRUE))
data$homeless12 <- ifelse(data$homeless12 > 1, 1, data$homeless12)
label(data$homeless12) <- "Experienced homelessness in past 12 months"

## .do file (lines 343-368)
## Renaming several binary variables
data$shelterall <- ifelse(data$H14_5 < 3, 1, data$H14_5)
data$shelterall <- ifelse(data$H14_5 == 3, 0, data$shelterall)

data$couchsurfall <- ifelse(data$H14_6 < 3, 1, data$H14_6)
data$couchsurfall <- ifelse(data$H14_6 == 3, 0, data$couchsurfall)

data$hotmotall <- ifelse(data$H14_7 < 3, 1, data$H14_7)
data$hotmotall <- ifelse(data$H14_7 == 3, 0, data$hotmotall)

data$transithousingall <- ifelse(data$H14_8 < 3, 1, data$H14_8)
data$transithousingall <- ifelse(data$H14_8 == 3, 0, data$transithousingall)

data$grouphomeall <- ifelse(data$H14_9 < 3, 1, data$H14_9)
data$grouphomeall <- ifelse(data$H14_9 == 3, 0, data$grouphomeall)

data$outdoorall <- ifelse(data$H14_10 < 3, 1, data$H14_10)
data$outdoorall <- ifelse(data$H14_10 == 3, 0, data$outdoorall)

data$carall <- ifelse(data$H14_11 < 3, 1, data$H14_11)
data$carall <- ifelse(data$H14_11 == 3, 0, data$carall)

data$bldgall <- ifelse(data$H14_12 < 3, 1, data$H14_12)
data$bldgall <- ifelse(data$H14_12 == 3, 0, data$bldgall)

label(data$shelterall) <- "in a shelter the last 30 days or 12 months? (no/yes = 0/1)"
label(data$couchsurfall) <- "Temporarily stayed with friend of family member until R. could find other hsg - past 30 days or 12 months"
label(data$hotmotall) <- "Stayed in a hotel or motel without having a permanent home, in past 30 days or 12 months"
label(data$transithousingall) <- "Stayed in a transitional housing program, in past 30 days or 12 months"
label(data$grouphomeall) <- "Stayed in a grouphome, in past 30 days or 12 months"
label(data$outdoorall) <- "Stayed in an outdoor location, in past 30 days or 12 months"
label(data$carall) <- "Stayed in a car, truck, van, RV or Camper, in past 30 days or 12 months"
label(data$bldgall) <- "Stayed in a closed area or space not meant for human habitation, in past 30 days or 12 months"

## .do file (lines 370-384)
data$nonuseprograms <- NA
data$nonuseprograms[data$S3_1 == 1] <- 1
data$nonuseprograms[data$S3_2 == 1] <- 2
data$nonuseprograms[data$S3_3 == 1] <- 3
data$nonuseprograms[data$S3_4 == 1] <- 4
data$nonuseprograms[data$S3_5 == 1] <- 5
data$nonuseprograms[data$S3_6 == 1] <- 6
data$nonuseprograms[data$S3_7 == 1] <- 7
data$nonuseprograms[data$S3_8 == 1] <- 8
data$nonuseprograms[data$S3_9 == 1] <- 9
data$nonuseprograms <- factor(data$nonuseprograms, labels = c("already use programs","do not need assistance", "do not know how to access", "not eligible", "others have greater need", "do not believe in social services", "uncomfortable disclosing my need", "lack transportation to access the program",  "other"))
label(data$nonuseprograms) <- "reasons students do not use existing services"

## .do file (lines 391-395)
data <- data %>% mutate(homelessall = select(., shelterall:bldgall) %>% rowSums(na.rm = TRUE))
data$homelessall <- ifelse(data$homelessall > 1, 1, data$homelessall)
label(data$homelessall) <- "Experienced homelessness in past 30 days or 12 months"

## .do file (lines 396-400)
colnames(data)[colnames(data)=="H15"] <- "HIreasons"
label(data$HIreasons) <- "Reasons given for housing insecurity"
data$interpersonal <- ifelse(data$HIreasons == 4, 1, 0)

## .do file (lines 403-413)
data$wildcatfoodpantry <- NA
data$wildcatfoodpantry[data$S2_1_1 == 1] <- 1
data$wildcatfoodpantry[data$S2_1_2 == 1] <- 2
data$wildcatfoodpantry[data$S2_1_3 == 1] <- 3
data$wildcatfoodpantry[data$S2_1_4 == 1] <- 4
data$wildcatfoodpantrydummy <- ifelse(data$wildcatfoodpantry > 1, 1, 0)
data$wildcatfoodpantry <- factor(data$wildcatfoodpantry, labels = c("Unaware of resource", "Aware, but did not use", "Have used in the past", "Currently use"))
label(data$wildcatfoodpantry) <- "Awarness and use of wildcatfoodpantry"

## .do file (lines 415-426)
data$basicneedsproj <- NA
data$basicneedsproj[data$S2_2_1 == 1] <- 1
data$basicneedsproj[data$S2_2_2 == 1] <- 2
data$basicneedsproj[data$S2_2_3 == 1] <- 3
data$basicneedsproj[data$S2_2_4 == 1] <- 4
data$basicneedsprojduumy <- ifelse(data$basicneedsproj > 1, 1, 0) 
data$basicneedsproj <- factor(data$basicneedsproj, labels = c("Unaware of resource", "Aware, but did not use", "Have used in the past", "Currently use"))
label(data$basicneedsproj) <- "Awarness and use of Basic Needs Project"


## .do file (lines 428-439)
data$offcampusss <- NA
data$offcampusss[data$S2_3_1 == 1] <- 1
data$offcampusss[data$S2_3_2 == 1] <- 2
data$offcampusss[data$S2_3_3 == 1] <- 3
data$offcampusss[data$S2_3_4 == 1] <- 4
data$offcampusssdummy <- ifelse(data$offcampusss > 1, 1, 0)
data$offcampusss <- factor(data$offcampusss, labels = c("Unaware of resource", "Aware, but did not use", "Have used in the past", "Currently use"))
label(data$offcampusss) <- "Awarness and use of Off Campus Student Services"


## .do files (lines 441-449)
data$studentemergencygrant <- NA
data$studentemergencygrant[data$S2_4_1 == 1] <- 1
data$studentemergencygrant[data$S2_4_2 == 1] <- 2
data$studentemergencygrant[data$S2_4_3 == 1] <- 3
data$studentemergencygrant[data$S2_4_4 == 1] <- 4
data$studentemergencygrantdummy <- ifelse(data$studentemergencygrant > 1, 1, 0)
data$studentemergencygrant <- factor(data$studentemergencygrant, labels = c("Unaware of resource", "Aware, but did not use", "Have used in the past", "Currently use"))
label(data$studentemergencygrant) <- "Awarness and use of student emergency grant"

## .do file (lines 454-465)
data$emerghousing <- NA
data$emerghousing[data$S2_5_1 == 1] <- 1
data$emerghousing[data$S2_5_2 == 1] <- 2
data$emerghousing[data$S2_5_3 == 1] <- 3
data$emerghousing[data$S2_5_4 == 1] <- 4
data$emerghousingdummy <- ifelse(data$emerghousing > 1, 1, 0)
data$emerghousing <- factor(data$emerghousing, labels = c("Unaware of resource", "Aware, but did not use", "Have used in the past", "Currently use"))
label(data$emerghousing) <- "Awarness and use of emergency housing"

## .do file (lines 467-478) Campus Resource: Financial Aid Advisor
data$FAadvisor <-NA
data$FAadvisor[data$S2_6_1 == 1] <- 1
data$FAadvisor[data$S2_6_2 == 1] <- 2
data$FAadvisor[data$S2_6_3 == 1] <- 3
data$FAadvisor[data$S2_6_4 == 1] <- 4
data$FAadvisordummy <- ifelse(data$FAadvisor > 1, 1, 0)
data$FAadvisor <- factor(data$FAadvisor, labels = c("Unaware of resource", "Aware, but did not use", "Have used in the past", "Currently use"))
label(data$FAadvisor) <- "Awarness and use of financial aid advising"

## .do file (line 509)
data$creditcarddebt <- data$S5

## .do file (lines 523-528)
data$campfireimpact <- NA
data$campfireimpact[data$CF3_4 == 1] <- 1
data$campfireimpact[data$CF3_3 == 1] <- 2
data$campfireimpact[data$CF3_2 == 1] <- 3
data$campfireimpact[data$CF3_1 == 1] <- 4
data$campfireimpact[data$CF3_5 == 1] <- 5
data$campfireimpact <- factor(data$campfireimpact, labels = c("None", "Housing Exp. Increased", "Temp. Moved", "Permanently Moved", "Other"))
label(data$campfireimpact) <- "Impact of Camp Fire on Housing"

###-------- End Constantin Section ---------###

###------------ Lauras section -------------###

## .do file (lines 530-538): Housing Situation
#change missing data to NA
data$CF3_1[is.na(data$CF3_1)] <- 0
data$CF3_2[is.na(data$CF3_2)] <- 0
data$CF3_3[is.na(data$CF3_3)] <- 0
data$CF3_4[is.na(data$CF3_4)] <- 0
data$CF3_5[is.na(data$CF3_5)] <- 0

#label columns so they are easier to understand
label(data$CF3_1) <- "No Impact (0/1 = no/yes)"
label(data$CF3_2) <- "Housing Expenses Increased (0/1 = no/yes)"
label(data$CF3_3) <- "Temporarily Moved (0/1 = no/yes)"
label(data$CF3_4) <- "Permanently Moved (0/1 = no/yes)"
label(data$CF3_5) <- "Other"

## .do file (lines 540-545)
#Create new variable that represents if individual housing was impacted by CF
data$CFimpact <- 0
data$CFimpact[data$CF3_1 == 1] <- 1
data$CFimpact[data$CF3_2 == 1] <- 1
data$CFimpact[data$CF3_3 == 1] <- 1
data$CFimpact[data$CF3_5 == 1] <- 1
data$CFimpact[data$CF3_4 == 1] <- 0
label(data$CFimpact) <- "Impact of Camp Fire on Housing"

## .do file (lines 551-552): Reasons for Moving
#Recode CF4 
data$CF4[data$CF4 == 6] <- NA
label(data$CF4) <- "Reasons for moving"

## .do file (lines 554-577): Finding Housing
#change missing data to NA
data$CF7[data$CF7 == 6] <- NA
data$CF8[data$CF8 == 6] <- NA
data$CF9[data$CF9 == 6] <- NA
data$CF10[data$CF10 == 6] <- NA

#create new variables
data$findhousing <- data$CF7
data$findaffhousing <- data$CF8
data$housingstress <- data$CF9
data$leaveChicohsg <- data$CF10

#factor variables
data$findhousing <- factor(data$findhousing, labels = c("Strongly Agree", "Agree", "Neither Agree/Disagree", "Disagree", "Strongly Disagree"))
data$findaffhousing <- factor(data$findaffhousing, labels = c("Strongly Agree", "Agree", "Neither Agree/Disagree", "Disagree", "Strongly Disagree"))
data$housingstress <- factor(data$housingstress, labels = c("Strongly Agree", "Agree", "Neither Agree/Disagree", "Disagree", "Strongly Disagree"))
data$leaveChicohsg <- factor(data$leaveChicohsg, labels = c("Strongly Agree", "Agree", "Neither Agree/Disagree", "Disagree", "Strongly Disagree"))
#label variables
label(data$findhousing) <- "I easily found housing for the current academic year"
label(data$findaffhousing) <- "I easily found housing I can afford for the current academic year"
label(data$housingstress) <- "Finding housing has been a major source of stress in attending Chico State"
label(data$leaveChicohsg) <- "I have considered leaving Chico State due to difficulties in finding affordable housing"

## .do file (lines 578-589)
#create dummy variables
data$findhousingdummy <- as.numeric(data$findhousing)
data$findhousingdummy <- ifelse(data$findhousingdummy > 3, 1, 0)
data$findaffhousingdummy <- as.numeric(data$findaffhousing)
data$findaffhousingdummy <- ifelse(data$findaffhousingdummy > 3, 1, 0)
data$housingstressdummy <- as.numeric(data$housingstress)
data$housingstressdummy <- ifelse(data$housingstressdummy < 3, 1, 0)
data$leaveChicohsgdummy <- as.numeric(data$leaveChicohsg)
data$leaveChicohsgdummy <- ifelse(data$leaveChicohsgdummy < 3, 1, 0)

#create variable negperchousing to show negative housing experiences (higher number is more)
data <- data %>% mutate(negperchousing = select(., findhousingdummy:leaveChicohsgdummy) %>% rowSums(na.rm = TRUE))

## .do file (lines 590-594): General Health
#Create variable `genhealth` and reorder levels from decreasing to increasing
data$genhealth <- data$W2
data$genhealth[data$W2 == 5] <- 1
data$genhealth[data$W2 == 4] <- 2
data$genhealth[data$W2 == 2] <- 4
data$genhealth[data$W2 == 1] <- 5

#Factor variable `genhealth`
data$genhealth <- factor(data$genhealth, labels = c("poor", "fair", "good", "very good", "excellent"))

## .do file (lines 596-600): Physical Health Days
#Create variable `physicalhealthdays` and reorder levels from decreasing to increasing
data$physicalhealthdays <- data$W3 - 1

#Factor variable `physicalhealthdays`
data$physicalhealthdays <- factor(data$physicalhealthdays, labels = c("0 days", "1-2 days", "3-4 days", "5 or more days"))

## .do file (lines 602-606): Mental Health Days
#Create variable `mentalhealthdays` and reorder levels from decreasing to increasing
data$mentalhealthdays <- data$W4 - 1
#Factor variable `mentalhealthdays`
data$mentalhealthdays <- factor(data$mentalhealthdays, labels = c("0 days", "1-2 days", "3-4 days", "5 or more days"))

## .do file (lines 608-612): Poor Health Days
#Create variable `poorhealthdays` and reorder levels from decreasing to increasing
data$poorhealthdays <- data$W5 - 1
#Factor variable `poorhealthdays`
data$poorhealthdays <- factor(data$poorhealthdays, labels = c("0 days", "1-2 days", "3-4 days", "5 or more days"))

## .do file (lines 614-626)
#Create new variable `famincome `
data$famincome <- data$DependentIncome

#Recode levels to numeric and convert missing values to NA
data$famincome[data$DependentIncome == "24,000 or less"] <- 1
data$famincome[data$DependentIncome == "24,000 - 35,999"] <- 2
data$famincome[data$DependentIncome == "36,000 - 47,999"] <- 3
data$famincome[data$DependentIncome == "48,000 - 59,999"] <- 4
data$famincome[data$DependentIncome == "60,000 - 71,999"] <- 5
data$famincome[data$DependentIncome == "72,000 or more"] <- 6
data$famincome <- ifelse(data$famincome == "No response", NA, data$famincome)
data$famincome <- ifelse(data$famincome == "Other", NA, data$famincome)
data$famincome <- ifelse(data$famincome == "", NA, data$famincome)

#recode from numeric back to ranges (lines 624-626)
#data$famincome <- factor(data$famincome, labels = c("24,000 or less", "24,000-35,999", "36,000 - 47,999", "48,000 - 59,999", "60,000 - 71,999", "72,000 or more"))

#create variable `income` and collapse IndepIncome and matching famincome levels
data$incomecateg <- data$famincome
data$incomecateg[data$IndepIncome == "6,000 or less"] <- 1
data$incomecateg[data$IndepIncome == "6,000 - 11,999"] <- 1
data$incomecateg[data$IndepIncome == "12,000 - 23,999"] <- 1
data$incomecateg[data$IndepIncome == "24,000 - 35,000"] <- 2
data$incomecateg[data$IndepIncome == "36,000 - 47,999"] <- 3
data$incomecateg[data$IndepIncome == "48,000 - 59,999"] <- 4
data$incomecateg[data$IndepIncome == "60,000 or more"] <- 5

## .do file (lines 638-644): Class Standing: no action required due to lack of meaningful changes in .do file; note for analyses that AcademicLvl = classstanding.

## .do file (lines 646-649): AdmitType/First Time Students
#Create binary indicator variable firsttimestudent where firsttimestudent=1 and all others (including transfer) = 0.
data$firsttimestudent <- data$AdmitType
data$firsttimestudent <- ifelse(data$AdmitType == "First Time", 1, 0)

## .do file (lines 651-653): Independent Dummy Variable: no action required due to lack of use 

## .do file (lines 655-665): Income Intervals
#create variable `incomeinterval`
data$incomeinterval <- data$IndepIncome
data$incomeinterval <- data$DependentIncome

#Recode `DependentIncome` and `IndepIncome` levels into appropriate `incomeinterval` levels 
data$incomeinterval[data$IndepIncome == "6,000 or less"] <- 3000
data$incomeinterval[data$IndepIncome == "6,000 - 11,999"] <- 6000
data$incomeinterval[data$IndepIncome == "12,000 - 23,999"] <- 18000
data$incomeinterval[data$DependentIncome == "24,000 or less"] <- 18000
data$incomeinterval[data$IndepIncome == "24,000 - 35,999" | data$DependentIncome=="24,000 - 35,999"] <- 30000
data$incomeinterval[data$IndepIncome == "36,000 - 47,999" | data$DependentIncome=="36,000 - 47,999"] <- 42000
data$incomeinterval[data$IndepIncome == "48,000 - 59,999" | data$DependentIncome=="48,000 - 59,999"] <- 54000
data$incomeinterval[data$IndepIncome== "60,000 or more"] <- 66000
data$incomeinterval[data$DependentIncome == "60,000 - 71,999"] <- 66000
data$incomeinterval[data$DependentIncome== "72,000 or more"] <- 78000
data$incomeinterval <- ifelse(data$incomeinterval == "No response", NA, data$incomeinterval)
data$incomeinterval <- ifelse(data$incomeinterval == "Other", NA, data$incomeinterval)

## .do file (lines 667-691): Postal Codes
zipCodes <- c("95914","95916","95917","95926","95927","95928","95930","95938","95940","95941","95942","95948","95954","95958","95965","95966","95967","95968","95969","95973","95974","95978")
data$homepostalcode <- substr(data$ResPostalCode, 1, 5)
data$buttecountyres <- ifelse(data$homepostalcode %in% zipCodes, 1, 0)
rm(zipCodes)

## .do file (lines 693-782): No action required; coded out by original researcher

## .do file (lines 784-785): GPA
#create variable `ChicoGPA` and recode zeros to missing
data$ChicoGPA <- data$ChicoCumltvGPA
data$ChicoGPA[is.na(data$ChicoGPA)] <- 0

